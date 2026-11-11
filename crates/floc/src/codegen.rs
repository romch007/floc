use std::{collections::HashMap, path::Path};

use inkwell::{
    AddressSpace, IntPredicate,
    builder::Builder,
    context::Context,
    intrinsics::Intrinsic,
    module::{Linkage, Module},
    passes::PassBuilderOptions,
    targets::{CodeModel, FileType, RelocMode, Target, TargetMachine, TargetTriple},
    types::{BasicMetadataTypeEnum, IntType},
    values::{BasicMetadataValueEnum, FunctionValue, GlobalValue, IntValue, PointerValue},
};

use crate::{analyzer, ast, cli};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Builder error: {0}")]
    Builer(#[from] inkwell::builder::BuilderError),

    #[error("Inkwell error: {0}")]
    Inkwell(#[from] inkwell::Error),

    #[error("Verification error: {0}")]
    Verification(String),

    #[error("LLVM Error: {0}")]
    Other(String),
}

#[derive(Debug)]
pub struct Compiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    variables: HashMap<String, Variable<'ctx>>,
    functions: HashMap<String, Function<'ctx>>,

    current_function: Option<FunctionValue<'ctx>>,

    printf: (FunctionValue<'ctx>, GlobalValue<'ctx>),
    scanf: (FunctionValue<'ctx>, GlobalValue<'ctx>),
    read_fn: FunctionValue<'ctx>,
    fflush: FunctionValue<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    #[must_use]
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let printf = Compiler::create_printf(context, &module);
        let scanf = Compiler::create_scanf(context, &module);
        let fflush = module.add_function(
            "fflush",
            context
                .i32_type()
                .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false),
            None,
        );
        let read_fn = module.add_function(
            "floc_read",
            context.i64_type().fn_type(&[], false),
            Some(Linkage::Internal),
        );

        Self {
            context,
            module,
            builder,
            printf,
            scanf,
            read_fn,
            fflush,
            current_function: None,
            variables: HashMap::default(),
            functions: HashMap::default(),
        }
    }

    pub fn dump_to_stderr(&self) {
        self.module.print_to_stderr();
    }

    pub fn verify(&self) -> Result<(), Error> {
        self.module
            .verify()
            .map_err(|llvm_str| Error::Verification(llvm_str.to_string()))
    }

    pub fn create_target_machine(
        target_triple: Option<&str>,
        target_cpu: Option<&str>,
        target_features: Option<&str>,
        optimization_level: inkwell::OptimizationLevel,
    ) -> Result<TargetMachine, Error> {
        let target_triple =
            target_triple.map_or_else(TargetMachine::get_default_triple, TargetTriple::create);

        let target =
            Target::from_triple(&target_triple).map_err(|err| Error::Other(err.to_string()))?;

        let target_machine = target
            .create_target_machine(
                &target_triple,
                target_cpu.unwrap_or("generic"),
                target_features.unwrap_or(""),
                optimization_level,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .ok_or_else(|| {
                Error::Other(format!(
                    "cannot create target machine for {}",
                    target_triple.as_str().to_string_lossy()
                ))
            })?;

        Ok(target_machine)
    }

    pub fn optimize(
        &self,
        target_machine: &TargetMachine,
        optimization_level: cli::args::OptimizationLevel,
    ) -> Result<(), Error> {
        use cli::args::OptimizationLevel;

        let pipeline = match optimization_level {
            OptimizationLevel::None => "default<O0>",
            OptimizationLevel::Less => "default<O1>",
            OptimizationLevel::Default => "default<O2>",
            OptimizationLevel::Aggressive => "default<O3>",
        };

        self.module
            .run_passes(pipeline, target_machine, PassBuilderOptions::create())
            .map_err(|err| Error::Other(err.to_string()))?;

        Ok(())
    }

    pub fn compile(
        &self,
        target_machine: &TargetMachine,
        dest_path: &Path,
        file_type: FileType,
    ) -> Result<(), Error> {
        target_machine
            .write_to_file(&self.module, file_type, dest_path)
            .map_err(|err| Error::Other(err.to_string()))?;

        Ok(())
    }

    pub fn emit_program(&mut self, prog: &ast::Program) -> Result<(), Error> {
        let comment = format!("floc {}", cli::get_version());
        let md_string = self.context.metadata_string(&comment);
        let md_node = self.context.metadata_node(&[md_string.into()]);

        self.module.add_global_metadata("llvm.ident", &md_node)?;

        self.emit_read_function()?;

        for fn_decl in &prog.function_decls {
            self.emit_function_declaration(fn_decl)?;
        }

        let main_function_type = self.context.i32_type().fn_type(&[], false);
        let main_function = self.module.add_function("main", main_function_type, None);

        let bb = self.context.append_basic_block(main_function, "entry");
        self.builder.position_at_end(bb);

        self.current_function = Some(main_function);

        if !self.emit_block(&prog.statements)? {
            self.builder
                .build_return(Some(&self.context.i32_type().const_zero()))?;
        }

        Ok(())
    }

    fn create_printf(
        context: &'ctx Context,
        module: &Module<'ctx>,
    ) -> (FunctionValue<'ctx>, GlobalValue<'ctx>) {
        let printf_format = "%lld\n";
        let printf_format_type = context
            .i8_type()
            .array_type((printf_format.len() + 1) as u32);
        let printf_format_global = module.add_global(printf_format_type, None, "write_format");

        printf_format_global.set_initializer(&context.const_string(printf_format.as_bytes(), true));

        let printf_args = [context.ptr_type(AddressSpace::default()).into()];

        let printf_type = context.i32_type().fn_type(&printf_args, true);
        let printf_fn = module.add_function("printf", printf_type, None);

        (printf_fn, printf_format_global)
    }

    fn create_scanf(
        context: &'ctx Context,
        module: &Module<'ctx>,
    ) -> (FunctionValue<'ctx>, GlobalValue<'ctx>) {
        let scanf_format = "%lld";
        let scanf_format_type = context
            .i8_type()
            .array_type((scanf_format.len() + 1) as u32);
        let scanf_format_global = module.add_global(scanf_format_type, None, "read_format");

        scanf_format_global.set_initializer(&context.const_string(scanf_format.as_bytes(), true));

        let scanf_args = [context.ptr_type(AddressSpace::default()).into()];

        let scanf_type = context.i32_type().fn_type(&scanf_args, true);
        let scanf_fn = module.add_function("scanf", scanf_type, None);

        (scanf_fn, scanf_format_global)
    }

    /// Body of `floc_read`: returns 0 when `scanf` fails (EOF, non-numeric input).
    fn emit_read_function(&mut self) -> Result<(), Error> {
        let i64_type = self.context.i64_type();

        let entry = self.context.append_basic_block(self.read_fn, "entry");
        self.builder.position_at_end(entry);

        let result = self.builder.build_alloca(i64_type, "read_result")?;
        self.builder.build_store(result, i64_type.const_zero())?;

        let args: &[BasicMetadataValueEnum<'ctx>] =
            &[self.scanf.1.as_pointer_value().into(), result.into()];
        self.builder.build_call(self.scanf.0, args, "read_call")?;

        let value = self.builder.build_load(i64_type, result, "read_value")?;
        self.builder.build_return(Some(&value))?;

        Ok(())
    }

    /// Allocas outside the entry block grow the stack on every loop iteration
    /// and are not promoted to registers.
    fn build_entry_alloca(
        &self,
        ty: IntType<'ctx>,
        name: &str,
    ) -> Result<PointerValue<'ctx>, Error> {
        let entry = self
            .current_function
            .and_then(FunctionValue::get_first_basic_block)
            .unwrap();

        let builder = self.context.create_builder();
        match entry.get_first_instruction() {
            Some(first) => builder.position_before(&first),
            None => builder.position_at_end(entry),
        }

        Ok(builder.build_alloca(ty, name)?)
    }

    pub fn declare_functions(&mut self, functions: &[&analyzer::Function]) {
        for function in functions {
            let function_params = function
                .arguments
                .iter()
                .map(|arg| arg.1.to_llvm(self.context).into())
                .collect::<Vec<BasicMetadataTypeEnum<'ctx>>>();

            let function_type = function
                .return_type
                .to_llvm(self.context)
                .fn_type(&function_params, false);

            let llvm_function =
                self.module
                    .add_function(&function.name, function_type, Some(Linkage::Internal));

            self.functions
                .insert(function.name.clone(), Function { ptr: llvm_function });
        }
    }

    pub fn emit_function_declaration(
        &mut self,
        fn_decl: &ast::FunctionDeclaration,
    ) -> Result<(), Error> {
        let function = self.functions.get(&fn_decl.name.ident).unwrap().ptr;

        let entry_bb = self.context.append_basic_block(function, "entry");

        self.current_function = Some(function);

        self.builder.position_at_end(entry_bb);

        for (idx, arg) in fn_decl.arguments.iter().enumerate() {
            let value = function.get_nth_param(idx as u32).unwrap();

            let alloca_ptr =
                self.build_entry_alloca(arg.r#type.to_llvm(self.context), &arg.name)?;

            self.builder.build_store(alloca_ptr, value)?;
            self.variables.insert(
                arg.name.ident.clone(),
                Variable {
                    ptr: alloca_ptr,
                    r#type: arg.r#type.clone(),
                },
            );
        }

        self.emit_block(&fn_decl.statements)?;

        Ok(())
    }

    /// Returns whether the block never falls through. Statements after one
    /// that never falls through are unreachable and not emitted.
    pub fn emit_block(&mut self, block: &[ast::Statement]) -> Result<bool, Error> {
        for stmt in block {
            if self.emit_statement(stmt)? {
                return Ok(true);
            }
        }

        Ok(false)
    }

    pub fn emit_statement(&mut self, stmt: &ast::Statement) -> Result<bool, Error> {
        match stmt {
            ast::Statement::Declaration(decl) => self.emit_declaration(decl),
            ast::Statement::Return(ret) => self.emit_return(ret),
            ast::Statement::Assignment(assign) => self.emit_assignment(assign),
            ast::Statement::While(whil) => self.emit_while(whil),
            ast::Statement::If(i) => self.emit_if(i),
            ast::Statement::Write(write) => self.emit_write(write),
            ast::Statement::DiscardFunctionCall(fn_call) => {
                let _ = self.emit_function_call(fn_call)?;

                Ok(false)
            }
            ast::Statement::Error(_) => unreachable!("error node reached codegen"),
        }
    }

    pub fn emit_declaration(&mut self, decl: &ast::Declaration) -> Result<bool, Error> {
        let int_type = decl.r#type.to_llvm(self.context);

        let alloca_ptr = self.build_entry_alloca(int_type, &decl.variable)?;

        self.variables.insert(
            decl.variable.ident.clone(),
            Variable {
                ptr: alloca_ptr,
                r#type: decl.r#type.clone(),
            },
        );

        // Zero-initialize so a declaration in a loop doesn't see the previous
        // iteration's value.
        let init_val = match &decl.value {
            Some(default_value) => self.emit_expression(default_value)?,
            None => int_type.const_zero(),
        };

        self.builder.build_store(alloca_ptr, init_val)?;

        Ok(false)
    }

    pub fn emit_assignment(&mut self, assign: &ast::Assignment) -> Result<bool, Error> {
        let variable = self.variables.get(&assign.variable.ident).unwrap().clone();

        let val = self.emit_expression(&assign.value)?;

        self.builder.build_store(variable.ptr, val)?;

        Ok(false)
    }

    pub fn emit_return(&mut self, ret: &ast::Return) -> Result<bool, Error> {
        let retval = self.emit_expression(&ret.value)?;
        self.builder.build_return(Some(&retval))?;

        Ok(true)
    }

    pub fn emit_while(&mut self, whil: &ast::While) -> Result<bool, Error> {
        let body_bb = self
            .context
            .append_basic_block(self.current_function.unwrap(), "while.body");

        let condition_bb = self
            .context
            .append_basic_block(self.current_function.unwrap(), "while.condition");

        let end_bb = self
            .context
            .append_basic_block(self.current_function.unwrap(), "while.end");

        // Jump to condition
        self.builder.build_unconditional_branch(condition_bb)?;

        self.builder.position_at_end(body_bb);

        // Emit while body
        let has_ended_bb = self.emit_block(&whil.statements)?;

        if !has_ended_bb {
            self.builder.build_unconditional_branch(condition_bb)?;
        }

        self.builder.position_at_end(condition_bb);

        // Evaluate condition
        let condition = self.emit_expression(&whil.condition)?;

        self.builder
            .build_conditional_branch(condition, body_bb, end_bb)?;

        self.builder.position_at_end(end_bb);

        if whil.condition.const_bool() == Some(true) {
            self.builder.build_unreachable()?;
            return Ok(true);
        }

        Ok(false)
    }

    pub fn emit_if(&mut self, i: &ast::If) -> Result<bool, Error> {
        let then_bb = self
            .context
            .append_basic_block(self.current_function.unwrap(), "if.then");

        let condition = self.emit_expression(&i.condition)?;

        if let Some(statements_else) = &i.statements_else {
            let else_bb = self
                .context
                .append_basic_block(self.current_function.unwrap(), "if.else");

            self.builder
                .build_conditional_branch(condition, then_bb, else_bb)?;

            let end_bb = self
                .context
                .append_basic_block(self.current_function.unwrap(), "if.end");

            self.builder.position_at_end(then_bb);
            let has_ended_bb_then = self.emit_block(&i.statements)?;

            // The 'then' body didn't return, so jump to end
            if !has_ended_bb_then {
                self.builder.build_unconditional_branch(end_bb)?;
            }

            self.builder.position_at_end(else_bb);
            let has_ended_bb_else = self.emit_block(statements_else)?;

            // The 'else' body didn't return, so jump to end
            if !has_ended_bb_else {
                self.builder.build_unconditional_branch(end_bb)?;
            }

            if !has_ended_bb_else || !has_ended_bb_then {
                self.builder.position_at_end(end_bb);

                Ok(false)
            } else {
                // Every code path returned, so we don't need the end basic block.
                // SAFETY: end_bb is unused (no branches/references to it) and not
                // referenced anywhere else, so deleting it is sound.
                unsafe { end_bb.delete().unwrap() };

                Ok(true)
            }
        } else {
            let end_bb = self
                .context
                .append_basic_block(self.current_function.unwrap(), "if.end");
            self.builder
                .build_conditional_branch(condition, then_bb, end_bb)?;

            self.builder.position_at_end(then_bb);

            let has_ended_bb = self.emit_block(&i.statements)?;

            // The 'then' body didn't return, so jump to end
            if !has_ended_bb {
                self.builder.build_unconditional_branch(end_bb)?;
            }

            self.builder.position_at_end(end_bb);

            Ok(false)
        }
    }

    pub fn emit_write(&mut self, write: &ast::Write) -> Result<bool, Error> {
        let value = self.emit_expression(&write.value)?;

        // `%lld` reads a full i64; booleans are i1.
        let i64_type = self.context.i64_type();
        let value = if value.get_type() == i64_type {
            value
        } else {
            self.builder
                .build_int_z_extend(value, i64_type, "write_zext")?
        };

        let args: &[BasicMetadataValueEnum<'ctx>] =
            &[self.printf.1.as_pointer_value().into(), value.into()];

        self.builder.build_call(self.printf.0, args, "write_call")?;

        Ok(false)
    }

    pub fn emit_expression(&mut self, expr: &ast::Expression) -> Result<IntValue<'ctx>, Error> {
        Ok(match expr {
            ast::Expression::Integer(value, _) => self.emit_integer(*value),
            ast::Expression::Boolean(value, _) => self.emit_boolean(*value),
            ast::Expression::Variable(var) => self.emit_variable(var)?,
            ast::Expression::FunctionCall(fn_call) => self.emit_function_call(fn_call)?,
            ast::Expression::Read(_) => self.emit_read()?,
            ast::Expression::BinaryOp(binary_op) => self.emit_binary_op(binary_op)?,
            ast::Expression::UnaryOp(unary_op) => self.emit_unary_op(unary_op)?,
            ast::Expression::Error(_) => unreachable!("error node reached codegen"),
        })
    }

    pub fn emit_integer(&mut self, value: u64) -> IntValue<'ctx> {
        self.context.i64_type().const_int(value, false)
    }

    pub fn emit_boolean(&mut self, value: bool) -> IntValue<'ctx> {
        self.context.bool_type().const_int(value.into(), false)
    }

    pub fn emit_variable(&mut self, name: &str) -> Result<IntValue<'ctx>, Error> {
        let variable = self.variables.get(name).unwrap();

        let value =
            self.builder
                .build_load(variable.r#type.to_llvm(self.context), variable.ptr, name)?;

        Ok(value.into_int_value())
    }

    pub fn emit_function_call(
        &mut self,
        fn_call: &ast::FunctionCall,
    ) -> Result<IntValue<'ctx>, Error> {
        let function = self.functions.get(&fn_call.name.ident).cloned().unwrap();

        let exprs: Vec<BasicMetadataValueEnum<'ctx>> = fn_call
            .arguments
            .iter()
            .map(|arg| self.emit_expression(arg).map(Into::into))
            .collect::<Result<_, _>>()?;

        let retval = self.builder.build_call(
            function.ptr,
            &exprs,
            &format!("{}_call", fn_call.name.ident),
        )?;

        Ok(retval.try_as_basic_value().unwrap_basic().into_int_value())
    }

    pub fn emit_read(&mut self) -> Result<IntValue<'ctx>, Error> {
        let value = self.builder.build_call(self.read_fn, &[], "read_call")?;

        Ok(value.try_as_basic_value().unwrap_basic().into_int_value())
    }

    pub fn emit_binary_op(&mut self, binary_op: &ast::BinaryOp) -> Result<IntValue<'ctx>, Error> {
        if matches!(
            binary_op.kind,
            ast::BinaryOpKind::LogicAnd | ast::BinaryOpKind::LogicOr
        ) {
            return self.emit_short_circuit(binary_op);
        }

        let left = self.emit_expression(&binary_op.left)?;
        let right = self.emit_expression(&binary_op.right)?;

        let result = match &binary_op.kind {
            ast::BinaryOpKind::Add => self.builder.build_int_add(left, right, "add")?,
            ast::BinaryOpKind::Sub => self.builder.build_int_sub(left, right, "sub")?,
            ast::BinaryOpKind::Mul => self.builder.build_int_mul(left, right, "mul")?,
            ast::BinaryOpKind::Div | ast::BinaryOpKind::Mod => {
                self.emit_division(&binary_op.kind, left, right)?
            }
            ast::BinaryOpKind::Eq => {
                self.builder
                    .build_int_compare(IntPredicate::EQ, left, right, "eq")?
            }
            ast::BinaryOpKind::Neq => {
                self.builder
                    .build_int_compare(IntPredicate::NE, left, right, "neq")?
            }
            ast::BinaryOpKind::Lt => {
                self.builder
                    .build_int_compare(IntPredicate::SLT, left, right, "lt")?
            }
            ast::BinaryOpKind::Lte => {
                self.builder
                    .build_int_compare(IntPredicate::SLE, left, right, "lte")?
            }
            ast::BinaryOpKind::Gt => {
                self.builder
                    .build_int_compare(IntPredicate::SGT, left, right, "gt")?
            }
            ast::BinaryOpKind::Gte => {
                self.builder
                    .build_int_compare(IntPredicate::SGE, left, right, "gte")?
            }
            ast::BinaryOpKind::LogicAnd | ast::BinaryOpKind::LogicOr => unreachable!(),
        };

        Ok(result)
    }

    fn emit_short_circuit(&mut self, binary_op: &ast::BinaryOp) -> Result<IntValue<'ctx>, Error> {
        let is_and = matches!(binary_op.kind, ast::BinaryOpKind::LogicAnd);
        let function = self.current_function.unwrap();
        let bool_type = self.context.bool_type();

        let left = self.emit_expression(&binary_op.left)?;
        let left_end = self.builder.get_insert_block().unwrap();

        let right_bb = self.context.append_basic_block(function, "logic.rhs");
        let end_bb = self.context.append_basic_block(function, "logic.end");

        if is_and {
            self.builder
                .build_conditional_branch(left, right_bb, end_bb)?;
        } else {
            self.builder
                .build_conditional_branch(left, end_bb, right_bb)?;
        }

        self.builder.position_at_end(right_bb);
        let right = self.emit_expression(&binary_op.right)?;
        let right_end = self.builder.get_insert_block().unwrap();
        self.builder.build_unconditional_branch(end_bb)?;

        self.builder.position_at_end(end_bb);
        let phi = self.builder.build_phi(bool_type, "logic")?;
        // Skipping the right side means the result is `left`: false for `et`, true for `ou`.
        let short_circuit_value = bool_type.const_int(u64::from(!is_and), false);
        phi.add_incoming(&[(&short_circuit_value, left_end), (&right, right_end)]);

        Ok(phi.as_basic_value().into_int_value())
    }

    /// Traps on a zero divisor; `i64::MIN / -1` wraps like other arithmetic.
    fn emit_division(
        &mut self,
        kind: &ast::BinaryOpKind,
        left: IntValue<'ctx>,
        right: IntValue<'ctx>,
    ) -> Result<IntValue<'ctx>, Error> {
        let function = self.current_function.unwrap();
        let i64_type = self.context.i64_type();

        let is_zero = self.builder.build_int_compare(
            IntPredicate::EQ,
            right,
            i64_type.const_zero(),
            "div.is_zero",
        )?;

        let trap_bb = self.context.append_basic_block(function, "div.zero");
        let ok_bb = self.context.append_basic_block(function, "div.ok");
        self.builder
            .build_conditional_branch(is_zero, trap_bb, ok_bb)?;

        self.builder.position_at_end(trap_bb);
        let trap = Intrinsic::find("llvm.trap")
            .and_then(|trap| trap.get_declaration(&self.module, &[]))
            .ok_or_else(|| Error::Other("cannot find llvm.trap".to_string()))?;
        // The trap would discard buffered output.
        let all_streams = self.context.ptr_type(AddressSpace::default()).const_null();
        self.builder
            .build_call(self.fflush, &[all_streams.into()], "")?;
        self.builder.build_call(trap, &[], "")?;
        self.builder.build_unreachable()?;

        self.builder.position_at_end(ok_bb);

        // Divide by 1 instead of -1, then negate: `left % 1` is already 0.
        let is_minus_one = self.builder.build_int_compare(
            IntPredicate::EQ,
            right,
            i64_type.const_all_ones(),
            "div.is_minus_one",
        )?;
        let divisor = self
            .builder
            .build_select(
                is_minus_one,
                i64_type.const_int(1, false),
                right,
                "div.divisor",
            )?
            .into_int_value();

        Ok(match kind {
            ast::BinaryOpKind::Div => {
                let quotient = self.builder.build_int_signed_div(left, divisor, "div")?;
                let negated = self.builder.build_int_neg(left, "div.neg")?;
                self.builder
                    .build_select(is_minus_one, negated, quotient, "div.result")?
                    .into_int_value()
            }
            ast::BinaryOpKind::Mod => self.builder.build_int_signed_rem(left, divisor, "mod")?,
            _ => unreachable!(),
        })
    }

    pub fn emit_unary_op(&mut self, unary_op: &ast::UnaryOp) -> Result<IntValue<'ctx>, Error> {
        let operand = self.emit_expression(&unary_op.operand)?;

        let result = match &unary_op.kind {
            ast::UnaryOpKind::Neg => self.builder.build_int_neg(operand, "neg")?,
            ast::UnaryOpKind::LogicNot => self.builder.build_not(operand, "not")?,
        };

        Ok(result)
    }
}

#[derive(Debug, Clone)]
struct Function<'ctx> {
    pub ptr: FunctionValue<'ctx>,
}

#[derive(Debug, Clone)]
pub struct Variable<'ctx> {
    ptr: PointerValue<'ctx>,
    r#type: ast::Type,
}

trait ToLlvmType {
    fn to_llvm<'ctx>(&self, context: &'ctx Context) -> IntType<'ctx>;
}

impl ToLlvmType for ast::Type {
    fn to_llvm<'ctx>(&self, context: &'ctx Context) -> IntType<'ctx> {
        match self.kind {
            ast::TypeKind::Integer => context.i64_type(),
            ast::TypeKind::Boolean => context.bool_type(),
        }
    }
}

impl From<cli::args::OptimizationLevel> for inkwell::OptimizationLevel {
    fn from(value: cli::args::OptimizationLevel) -> Self {
        use cli::args::OptimizationLevel;

        match value {
            OptimizationLevel::None => inkwell::OptimizationLevel::None,
            OptimizationLevel::Less => inkwell::OptimizationLevel::Less,
            OptimizationLevel::Default => inkwell::OptimizationLevel::Default,
            OptimizationLevel::Aggressive => inkwell::OptimizationLevel::Aggressive,
        }
    }
}
