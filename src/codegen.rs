use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    support::LLVMString,
    types::{BasicMetadataTypeEnum, BasicType, IntType},
    values::{BasicMetadataValueEnum, FunctionValue, GlobalValue, IntValue, PointerValue},
    AddressSpace, IntPredicate,
};

use crate::ast;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("LLVM builder error: {0}")]
    Builder(#[from] inkwell::builder::BuilderError),

    #[error("variable '{0}' not found")]
    VariableNotFound(String),

    #[error("function '{0}' not found")]
    FunctionNotFound(String),

    #[error("expected type '{expected}' but got '{got}'")]
    TypeMismatch { expected: ast::Type, got: ast::Type },

    #[error("function '{func}' expected '{expected}' arguments but got '{got}'")]
    ArgumentCountMismatch {
        func: String,
        expected: usize,
        got: usize,
    },
}

#[derive(Debug)]
pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    current_function: Option<FunctionValue<'ctx>>,
    variables: Vec<HashMap<String, Variable<'ctx>>>,
    functions: HashMap<String, Function<'ctx>>,

    printf: (FunctionValue<'ctx>, GlobalValue<'ctx>),
    scanf: (FunctionValue<'ctx>, GlobalValue<'ctx>),
    rand: FunctionValue<'ctx>,

    srand: FunctionValue<'ctx>,
    time: FunctionValue<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let printf = CodeGen::create_printf(context, &module);
        let scanf = CodeGen::create_scanf(context, &module);
        let (rand, srand, time) = CodeGen::create_rand(context, &module);

        Self {
            context,
            module,
            builder,
            printf,
            scanf,
            rand,
            srand,
            time,
            current_function: None,
            variables: vec![],
            functions: HashMap::default(),
        }
    }

    fn push_stack_frame(&mut self) {
        self.variables.push(HashMap::default());
    }

    fn pop_stack_frame(&mut self) {
        self.variables.pop();
    }

    fn get_variable(&self, name: &str) -> Option<Variable<'ctx>> {
        for stack_frame in self.variables.iter().rev() {
            if let Some(variable) = stack_frame.get(name) {
                return Some(variable.clone());
            }
        }

        None
    }

    fn insert_variable(&mut self, name: &str, variable: Variable<'ctx>) {
        let current_stack_frame = self.variables.last_mut().unwrap();

        current_stack_frame.insert(name.to_string(), variable);
    }

    pub fn dump_to_stderr(&self) {
        self.module.print_to_stderr();
    }

    pub fn dump_to_string(&self) -> LLVMString {
        self.module.print_to_string()
    }

    pub fn emit_program(&mut self, prog: &ast::Program) -> Result<(), Error> {
        for fn_decl in &prog.function_decls {
            self.emit_function_declaration(fn_decl)?;
        }

        let main_function_type = self.context.i32_type().fn_type(&[], false);
        let main_function = self.module.add_function("main", main_function_type, None);

        let bb = self.context.append_basic_block(main_function, "entry");
        self.builder.position_at_end(bb);

        self.current_function = Some(main_function);

        self.emit_srand_init()?;

        self.push_stack_frame();
        for stmt in &prog.statements {
            self.emit_statement(stmt)?;
        }
        self.pop_stack_frame();

        self.builder
            .build_return(Some(&self.context.i32_type().const_int(0, false)))?;

        Ok(())
    }

    fn create_printf(
        context: &'ctx Context,
        module: &Module<'ctx>,
    ) -> (FunctionValue<'ctx>, GlobalValue<'ctx>) {
        let printf_format = "%d\n";
        let printf_format_type = context
            .i8_type()
            .array_type((printf_format.len() + 1) as u32);
        let printf_format_global = module.add_global(printf_format_type, None, "write_format");

        printf_format_global.set_initializer(&context.const_string(printf_format.as_bytes(), true));

        let printf_args = [context.i32_type().ptr_type(AddressSpace::default()).into()];

        let printf_type = context.i32_type().fn_type(&printf_args, true);
        let printf_fn = module.add_function("printf", printf_type, None);

        (printf_fn, printf_format_global)
    }

    fn create_scanf(
        context: &'ctx Context,
        module: &Module<'ctx>,
    ) -> (FunctionValue<'ctx>, GlobalValue<'ctx>) {
        let scanf_format = "%d";
        let scanf_format_type = context
            .i8_type()
            .array_type((scanf_format.len() + 1) as u32);
        let scanf_format_global = module.add_global(scanf_format_type, None, "read_format");

        scanf_format_global.set_initializer(&context.const_string(scanf_format.as_bytes(), true));

        let scanf_args = [context.i32_type().ptr_type(AddressSpace::default()).into()];

        let scanf_type = context.i32_type().fn_type(&scanf_args, true);
        let scanf_fn = module.add_function("scanf", scanf_type, None);

        (scanf_fn, scanf_format_global)
    }

    fn create_rand(
        context: &'ctx Context,
        module: &Module<'ctx>,
    ) -> (
        FunctionValue<'ctx>,
        FunctionValue<'ctx>,
        FunctionValue<'ctx>,
    ) {
        let rand_fn = module.add_function("rand", context.i32_type().fn_type(&[], false), None);

        let srand_type = context
            .void_type()
            .fn_type(&[context.i32_type().into()], false);
        let srand_fn = module.add_function("srand", srand_type, None);

        let time_type = context
            .i32_type()
            .fn_type(&[context.i64_type().into()], false);
        let time_fn = module.add_function("time", time_type, None);

        (rand_fn, srand_fn, time_fn)
    }

    fn emit_srand_init(&mut self) -> Result<(), Error> {
        let time_null_ptr = self.context.i64_type().const_int(0_u64, false);
        let retval = self
            .builder
            .build_call(self.time, &[time_null_ptr.into()], "time_call")?;

        let current_time = retval.try_as_basic_value().unwrap_left().into_int_value();
        self.builder
            .build_call(self.srand, &[current_time.into()], "srand_call")?;

        Ok(())
    }

    pub fn emit_function_declaration(
        &mut self,
        fn_decl: &ast::FunctionDeclaration,
    ) -> Result<(), Error> {
        let function_params = fn_decl
            .arguments
            .iter()
            .map(|arg| arg.r#type.to_llvm(self.context).into())
            .collect::<Vec<BasicMetadataTypeEnum<'ctx>>>();

        let function_type = fn_decl
            .return_type
            .to_llvm(self.context)
            .fn_type(&function_params, false);

        let function = self.module.add_function(&fn_decl.name, function_type, None);

        self.functions.insert(
            fn_decl.name.clone(),
            Function {
                func: function,
                args: fn_decl
                    .arguments
                    .iter()
                    .map(|arg| arg.r#type.clone())
                    .collect(),
            },
        );

        let bb = self.context.append_basic_block(function, "entry");

        self.current_function = Some(function);

        self.builder.position_at_end(bb);

        self.push_stack_frame();

        for (idx, arg) in fn_decl.arguments.iter().enumerate() {
            let value = function.get_nth_param(idx as u32).unwrap();

            let alloca_ptr = self
                .builder
                .build_alloca(self.context.i32_type(), &arg.name)?;

            self.builder.build_store(alloca_ptr, value)?;
            self.insert_variable(
                &arg.name,
                Variable {
                    ptr: alloca_ptr,
                    r#type: arg.r#type.clone(),
                },
            );
        }

        for stmt in &fn_decl.statements {
            self.emit_statement(stmt)?;
        }
        self.pop_stack_frame();

        let default_ret_val = fn_decl
            .return_type
            .to_llvm(self.context)
            .const_int(0_u64, false);

        self.builder.build_return(Some(&default_ret_val))?;

        Ok(())
    }

    pub fn emit_statement(&mut self, stmt: &ast::Statement) -> Result<(), Error> {
        match stmt {
            ast::Statement::Declaration(decl) => self.emit_declaration(decl)?,
            ast::Statement::Return { value } => self.emit_return(value)?,
            ast::Statement::Assignment(assign) => self.emit_assignment(assign)?,
            ast::Statement::While(whil) => self.emit_while(whil)?,
            ast::Statement::If(i) => self.emit_if(i)?,
            ast::Statement::Write { value } => self.emit_write(value)?,
        };

        Ok(())
    }

    pub fn emit_declaration(&mut self, decl: &ast::Declaration) -> Result<(), Error> {
        let int_type = decl.r#type.to_llvm(self.context);

        let alloca_ptr = self.builder.build_alloca(int_type, &decl.variable)?;

        self.insert_variable(
            &decl.variable,
            Variable {
                ptr: alloca_ptr,
                r#type: decl.r#type.clone(),
            },
        );

        if let Some(default_value) = &decl.value {
            let init_val = self.emit_expression(default_value)?;
            let init_val_type = init_val.get_type().to_ast();

            if init_val_type != decl.r#type {
                return Err(Error::TypeMismatch {
                    expected: decl.r#type.clone(),
                    got: init_val_type.clone(),
                });
            }

            self.builder.build_store(alloca_ptr, init_val)?;
        }

        Ok(())
    }

    pub fn emit_assignment(&mut self, assign: &ast::Assignment) -> Result<(), Error> {
        let variable = self
            .get_variable(&assign.variable)
            .ok_or(Error::VariableNotFound(assign.variable.clone()))?;

        let val = self.emit_expression(&assign.value)?;
        let val_type = val.get_type().to_ast();

        if val_type != variable.r#type {
            return Err(Error::TypeMismatch {
                expected: variable.r#type.clone(),
                got: val_type.clone(),
            });
        }

        self.builder.build_store(variable.ptr, val)?;

        Ok(())
    }

    pub fn emit_return(&mut self, retval: &ast::Expression) -> Result<(), Error> {
        let ret_val = self.emit_expression(retval)?;

        let ret_val_type = ret_val.get_type().to_ast();
        let fn_return_type = self
            .current_function
            .unwrap()
            .get_type()
            .get_return_type()
            .unwrap()
            .into_int_type()
            .to_ast();

        if ret_val_type != fn_return_type {
            return Err(Error::TypeMismatch {
                expected: fn_return_type.clone(),
                got: ret_val_type.clone(),
            });
        }

        self.builder.build_return(Some(&ret_val))?;

        Ok(())
    }

    pub fn emit_while(&mut self, whil: &ast::While) -> Result<(), Error> {
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
        self.push_stack_frame();
        for stmt in &whil.statements {
            self.emit_statement(stmt)?;
        }
        self.pop_stack_frame();

        self.builder.build_unconditional_branch(condition_bb)?;

        self.builder.position_at_end(condition_bb);

        // Evaluate condition
        let condition = self.emit_expression(&whil.condition)?;

        self.builder
            .build_conditional_branch(condition, body_bb, end_bb)?;

        self.builder.position_at_end(end_bb);

        Ok(())
    }

    pub fn emit_if(&mut self, i: &ast::If) -> Result<(), Error> {
        let then_bb = self
            .context
            .append_basic_block(self.current_function.unwrap(), "if.then");

        let else_bb = self
            .context
            .append_basic_block(self.current_function.unwrap(), "if.else");

        let end_bb = self
            .context
            .append_basic_block(self.current_function.unwrap(), "if.end");

        let condition = self.emit_expression(&i.condition)?;

        self.builder
            .build_conditional_branch(condition, then_bb, else_bb)?;

        self.builder.position_at_end(then_bb);

        self.push_stack_frame();
        for stmt in &i.statements {
            self.emit_statement(stmt)?;
        }
        self.pop_stack_frame();
        self.builder.build_unconditional_branch(end_bb)?;

        self.builder.position_at_end(else_bb);
        // TODO: else condition
        self.builder.build_unconditional_branch(end_bb)?;

        self.builder.position_at_end(end_bb);

        Ok(())
    }

    pub fn emit_write(&mut self, value: &ast::Expression) -> Result<(), Error> {
        let value = self.emit_expression(value)?;

        let args: &[BasicMetadataValueEnum<'ctx>] =
            &[self.printf.1.as_pointer_value().into(), value.into()];

        self.builder.build_call(self.printf.0, args, "write_call")?;

        Ok(())
    }

    pub fn emit_expression(&mut self, expr: &ast::Expression) -> Result<IntValue<'ctx>, Error> {
        Ok(match expr {
            ast::Expression::Integer(value) => self.emit_integer(*value)?,
            ast::Expression::Boolean(value) => self.emit_boolean(*value)?,
            ast::Expression::Variable(var) => self.emit_variable(var)?,
            ast::Expression::FunctionCall(fn_call) => self.emit_function_call(fn_call)?,
            ast::Expression::Read => self.emit_read()?,
            ast::Expression::Random { max } => self.emit_rand(max)?,
            ast::Expression::BinaryOp { left, op, right } => {
                self.emit_binary_op(left, op, right)?
            }
            ast::Expression::UnaryOp { op, operand } => self.emit_unary_op(op, operand)?,
        })
    }

    pub fn emit_integer(&mut self, value: u32) -> Result<IntValue<'ctx>, Error> {
        Ok(self.context.i32_type().const_int(value as u64, false))
    }

    pub fn emit_boolean(&mut self, value: bool) -> Result<IntValue<'ctx>, Error> {
        Ok(self.context.bool_type().const_int(value.into(), false))
    }

    pub fn emit_variable(&mut self, name: &str) -> Result<IntValue<'ctx>, Error> {
        let variable = self
            .get_variable(name)
            .ok_or(Error::VariableNotFound(name.to_string()))?;

        let value = self
            .builder
            .build_load(self.context.i32_type(), variable.ptr, name)?;

        Ok(value.into_int_value())
    }

    pub fn emit_function_call(
        &mut self,
        fn_call: &ast::FunctionCall,
    ) -> Result<IntValue<'ctx>, Error> {
        let function = self
            .functions
            .get(&fn_call.name)
            .cloned()
            .ok_or(Error::FunctionNotFound(fn_call.name.clone()))?;

        if function.args.len() != fn_call.arguments.len() {
            return Err(Error::ArgumentCountMismatch {
                func: fn_call.name.clone(),
                expected: function.args.len(),
                got: fn_call.arguments.len(),
            });
        }

        let mut exprs = Vec::<BasicMetadataValueEnum<'ctx>>::with_capacity(function.args.len());

        for (fn_call_arg, function_arg) in fn_call.arguments.iter().zip(&function.args) {
            let expr = self.emit_expression(fn_call_arg)?;
            let expr_type = expr.get_type().to_ast();

            if &expr_type != function_arg {
                return Err(Error::TypeMismatch {
                    expected: function_arg.clone(),
                    got: expr_type.clone(),
                });
            }

            exprs.push(expr.into());
        }

        let retval =
            self.builder
                .build_call(function.func, &exprs, &format!("{}_call", &fn_call.name))?;
        Ok(retval.try_as_basic_value().unwrap_left().into_int_value())
    }

    pub fn emit_read(&mut self) -> Result<IntValue<'ctx>, Error> {
        let result_value = self
            .builder
            .build_alloca(self.context.i32_type(), "read_result")?;

        let args: &[BasicMetadataValueEnum<'ctx>] =
            &[self.scanf.1.as_pointer_value().into(), result_value.into()];

        self.builder.build_call(self.scanf.0, args, "read_call")?;

        let value =
            self.builder
                .build_load(self.context.i32_type(), result_value, "read_result")?;

        Ok(value.into_int_value())
    }

    pub fn emit_rand(&mut self, max: &ast::Expression) -> Result<IntValue<'ctx>, Error> {
        let max = self.emit_expression(max)?;

        let ret = self.builder.build_call(self.rand, &[], "rand_call")?;

        let big_retval = ret.try_as_basic_value().unwrap_left().into_int_value();
        let retval = self
            .builder
            .build_int_signed_rem(big_retval, max, "rand_mod")?;

        Ok(retval)
    }

    pub fn emit_binary_op(
        &mut self,
        left: &ast::Expression,
        op: &ast::BinaryOpType,
        right: &ast::Expression,
    ) -> Result<IntValue<'ctx>, Error> {
        let left = self.emit_expression(left)?;
        let right = self.emit_expression(right)?;

        let result = match op {
            ast::BinaryOpType::Add => self.builder.build_int_add(left, right, "add")?,
            ast::BinaryOpType::Sub => self.builder.build_int_sub(left, right, "sub")?,
            ast::BinaryOpType::Mul => self.builder.build_int_mul(left, right, "mul")?,
            ast::BinaryOpType::Div => self.builder.build_int_signed_div(left, right, "div")?,
            ast::BinaryOpType::Mod => self.builder.build_int_signed_rem(left, right, "mod")?,
            ast::BinaryOpType::Eq => {
                self.builder
                    .build_int_compare(IntPredicate::EQ, left, right, "eq")?
            }
            ast::BinaryOpType::Neq => {
                self.builder
                    .build_int_compare(IntPredicate::NE, left, right, "neq")?
            }
            ast::BinaryOpType::Lt => {
                self.builder
                    .build_int_compare(IntPredicate::SLT, left, right, "lt")?
            }
            ast::BinaryOpType::Lte => {
                self.builder
                    .build_int_compare(IntPredicate::SLE, left, right, "lte")?
            }
            ast::BinaryOpType::Gt => {
                self.builder
                    .build_int_compare(IntPredicate::SGT, left, right, "gt")?
            }
            ast::BinaryOpType::Gte => {
                self.builder
                    .build_int_compare(IntPredicate::SGE, left, right, "gte")?
            }
        };

        Ok(result)
    }

    pub fn emit_unary_op(
        &mut self,
        op: &ast::UnaryOpType,
        operand: &ast::Expression,
    ) -> Result<IntValue<'ctx>, Error> {
        let operand = self.emit_expression(operand)?;

        let result = match op {
            ast::UnaryOpType::Neg => self.builder.build_int_neg(operand, "neg")?,
            ast::UnaryOpType::Not => self.builder.build_not(operand, "not")?,
        };

        Ok(result)
    }
}

#[derive(Debug, Clone)]
struct Function<'ctx> {
    pub func: FunctionValue<'ctx>,
    pub args: Vec<ast::Type>,
}

#[derive(Debug, Clone)]
pub struct Variable<'ctx> {
    ptr: PointerValue<'ctx>,
    r#type: ast::Type,
}

trait ToAstType {
    fn to_ast(&self) -> ast::Type;
}

impl ToAstType for IntType<'_> {
    fn to_ast(&self) -> ast::Type {
        match self.get_bit_width() {
            1 => ast::Type::Boolean,
            32 => ast::Type::Integer,
            width => unreachable!("unknown IntType of width {width}"),
        }
    }
}

trait ToLlvmType {
    fn to_llvm<'ctx>(&self, context: &'ctx Context) -> IntType<'ctx>;
}

impl ToLlvmType for ast::Type {
    fn to_llvm<'ctx>(&self, context: &'ctx Context) -> IntType<'ctx> {
        match self {
            ast::Type::Integer => context.i32_type(),
            ast::Type::Boolean => context.bool_type(),
        }
    }
}
