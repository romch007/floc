use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    support::LLVMString,
    types::BasicMetadataTypeEnum,
    values::{BasicMetadataValueEnum, FunctionValue, GlobalValue, IntValue, PointerValue},
    AddressSpace,
};

use crate::ast;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("LLVM builder error")]
    BuilderError(#[from] inkwell::builder::BuilderError),

    #[error("variable '{0}' not found")]
    VariableNotFound(String),

    #[error("function '{0}' not found")]
    FunctionNotFound(String),

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
    variables: Vec<HashMap<String, PointerValue<'ctx>>>,
    functions: HashMap<String, Function<'ctx>>,
    printf: (FunctionValue<'ctx>, GlobalValue<'ctx>),
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let printf = CodeGen::create_printf(context, &module);

        Self {
            context,
            module,
            builder,
            printf,
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

    fn get_variable(&self, name: &str) -> Option<PointerValue<'ctx>> {
        for stack_frame in self.variables.iter().rev() {
            if let Some(variable) = stack_frame.get(name) {
                return Some(*variable);
            }
        }

        None
    }

    fn insert_variable(&mut self, name: &str, ptr: PointerValue<'ctx>) {
        let current_stack_frame = self.variables.last_mut().unwrap();

        current_stack_frame.insert(name.to_string(), ptr);
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
        let printf_format_type = context.i8_type().array_type(4);
        let printf_format_global = module.add_global(printf_format_type, None, "write_format");

        printf_format_global.set_initializer(&context.const_string(printf_format.as_bytes(), true));

        let printf_args = [context.i32_type().ptr_type(AddressSpace::default()).into()];

        let printf_type = context.i32_type().fn_type(&printf_args, false);
        let printf_fn = module.add_function("printf", printf_type, None);

        (printf_fn, printf_format_global)
    }

    pub fn emit_function_declaration(
        &mut self,
        fn_decl: &ast::FunctionDeclaration,
    ) -> Result<(), Error> {
        let function_params: Vec<BasicMetadataTypeEnum<'ctx>> =
            vec![self.context.i32_type().into(); fn_decl.arguments.len()];
        let function_type = self.context.i32_type().fn_type(&function_params, false);
        let function = self
            .module
            .add_function(fn_decl.name.as_ref(), function_type, None);

        self.functions.insert(
            fn_decl.name.as_ref().to_string(),
            Function {
                func: function,
                args_count: fn_decl.arguments.len(),
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
                .build_alloca(self.context.i32_type(), arg.name.as_ref())?;

            self.builder.build_store(alloca_ptr, value)?;
            self.insert_variable(arg.name.as_ref(), alloca_ptr);
        }

        for stmt in &fn_decl.statements {
            self.emit_statement(stmt)?;
        }
        self.pop_stack_frame();

        self.builder
            .build_return(Some(&self.context.i32_type().const_int(0 as u64, false)))?;

        Ok(())
    }

    pub fn emit_statement(&mut self, stmt: &ast::Statement) -> Result<(), Error> {
        match stmt {
            ast::Statement::Declaration(decl) => self.emit_declaration(decl)?,
            ast::Statement::Return(ret) => self.emit_return(ret)?,
            ast::Statement::Assignment(assign) => self.emit_assignment(assign)?,
            ast::Statement::While(whil) => self.emit_while(whil)?,
            ast::Statement::If(i) => self.emit_if(i)?,
            ast::Statement::Write(write) => self.emit_write(write)?,
            _ => unimplemented!("unimplemented {stmt:?}"),
        };

        Ok(())
    }

    pub fn emit_declaration(&mut self, decl: &ast::Declaration) -> Result<(), Error> {
        let alloca_ptr = self
            .builder
            .build_alloca(self.context.i32_type(), decl.variable.as_ref())?;

        self.insert_variable(decl.variable.as_ref(), alloca_ptr);

        if let Some(default_value) = &decl.value {
            let init_val = self.emit_expression(default_value)?;
            self.builder.build_store(alloca_ptr, init_val)?;
        }

        Ok(())
    }

    pub fn emit_assignment(&mut self, assign: &ast::Assignment) -> Result<(), Error> {
        let ptr = self
            .get_variable(assign.variable.as_ref())
            .ok_or(Error::VariableNotFound(assign.variable.value.clone()))?;

        let val = self.emit_expression(&assign.value)?;
        self.builder.build_store(ptr, val)?;

        Ok(())
    }

    pub fn emit_return(&mut self, ret: &ast::Return) -> Result<(), Error> {
        let ret_val = self.emit_expression(&ret.value)?;

        self.builder.build_return(Some(&ret_val))?;

        Ok(())
    }

    pub fn emit_while(&mut self, whil: &ast::While) -> Result<(), Error> {
        Ok(())
    }

    pub fn emit_if(&mut self, i: &ast::If) -> Result<(), Error> {
        let condition = self.emit_expression(&i.condition)?;
        let condition =
            self.builder
                .build_int_cast(condition, self.context.bool_type(), "condition")?;

        let then_bb = self
            .context
            .append_basic_block(self.current_function.unwrap(), "if.then");
        let else_bb = self
            .context
            .append_basic_block(self.current_function.unwrap(), "if.else");

        let _cond_br = self
            .builder
            .build_conditional_branch(condition, then_bb, else_bb)?;

        self.builder.position_at_end(then_bb);

        self.push_stack_frame();
        for stmt in &i.statements {
            self.emit_statement(stmt)?;
        }
        self.pop_stack_frame();

        self.builder.position_at_end(else_bb);

        Ok(())
    }

    pub fn emit_write(&mut self, write: &ast::Write) -> Result<(), Error> {
        let value = self.emit_expression(&write.value)?;

        let args: &[BasicMetadataValueEnum<'ctx>] =
            &[self.printf.1.as_pointer_value().into(), value.into()];

        self.builder.build_call(self.printf.0, args, "")?;

        Ok(())
    }

    pub fn emit_expression(&mut self, expr: &ast::Expression) -> Result<IntValue<'ctx>, Error> {
        Ok(match expr {
            ast::Expression::Integer(int) => self.emit_integer(int)?,
            ast::Expression::Variable(var) => self.emit_variable(var)?,
            ast::Expression::FunctionCall(fn_call) => self.emit_function_call(fn_call)?,
            _ => unimplemented!("unimplemented {expr:?}"),
        })
    }

    pub fn emit_integer(&mut self, int: &ast::Integer) -> Result<IntValue<'ctx>, Error> {
        Ok(self.context.i32_type().const_int(int.value as u64, false))
    }

    pub fn emit_variable(&mut self, var: &ast::Identifier) -> Result<IntValue<'ctx>, Error> {
        let ptr = self
            .get_variable(var.value.as_ref())
            .ok_or(Error::VariableNotFound(var.value.clone()))?;

        let value = self
            .builder
            .build_load(self.context.i32_type(), ptr, var.as_ref())?;

        Ok(value.into_int_value())
    }

    pub fn emit_function_call(
        &mut self,
        fn_call: &ast::FunctionCall,
    ) -> Result<IntValue<'ctx>, Error> {
        let function = self
            .functions
            .get(fn_call.name.as_ref())
            .cloned()
            .ok_or(Error::FunctionNotFound(fn_call.name.value.clone()))?;

        if function.args_count != fn_call.arguments.len() {
            return Err(Error::ArgumentCountMismatch {
                func: fn_call.name.value.clone(),
                expected: function.args_count,
                got: fn_call.arguments.len(),
            });
        }

        let exprs = fn_call
            .arguments
            .iter()
            .map(|arg| self.emit_expression(&arg).map(Into::into))
            .collect::<Result<Vec<_>, _>>()?;

        let retval = self.builder.build_call(function.func, &exprs, "")?;
        Ok(retval.try_as_basic_value().unwrap_left().into_int_value())
    }
}

#[derive(Debug, Clone)]
struct Function<'ctx> {
    pub func: FunctionValue<'ctx>,
    pub args_count: usize,
}
