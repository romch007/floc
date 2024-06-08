use std::collections::HashMap;

use crate::ast;

macro_rules! match_type {
    ($expected:expr, $got:expr) => {{
        if $expected != $got {
            Err(Error::TypeMismatch {
                expected: $expected,
                got: $got,
            })
        } else {
            Ok(())
        }
    }};
}

#[derive(Debug)]
pub struct Variable {
    r#type: ast::Type,
}

#[derive(Debug, Clone)]
pub struct Function {
    return_type: ast::Type,
    args: Vec<ast::Type>,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Expected type '{expected}' but got '{got}'")]
    TypeMismatch { expected: ast::Type, got: ast::Type },

    #[error("Variable '{0}' not found")]
    VariableNotFound(String),

    #[error("Variable '{0}' is already defined")]
    VariableAlreadyDefined(String),

    #[error("Function '{0}' not found")]
    FunctionNotFound(String),

    #[error("Return statement outside of function body")]
    ReturnOutsideFunction,

    #[error("Function '{func}' expected {expected} arguments but got {got}")]
    ArgumentCountMismatch {
        func: String,
        expected: usize,
        got: usize,
    },
}

pub struct Analyzer {
    variables: Vec<HashMap<String, Variable>>,
    functions: HashMap<String, Function>,
    parent_function: Option<String>,
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            variables: vec![],
            functions: HashMap::new(),
            parent_function: None,
        }
    }

    fn enter_block(&mut self) {
        self.variables.push(HashMap::default());
    }

    fn leave_block(&mut self) {
        self.variables.pop();
    }

    fn get_variable(&mut self, var_name: &str) -> Option<&Variable> {
        self.variables
            .iter()
            .rev()
            .find_map(|block| block.get(var_name))
    }

    fn declare_variable(&mut self, var_name: &str, r#type: ast::Type) {
        self.variables
            .last_mut()
            .unwrap()
            .insert(var_name.to_string(), Variable { r#type });
    }

    pub fn analyze_program(&mut self, prog: &ast::Program) -> Result<(), Error> {
        // Register all functions
        for fn_decl in &prog.function_decls {
            let args = fn_decl.arguments.iter().map(|arg| arg.r#type).collect();

            self.functions.insert(
                fn_decl.name.clone(),
                Function {
                    return_type: fn_decl.return_type,
                    args,
                },
            );
        }

        for fn_decl in &prog.function_decls {
            self.analyze_function_decl(fn_decl)?;
        }

        self.analyze_block(&prog.statements)?;

        Ok(())
    }

    fn analyze_function_decl(&mut self, fn_decl: &ast::FunctionDeclaration) -> Result<(), Error> {
        self.parent_function = Some(fn_decl.name.clone());

        self.enter_block();

        for arg in &fn_decl.arguments {
            self.declare_variable(&arg.name, arg.r#type);
        }

        for stmt in &fn_decl.statements {
            self.analyze_statement(stmt)?;
        }

        self.leave_block();

        self.parent_function = None;

        Ok(())
    }

    fn analyze_statement(&mut self, stmt: &ast::Statement) -> Result<(), Error> {
        match stmt {
            ast::Statement::Assignment(assign) => self.analyze_assignment(assign),
            ast::Statement::Declaration(decl) => self.analyze_declaration(decl),
            ast::Statement::Write { value } => {
                // We can print whatever type we want, can we?

                self.analyze_expr(value)?;

                Ok(())
            }
            ast::Statement::Return { value } => self.analyze_return(value),
            ast::Statement::While(whil) => self.analyze_while(whil),
            ast::Statement::If(i) => self.analyze_if(i),
            ast::Statement::DiscardFunctionCall(fn_call) => {
                self.analyze_function_call(fn_call)?;

                Ok(())
            }
        }
    }

    fn analyze_block(&mut self, stmts: &[ast::Statement]) -> Result<(), Error> {
        self.enter_block();

        for stmt in stmts {
            self.analyze_statement(stmt)?;
        }

        self.leave_block();

        Ok(())
    }

    fn analyze_return(&mut self, value: &ast::Expression) -> Result<(), Error> {
        let current_function_name = self
            .parent_function
            .as_deref()
            .ok_or(Error::ReturnOutsideFunction)?;

        let function_ret_type = self
            .functions
            .get(current_function_name)
            .unwrap()
            .return_type;

        let value_type = self.analyze_expr(value)?;

        match_type!(function_ret_type, value_type)?;

        Ok(())
    }

    fn analyze_if(&mut self, i: &ast::If) -> Result<(), Error> {
        let condition_type = self.analyze_expr(&i.condition)?;
        match_type!(ast::Type::Boolean, condition_type)?;

        self.analyze_block(&i.statements)?;

        if let Some(stmts_else) = &i.statements_else {
            self.analyze_block(stmts_else)?;
        }

        Ok(())
    }

    fn analyze_while(&mut self, whil: &ast::While) -> Result<(), Error> {
        let condition_type = self.analyze_expr(&whil.condition)?;
        match_type!(ast::Type::Boolean, condition_type)?;

        self.analyze_block(&whil.statements)?;

        Ok(())
    }

    fn analyze_declaration(&mut self, declaration: &ast::Declaration) -> Result<(), Error> {
        if self.get_variable(&declaration.variable).is_some() {
            return Err(Error::VariableAlreadyDefined(declaration.variable.clone()));
        }

        if let Some(default_value) = &declaration.value {
            let default_value_type = self.analyze_expr(default_value)?;
            match_type!(declaration.r#type, default_value_type)?;
        }

        self.declare_variable(&declaration.variable, declaration.r#type);

        Ok(())
    }

    fn analyze_assignment(&mut self, assignment: &ast::Assignment) -> Result<(), Error> {
        let variable_type = self
            .get_variable(&assignment.variable)
            .map(|var| var.r#type)
            .ok_or(Error::VariableNotFound(assignment.variable.clone()))?;

        let expr_type = self.analyze_expr(&assignment.value)?;

        match_type!(variable_type, expr_type)?;

        Ok(())
    }

    fn analyze_expr(&mut self, expr: &ast::Expression) -> Result<ast::Type, Error> {
        match expr {
            ast::Expression::Integer(_) => Ok(ast::Type::Integer),
            ast::Expression::Boolean(_) => Ok(ast::Type::Boolean),
            ast::Expression::Variable(var) => self.analyze_variable(var),
            ast::Expression::Read => Ok(ast::Type::Integer),
            ast::Expression::Random { max } => {
                match_type!(ast::Type::Integer, self.analyze_expr(max)?)?;
                Ok(ast::Type::Integer)
            }
            ast::Expression::FunctionCall(fn_call) => self.analyze_function_call(fn_call),
            ast::Expression::BinaryOp { left, op, right } => {
                self.analyze_binary_op(left, op, right)
            }
            ast::Expression::UnaryOp { op, operand } => self.analyze_unary_op(op, operand),
        }
    }

    fn analyze_function_call(&mut self, fn_call: &ast::FunctionCall) -> Result<ast::Type, Error> {
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

        for (fn_call_arg, expected_type) in fn_call.arguments.iter().zip(function.args.iter()) {
            let arg_type = self.analyze_expr(fn_call_arg)?;

            match_type!(*expected_type, arg_type)?;
        }

        Ok(function.return_type)
    }

    fn analyze_variable(&mut self, var_name: &str) -> Result<ast::Type, Error> {
        self.get_variable(var_name)
            .map(|var| var.r#type)
            .ok_or(Error::VariableNotFound(var_name.to_string()))
    }

    fn analyze_unary_op(
        &mut self,
        op: &ast::UnaryOpType,
        operand: &ast::Expression,
    ) -> Result<ast::Type, Error> {
        let expected_type = match op {
            ast::UnaryOpType::Neg => ast::Type::Integer,
            ast::UnaryOpType::LogicNot => ast::Type::Boolean,
        };

        let operand_type = self.analyze_expr(operand)?;
        match_type!(expected_type, operand_type)?;

        Ok(expected_type)
    }

    fn analyze_binary_op(
        &mut self,
        left: &ast::Expression,
        op: &ast::BinaryOpType,
        right: &ast::Expression,
    ) -> Result<ast::Type, Error> {
        use ast::BinaryOpType::*;

        let expected_operand_type = match op {
            Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Lte | Gt | Gte => ast::Type::Integer,
            LogicAnd | LogicOr => ast::Type::Boolean,
        };

        let left_type = self.analyze_expr(left)?;
        match_type!(expected_operand_type, left_type)?;

        let right_type = self.analyze_expr(right)?;
        match_type!(expected_operand_type, right_type)?;

        let result_type = match op {
            Add | Sub | Mul | Div | Mod => ast::Type::Integer,
            Eq | Neq | Lt | Lte | Gt | Gte | LogicOr | LogicAnd => ast::Type::Boolean,
        };

        Ok(result_type)
    }
}
