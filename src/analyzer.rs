use crate::{ast, utils};
use std::collections::HashMap;

const MAX_LEVENSHTEIN_DIST_FOR_SUGGEST: usize = 3;

#[derive(Debug, Clone)]
struct Variable {
    r#type: ast::Type,

    /// Span to the variable declaration
    decl_span: ast::Span,

    /// Span to the variable type in the variable declaration
    type_decl_span: ast::Span,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub return_type: ast::Type,
    pub arguments: Vec<ast::Type>,

    /// Span to the return type in the function declaration
    ret_type_decl_span: ast::Span,

    /// Spans to the function arguments
    args_span: Vec<ast::Span>,

    /// Span to the function declaration
    decl_span: ast::Span,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum Error {
    #[error("type mismatch in operation")]
    #[diagnostic(code(floc::invalid_types_in_op))]
    TypeMismatchInOperation {
        #[source_code]
        src: miette::NamedSource<String>,

        operand_type: ast::TypeKind,

        #[label("expected {operator_type}, found '{operand_type}'")]
        operand: ast::Span,

        operator_type: ast::TypeKind,

        #[label("expected due to this operator")]
        operator: ast::Span,
    },

    #[error("type mismatch in assignment")]
    #[diagnostic(code(floc::invalid_types_in_assign))]
    TypeMismatchInAssign {
        #[source_code]
        src: miette::NamedSource<String>,

        expected_type: ast::TypeKind,

        wrong_value_type: ast::TypeKind,

        #[label("expected {expected_type}, found '{wrong_value_type}'")]
        wrong_value: ast::Span,

        #[label("expected due to this type")]
        type_def: ast::Span,
    },

    #[error("type mismatch in return statement")]
    #[diagnostic(code(floc::invalid_types_ret_stmt))]
    TypeMismatchInReturn {
        #[source_code]
        src: miette::NamedSource<String>,

        expected_type: ast::TypeKind,

        wrong_value_type: ast::TypeKind,

        #[label("expected {expected_type}, found '{wrong_value_type}'")]
        wrong_value: ast::Span,

        #[label("expected due to this type")]
        type_def: ast::Span,
    },

    #[error("type mismatch in condition")]
    #[diagnostic(code(floc::invalid_types_in_condition))]
    TypeMismatchInCondition {
        #[source_code]
        src: miette::NamedSource<String>,

        wrong_value_type: ast::TypeKind,

        #[label("expected booleen, found '{wrong_value_type}'")]
        wrong_value: ast::Span,
    },

    #[error("type mismatch in function argument")]
    #[diagnostic(code(floc::invalid_types_in_fn_arg))]
    TypeMismatchInFnArg {
        #[source_code]
        src: miette::NamedSource<String>,

        expected_type: ast::TypeKind,

        wrong_value_type: ast::TypeKind,

        #[label("expected {expected_type}, found '{wrong_value_type}'")]
        wrong_value: ast::Span,

        #[label("expected due to this type")]
        arg: ast::Span,

        #[label("arguments to this function are incorrect")]
        fn_call_name: ast::Span,
    },

    #[error("variable '{var_name}' not found")]
    #[diagnostic(code(floc::variable_not_found))]
    VariableNotFound {
        var_name: String,

        #[source_code]
        src: miette::NamedSource<String>,

        #[label("variable used here")]
        here: ast::Span,

        #[help]
        advice: Option<String>,
    },

    #[error("variable '{varname}' is already defined")]
    #[diagnostic(code(floc::variable_already_defined))]
    VariableAlreadyDefined {
        varname: String,

        #[source_code]
        src: miette::NamedSource<String>,

        #[label("here")]
        here: ast::Span,

        #[label("previous definition here")]
        previous_def: ast::Span,
    },

    #[error("function '{fn_name}' is already defined")]
    #[diagnostic(code(floc::function_already_defined))]
    FunctionAlreadyDefined {
        fn_name: String,

        #[source_code]
        src: miette::NamedSource<String>,

        #[label("here")]
        here: ast::Span,

        #[label("previous definition here")]
        previous_def: ast::Span,
    },

    #[error("function '{fn_name}' not found")]
    #[diagnostic(code(floc::function_not_found))]
    FunctionNotFound {
        fn_name: String,

        #[source_code]
        src: miette::NamedSource<String>,

        #[label("used here")]
        here: ast::Span,

        #[help]
        advice: Option<String>,
    },

    #[error("feturn statement outside of function body")]
    #[diagnostic(code(floc::return_statement_outside_fn_body))]
    ReturnOutsideFunction {
        #[source_code]
        src: miette::NamedSource<String>,

        #[label("here")]
        here: ast::Span,
    },

    #[error("not all code paths return")]
    #[diagnostic(code(floc::missing_return))]
    MissingReturn,

    #[error("extra statements after return")]
    #[diagnostic(code(floc::extra_stmts_after_return))]
    ExtraStmtsAfterReturn {
        #[source_code]
        src: miette::NamedSource<String>,

        #[label("here")]
        stmt_span: ast::Span,
    },

    #[error("function '{func}' expected {expected} argument(s) but got {got}")]
    #[diagnostic(code(floc::arg_count_mismatch))]
    ArgumentCountMismatch {
        func: String,

        expected: usize,

        got: usize,

        #[source_code]
        src: miette::NamedSource<String>,

        #[label("arguments to this function are incorrect")]
        fn_call_name: ast::Span,

        #[label("{got} arguments(s) supplied")]
        fn_call_args: Option<ast::Span>,

        #[label("expected {expected} arguments(s)")]
        fn_def_args: Option<ast::Span>,
    },
}

pub struct Analyzer {
    variables: Vec<HashMap<String, Variable>>,
    functions: HashMap<String, Function>,
    parent_function: Option<String>,
    source_code: miette::NamedSource<String>,
}

impl Analyzer {
    pub fn new(source_code: miette::NamedSource<String>) -> Self {
        Self {
            variables: Vec::with_capacity(1),
            functions: HashMap::new(),
            parent_function: None,
            source_code,
        }
    }

    pub fn functions(&self) -> &HashMap<String, Function> {
        &self.functions
    }

    fn enter_block(&mut self) {
        self.variables.push(HashMap::default());
    }

    fn leave_block(&mut self) {
        self.variables.pop();
    }

    fn get_variable(&self, var_name: &str) -> Option<&Variable> {
        self.variables
            .iter()
            .rev()
            .find_map(|block| block.get(var_name))
    }

    fn get_help_var_not_found(&self, var_name: &str) -> Option<String> {
        let variable_names = self
            .variables
            .iter()
            .flat_map(|frame| frame.keys())
            .map(|string| string.as_str());

        utils::closest_str(variable_names, var_name, MAX_LEVENSHTEIN_DIST_FOR_SUGGEST)
            .map(|name| format!("did you mean '{name}'?"))
    }

    fn get_help_fn_not_found(&self, fn_name: &str) -> Option<String> {
        let fn_names = self.functions.keys().map(|string| string.as_str());

        utils::closest_str(fn_names, fn_name, MAX_LEVENSHTEIN_DIST_FOR_SUGGEST)
            .map(|name| format!("did you mean '{name}'?"))
    }

    fn declare_variable(&mut self, var_name: &str, r#type: ast::Type, span: ast::Span) {
        let type_decl_span = r#type.span;

        self.variables.last_mut().unwrap().insert(
            var_name.to_string(),
            Variable {
                r#type,
                decl_span: span,
                type_decl_span,
            },
        );
    }

    pub fn analyze_program(&mut self, prog: &ast::Program) -> Result<(), Box<Error>> {
        // Register all functions
        for fn_decl in &prog.function_decls {
            if let Some(previous_fn) = self.functions.get(&fn_decl.name.ident) {
                return Err(Box::new(Error::FunctionAlreadyDefined {
                    fn_name: fn_decl.name.ident.to_string(),
                    src: self.source_code.clone(),
                    here: fn_decl.span,
                    previous_def: previous_fn.decl_span,
                }));
            }

            let args = fn_decl
                .arguments
                .iter()
                .map(|arg| arg.r#type.clone())
                .collect();

            self.functions.insert(
                fn_decl.name.ident.clone(),
                Function {
                    name: fn_decl.name.ident.clone(),
                    return_type: fn_decl.return_type.clone(),
                    arguments: args,
                    ret_type_decl_span: fn_decl.return_type.span,
                    args_span: fn_decl.arguments.iter().map(|arg| arg.span).collect(),
                    decl_span: fn_decl.span,
                },
            );
        }

        for fn_decl in &prog.function_decls {
            self.analyze_function_decl(fn_decl)?;
        }

        self.analyze_block(&prog.statements)?;

        Ok(())
    }

    fn analyze_function_decl(
        &mut self,
        fn_decl: &ast::FunctionDeclaration,
    ) -> Result<(), Box<Error>> {
        self.parent_function = Some(fn_decl.name.ident.clone());

        self.enter_block();

        for arg in &fn_decl.arguments {
            self.declare_variable(&arg.name, arg.r#type.clone(), arg.span);
        }

        let mut does_return = false;

        for stmt in &fn_decl.statements {
            if does_return {
                return Err(Box::new(Error::ExtraStmtsAfterReturn {
                    src: self.source_code.clone(),
                    stmt_span: *stmt.span(),
                }));
            }

            does_return = self.analyze_statement(stmt)?;
        }

        if !does_return {
            return Err(Box::new(Error::MissingReturn));
        }

        self.leave_block();

        self.parent_function = None;

        Ok(())
    }

    fn analyze_statement(&mut self, stmt: &ast::Statement) -> Result<bool, Box<Error>> {
        match stmt {
            ast::Statement::Assignment(assign) => self.analyze_assignment(assign),
            ast::Statement::Declaration(decl) => self.analyze_declaration(decl),
            ast::Statement::Write(write) => self.analyze_write(write),
            ast::Statement::Return(ret) => self.analyze_return(ret),
            ast::Statement::While(whil) => self.analyze_while(whil),
            ast::Statement::If(i) => self.analyze_if(i),
            ast::Statement::DiscardFunctionCall(fn_call) => {
                self.analyze_function_call(fn_call)?;
                Ok(false)
            }
        }
    }

    fn analyze_block(&mut self, stmts: &[ast::Statement]) -> Result<bool, Box<Error>> {
        self.enter_block();

        let mut does_return = false;

        for stmt in stmts {
            if does_return {
                return Err(Box::new(Error::ExtraStmtsAfterReturn {
                    src: self.source_code.clone(),
                    stmt_span: *stmt.span(),
                }));
            }

            if self.analyze_statement(stmt)? {
                does_return = true;
            }
        }

        self.leave_block();

        Ok(does_return)
    }

    fn analyze_return(&mut self, ret: &ast::Return) -> Result<bool, Box<Error>> {
        let current_function_name =
            self.parent_function
                .as_deref()
                .ok_or(Error::ReturnOutsideFunction {
                    src: self.source_code.clone(),
                    here: ret.span,
                })?;

        let defined_function = self.functions.get(current_function_name).unwrap().clone();

        let value_type = self.analyze_expr(&ret.value)?;

        if defined_function.return_type.kind != value_type.kind {
            return Err(Box::new(Error::TypeMismatchInReturn {
                src: self.source_code.clone(),
                expected_type: defined_function.return_type.kind,
                wrong_value_type: value_type.kind,
                wrong_value: value_type.span,
                type_def: defined_function.ret_type_decl_span,
            }));
        }

        Ok(true)
    }

    fn analyze_if(&mut self, i: &ast::If) -> Result<bool, Box<Error>> {
        let condition_type = self.analyze_expr(&i.condition)?;

        if condition_type.kind != ast::TypeKind::Boolean {
            return Err(Box::new(Error::TypeMismatchInCondition {
                src: self.source_code.clone(),
                wrong_value_type: condition_type.kind,
                wrong_value: condition_type.span,
            }));
        }

        let then_block_returns = self.analyze_block(&i.statements)?;

        if let Some(stmts_else) = &i.statements_else {
            let else_block_returns = self.analyze_block(stmts_else)?;

            Ok(then_block_returns && else_block_returns)
        } else {
            // The else body contains no statement, so it doesn't return
            Ok(false)
        }
    }

    fn analyze_while(&mut self, whil: &ast::While) -> Result<bool, Box<Error>> {
        let condition_type = self.analyze_expr(&whil.condition)?;

        if condition_type.kind != ast::TypeKind::Boolean {
            return Err(Box::new(Error::TypeMismatchInCondition {
                src: self.source_code.clone(),
                wrong_value_type: condition_type.kind,
                wrong_value: condition_type.span,
            }));
        }

        self.analyze_block(&whil.statements)?;

        Ok(false)
    }

    fn analyze_declaration(&mut self, declaration: &ast::Declaration) -> Result<bool, Box<Error>> {
        if let Some(previous_var) = self.get_variable(&declaration.variable) {
            return Err(Box::new(Error::VariableAlreadyDefined {
                varname: declaration.variable.ident.to_string(),
                src: self.source_code.clone(),
                here: declaration.span,
                previous_def: previous_var.decl_span,
            }));
        }

        if let Some(default_value) = &declaration.value {
            let default_value_type = self.analyze_expr(default_value)?;

            if declaration.r#type.kind != default_value_type.kind {
                return Err(Box::new(Error::TypeMismatchInAssign {
                    src: self.source_code.clone(),
                    expected_type: declaration.r#type.kind.clone(),
                    wrong_value_type: default_value_type.kind,
                    wrong_value: *default_value.span(),
                    type_def: declaration.r#type.span,
                }));
            }
        }

        self.declare_variable(
            &declaration.variable,
            declaration.r#type.clone(),
            declaration.span,
        );

        Ok(false)
    }

    fn analyze_assignment(&mut self, assignment: &ast::Assignment) -> Result<bool, Box<Error>> {
        let variable =
            self.get_variable(&assignment.variable)
                .cloned()
                .ok_or(Error::VariableNotFound {
                    var_name: assignment.variable.ident.to_string(),
                    src: self.source_code.clone(),
                    here: assignment.variable.span,
                    advice: self.get_help_var_not_found(&assignment.variable.ident),
                })?;

        let expr_type = self.analyze_expr(&assignment.value)?;

        if expr_type.kind != variable.r#type.kind {
            return Err(Box::new(Error::TypeMismatchInAssign {
                src: self.source_code.clone(),
                expected_type: variable.r#type.kind,
                wrong_value_type: expr_type.kind,
                wrong_value: expr_type.span,
                type_def: variable.type_decl_span,
            }));
        }

        Ok(false)
    }

    fn analyze_write(&mut self, write: &ast::Write) -> Result<bool, Box<Error>> {
        self.analyze_expr(&write.value)?;
        Ok(false)
    }

    fn analyze_expr(&mut self, expr: &ast::Expression) -> Result<ast::Type, Box<Error>> {
        match expr {
            ast::Expression::Integer(_, span) => Ok(ast::Type {
                kind: ast::TypeKind::Integer,
                span: *span,
            }),
            ast::Expression::Read(span) => Ok(ast::Type {
                kind: ast::TypeKind::Integer,
                span: *span,
            }),
            ast::Expression::Boolean(_, span) => Ok(ast::Type {
                kind: ast::TypeKind::Boolean,
                span: *span,
            }),
            ast::Expression::Variable(var) => self.analyze_variable(var),
            ast::Expression::FunctionCall(fn_call) => self.analyze_function_call(fn_call),
            ast::Expression::BinaryOp(binary_op) => self.analyze_binary_op(binary_op),
            ast::Expression::UnaryOp(unary_op) => self.analyze_unary_op(unary_op),
        }
    }

    fn analyze_function_call(
        &mut self,
        fn_call: &ast::FunctionCall,
    ) -> Result<ast::Type, Box<Error>> {
        let function =
            self.functions
                .get(&fn_call.name.ident)
                .cloned()
                .ok_or(Error::FunctionNotFound {
                    fn_name: fn_call.name.ident.to_string(),
                    src: self.source_code.clone(),
                    here: fn_call.span,
                    advice: self.get_help_fn_not_found(&fn_call.name.ident),
                })?;

        if function.arguments.len() != fn_call.arguments.len() {
            let fn_call_args = if !fn_call.arguments.is_empty() {
                let start = fn_call.arguments.first().unwrap().span().start;
                let end = fn_call.arguments.last().unwrap().span().end;

                Some(ast::Span { start, end })
            } else {
                None
            };

            let fn_def_args = if !function.arguments.is_empty() {
                let start = function.args_span.first().unwrap().start;
                let end = function.args_span.last().unwrap().end;

                Some(ast::Span { start, end })
            } else {
                None
            };

            return Err(Box::new(Error::ArgumentCountMismatch {
                func: fn_call.name.ident.clone(),
                expected: function.arguments.len(),
                got: fn_call.arguments.len(),
                src: self.source_code.clone(),
                fn_call_name: fn_call.name.span,
                fn_call_args,
                fn_def_args,
            }));
        }

        for (fn_call_arg, expected_type) in fn_call.arguments.iter().zip(function.arguments.iter())
        {
            let provided_arg_type = self.analyze_expr(fn_call_arg)?;

            if expected_type.kind != provided_arg_type.kind {
                return Err(Box::new(Error::TypeMismatchInFnArg {
                    src: self.source_code.clone(),
                    expected_type: expected_type.kind.clone(),
                    wrong_value_type: provided_arg_type.kind,
                    wrong_value: provided_arg_type.span,
                    arg: expected_type.span,
                    fn_call_name: fn_call.name.span,
                }));
            }
        }

        Ok(ast::Type {
            kind: function.return_type.kind,
            span: fn_call.span,
        })
    }

    fn analyze_variable(&mut self, variable: &ast::Identifier) -> Result<ast::Type, Box<Error>> {
        let var_type = self
            .get_variable(variable)
            .map(|var| var.r#type.kind.clone())
            .ok_or(Error::VariableNotFound {
                var_name: variable.ident.clone(),
                src: self.source_code.clone(),
                here: variable.span,
                advice: self.get_help_var_not_found(&variable.ident),
            })?;

        Ok(ast::Type {
            kind: var_type,
            span: variable.span,
        })
    }

    fn analyze_unary_op(&mut self, unary_op: &ast::UnaryOp) -> Result<ast::Type, Box<Error>> {
        let expected_type = match &unary_op.kind {
            ast::UnaryOpKind::Neg => ast::TypeKind::Integer,
            ast::UnaryOpKind::LogicNot => ast::TypeKind::Boolean,
        };

        let operand_type = self.analyze_expr(&unary_op.operand)?;

        if operand_type.kind != expected_type {
            return Err(Box::new(Error::TypeMismatchInOperation {
                src: self.source_code.clone(),
                operand_type: operand_type.kind,
                operand: operand_type.span,
                operator_type: expected_type,
                operator: unary_op.span,
            }));
        }

        // unary_op.span corresponds to the '-' and 'non' token,
        // so we create a new span with everything
        let span = ast::Span {
            start: unary_op.span.start,
            end: operand_type.span.end,
        };

        Ok(ast::Type {
            kind: expected_type,
            span,
        })
    }

    fn analyze_binary_op(&mut self, binary_op: &ast::BinaryOp) -> Result<ast::Type, Box<Error>> {
        use ast::BinaryOpKind::*;

        // This forbids 'example == Vrai', because:
        // 1. I'm lazy
        // 2. Nobody should ever write that
        let expected_operand_type = match &binary_op.kind {
            Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Lte | Gt | Gte => ast::TypeKind::Integer,
            LogicAnd | LogicOr => ast::TypeKind::Boolean,
        };

        let left_type = self.analyze_expr(&binary_op.left)?;
        if expected_operand_type != left_type.kind {
            return Err(Box::new(Error::TypeMismatchInOperation {
                src: self.source_code.clone(),
                operand_type: left_type.kind,
                operand: left_type.span,
                operator_type: expected_operand_type,
                operator: binary_op.span,
            }));
        }

        let right_type = self.analyze_expr(&binary_op.right)?;
        if expected_operand_type != right_type.kind {
            return Err(Box::new(Error::TypeMismatchInOperation {
                src: self.source_code.clone(),
                operand_type: right_type.kind,
                operand: right_type.span,
                operator_type: expected_operand_type,
                operator: binary_op.span,
            }));
        }

        let result_type = match &binary_op.kind {
            Add | Sub | Mul | Div | Mod => ast::TypeKind::Integer,
            Eq | Neq | Lt | Lte | Gt | Gte | LogicOr | LogicAnd => ast::TypeKind::Boolean,
        };

        // binary_op.span corresponds to the operator character,
        // so we create a new span with everything
        let span = ast::Span {
            start: left_type.span.start,
            end: right_type.span.end,
        };

        Ok(ast::Type {
            kind: result_type,
            span,
        })
    }
}
