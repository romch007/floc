use crate::{
    ast::{self},
    utils::{self, Span, SpanIterExt},
};
use std::{cell::Cell, collections::HashMap};

const MAX_LEVENSHTEIN_DIST_FOR_SUGGEST: usize = 3;

#[derive(Debug, Clone)]
struct Variable {
    r#type: ast::Type,
    use_count: Cell<usize>,
    initialized: Cell<bool>,

    /// Span to the variable declaration
    declaration_span: Span,

    /// Span to the variable type in the variable declaration
    type_declaration_span: Span,
}

impl Variable {
    pub fn new(
        r#type: ast::Type,
        declaration_span: Span,
        type_declaration_span: Span,
        initialized: bool,
    ) -> Self {
        Self {
            r#type,
            declaration_span,
            type_declaration_span,
            use_count: Cell::new(0),
            initialized: Cell::new(initialized),
        }
    }

    pub fn increment_use_count(&self) {
        self.use_count.replace(self.use_count.get() + 1);
    }

    pub fn set_initialized(&self) {
        self.initialized.replace(true);
    }

    pub fn is_unused(&self) -> bool {
        self.use_count.get() == 0
    }

    pub fn is_initialized(&self) -> bool {
        self.initialized.get()
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub return_type: ast::Type,
    pub arguments: Vec<ast::Type>,

    /// Span to the return type in the function declaration
    ret_type_decl_span: Span,

    /// Spans to the function arguments
    args_span: Vec<Span>,

    /// Span to the function declaration
    decl_span: Span,
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
        operand: Span,

        operator_type: ast::TypeKind,

        operator: Span,
    },

    #[error("type mismatch in {operator_name}: operands have different types")]
    #[diagnostic(code(floc::invalid_types_in_eq_or_neq))]
    TypeMismatchInEqOrNeq {
        #[source_code]
        src: miette::NamedSource<String>,

        left_operand_type: ast::TypeKind,

        #[label("found '{left_operand_type}'")]
        left_operand: Span,

        right_operand_type: ast::TypeKind,

        #[label("found '{right_operand_type}'")]
        right_operand: Span,

        operator_name: String,

        #[label("due to this operator")]
        operator: Span,
    },

    #[error("type mismatch in assignment")]
    #[diagnostic(code(floc::invalid_types_in_assign))]
    TypeMismatchInAssign {
        #[source_code]
        src: miette::NamedSource<String>,

        expected_type: ast::TypeKind,

        wrong_value_type: ast::TypeKind,

        #[label("expected {expected_type}, found '{wrong_value_type}'")]
        wrong_value: Span,

        #[label("expected due to this type")]
        type_def: Span,
    },

    #[error("type mismatch in return statement")]
    #[diagnostic(code(floc::invalid_types_ret_stmt))]
    TypeMismatchInReturn {
        #[source_code]
        src: miette::NamedSource<String>,

        expected_type: ast::TypeKind,

        wrong_value_type: ast::TypeKind,

        #[label("expected {expected_type}, found '{wrong_value_type}'")]
        wrong_value: Span,

        #[label("expected due to this type")]
        type_def: Span,
    },

    #[error("type mismatch in condition")]
    #[diagnostic(code(floc::invalid_types_in_condition))]
    TypeMismatchInCondition {
        #[source_code]
        src: miette::NamedSource<String>,

        wrong_value_type: ast::TypeKind,

        #[label("expected booleen, found '{wrong_value_type}'")]
        wrong_value: Span,
    },

    #[error("type mismatch in function argument")]
    #[diagnostic(code(floc::invalid_types_in_fn_arg))]
    TypeMismatchInFnArg {
        #[source_code]
        src: miette::NamedSource<String>,

        expected_type: ast::TypeKind,

        wrong_value_type: ast::TypeKind,

        #[label("expected {expected_type}, found '{wrong_value_type}'")]
        wrong_value: Span,

        #[label("expected due to this type")]
        arg: Span,

        #[label("arguments to this function are incorrect")]
        fn_call_name: Span,
    },

    #[error("variable '{var_name}' not found")]
    #[diagnostic(code(floc::variable_not_found))]
    VariableNotFound {
        var_name: String,

        #[source_code]
        src: miette::NamedSource<String>,

        #[label("variable used here")]
        here: Span,

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
        here: Span,

        #[label("previous definition here")]
        previous_def: Span,
    },

    #[error("function '{fn_name}' is already defined")]
    #[diagnostic(code(floc::function_already_defined))]
    FunctionAlreadyDefined {
        fn_name: String,

        #[source_code]
        src: miette::NamedSource<String>,

        #[label("here")]
        here: Span,

        #[label("previous definition here")]
        previous_def: Span,
    },

    #[error("function '{fn_name}' not found")]
    #[diagnostic(code(floc::function_not_found))]
    FunctionNotFound {
        fn_name: String,

        #[source_code]
        src: miette::NamedSource<String>,

        #[label("used here")]
        here: Span,

        #[help]
        advice: Option<String>,
    },

    #[error("feturn statement outside of function body")]
    #[diagnostic(code(floc::return_statement_outside_fn_body))]
    ReturnOutsideFunction {
        #[source_code]
        src: miette::NamedSource<String>,

        #[label("here")]
        here: Span,
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
        stmt_span: Span,
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
        fn_call_name: Span,

        #[label("{got} arguments(s) supplied")]
        fn_call_args: Option<Span>,

        #[label("expected {expected} arguments(s)")]
        fn_def_args: Option<Span>,
    },
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum Warning {
    #[error("unused variable '{varname}'")]
    #[diagnostic(code(floc::unused_variable))]
    #[diagnostic(severity(Warning))]
    UnusedVariable {
        varname: String,

        #[source_code]
        src: miette::NamedSource<String>,

        #[label("defined here")]
        varname_decl_span: Span,
    },

    #[error("variable '{varname}' is used uninitialized")]
    #[diagnostic(code(floc::variable_used_uninitialized))]
    #[diagnostic(severity(Warning))]
    VariableUsedUninitialized {
        varname: String,

        #[source_code]
        src: miette::NamedSource<String>,

        #[label("defined here")]
        varname_decl_span: Span,

        #[label("used here")]
        varname_use_span: Span,
    },
}

pub struct Analyzer {
    variables: Vec<HashMap<String, Variable>>,
    functions: HashMap<String, Function>,
    parent_function: Option<String>,
    source_code: miette::NamedSource<String>,
    warnings: Vec<miette::Report>,
}

impl Analyzer {
    pub fn new(source_code: miette::NamedSource<String>) -> Self {
        Self {
            variables: Vec::with_capacity(1),
            functions: HashMap::new(),
            parent_function: None,
            source_code,
            warnings: Vec::new(),
        }
    }

    pub fn functions(&self) -> &HashMap<String, Function> {
        &self.functions
    }

    pub fn warnings(&self) -> &[miette::Report] {
        &self.warnings
    }

    fn enter_block(&mut self) {
        self.variables.push(HashMap::default());
    }

    fn leave_block(&mut self) {
        if let Some(stack_frame) = self.variables.pop() {
            for (varname, unused_variable) in
                stack_frame.into_iter().filter(|entry| entry.1.is_unused())
            {
                let report = miette::Report::new(Warning::UnusedVariable {
                    varname,
                    src: self.source_code.clone(),
                    varname_decl_span: unused_variable.declaration_span,
                });

                self.warnings.push(report);
            }
        }
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

    fn declare_variable(
        &mut self,
        var_name: &str,
        r#type: ast::Type,
        span: Span,
        initialized: bool,
    ) {
        let type_decl_span = r#type.span;

        self.variables.last_mut().unwrap().insert(
            var_name.to_string(),
            Variable::new(r#type, span, type_decl_span, initialized),
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
            self.declare_variable(&arg.name, arg.r#type.clone(), arg.span, true);
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
                previous_def: previous_var.declaration_span,
            }));
        }

        let mut initialized = false;

        if let Some(default_value) = &declaration.value {
            let default_value_type = self.analyze_expr(default_value)?;

            initialized = true;

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
            initialized,
        );

        Ok(false)
    }

    fn analyze_assignment(&mut self, assignment: &ast::Assignment) -> Result<bool, Box<Error>> {
        let expr_type = self.analyze_expr(&assignment.value)?;

        let variable =
            self.get_variable(&assignment.variable)
                .ok_or_else(|| Error::VariableNotFound {
                    var_name: assignment.variable.ident.to_string(),
                    src: self.source_code.clone(),
                    here: assignment.variable.span,
                    advice: self.get_help_var_not_found(&assignment.variable.ident),
                })?;

        variable.set_initialized();

        if expr_type.kind != variable.r#type.kind {
            return Err(Box::new(Error::TypeMismatchInAssign {
                src: self.source_code.clone(),
                expected_type: variable.r#type.kind.clone(),
                wrong_value_type: expr_type.kind,
                wrong_value: expr_type.span,
                type_def: variable.type_declaration_span,
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
        let function = self
            .functions
            .get(&fn_call.name.ident)
            .cloned()
            .ok_or_else(|| Error::FunctionNotFound {
                fn_name: fn_call.name.ident.to_string(),
                src: self.source_code.clone(),
                here: fn_call.span,
                advice: self.get_help_fn_not_found(&fn_call.name.ident),
            })?;

        if function.arguments.len() != fn_call.arguments.len() {
            // Construct a span that ranges from the start of the first argument
            // to the end of the last argument of the function call
            let fn_call_args = fn_call
                .arguments
                .iter()
                .map(|expr| expr.span())
                .merge_spans();

            // Construct a span that ranges from the start of the first argument
            // to the end of the last argument of the function definition
            let fn_def_args = function.args_span.iter().merge_spans();

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
        let existing_variable =
            self.get_variable(variable)
                .ok_or_else(|| Error::VariableNotFound {
                    var_name: variable.ident.clone(),
                    src: self.source_code.clone(),
                    here: variable.span,
                    advice: self.get_help_var_not_found(&variable.ident),
                })?;

        existing_variable.increment_use_count();

        let var_type = existing_variable.r#type.clone();

        if !existing_variable.is_initialized() {
            let report = miette::Report::new(Warning::VariableUsedUninitialized {
                varname: variable.ident.clone(),
                src: self.source_code.clone(),
                varname_decl_span: existing_variable.declaration_span,
                varname_use_span: variable.span,
            });

            self.warnings.push(report);
        }

        Ok(ast::Type {
            kind: var_type.kind,
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
                operator: unary_op.operator_span,
            }));
        }

        Ok(ast::Type {
            kind: expected_type,
            span: unary_op.span,
        })
    }

    fn analyze_binary_op(&mut self, binary_op: &ast::BinaryOp) -> Result<ast::Type, Box<Error>> {
        use ast::BinaryOpKind::*;

        let result_type = if matches!(binary_op.kind, Eq | Neq) {
            // If it's an `equal` or `not equal` operation, check that both operands have the same type

            let left_type = self.analyze_expr(&binary_op.left)?;
            let right_type = self.analyze_expr(&binary_op.right)?;

            if left_type.kind != right_type.kind {
                let operator_name = match &binary_op.kind {
                    Eq => "equality",
                    Neq => "inequality",
                    _ => unreachable!(),
                };

                return Err(Box::new(Error::TypeMismatchInEqOrNeq {
                    src: self.source_code.clone(),
                    left_operand_type: left_type.kind,
                    left_operand: left_type.span,
                    right_operand_type: right_type.kind,
                    right_operand: right_type.span,
                    operator_name: operator_name.to_string(),
                    operator: binary_op.operator_span,
                }));
            }

            ast::TypeKind::Boolean
        } else {
            // If it's another kind of operation, do hardcoded checking

            let expected_operand_type = match &binary_op.kind {
                Add | Sub | Mul | Div | Mod | Lt | Lte | Gt | Gte => ast::TypeKind::Integer,
                LogicAnd | LogicOr => ast::TypeKind::Boolean,
                Eq | Neq => unreachable!(),
            };

            let left_type = self.analyze_expr(&binary_op.left)?;
            if expected_operand_type != left_type.kind {
                return Err(Box::new(Error::TypeMismatchInOperation {
                    src: self.source_code.clone(),
                    operand_type: left_type.kind,
                    operand: left_type.span,
                    operator_type: expected_operand_type,
                    operator: binary_op.operator_span,
                }));
            }

            let right_type = self.analyze_expr(&binary_op.right)?;
            if expected_operand_type != right_type.kind {
                return Err(Box::new(Error::TypeMismatchInOperation {
                    src: self.source_code.clone(),
                    operand_type: right_type.kind,
                    operand: right_type.span,
                    operator_type: expected_operand_type,
                    operator: binary_op.operator_span,
                }));
            }

            match &binary_op.kind {
                Add | Sub | Mul | Div | Mod => ast::TypeKind::Integer,
                Eq | Neq | Lt | Lte | Gt | Gte | LogicOr | LogicAnd => ast::TypeKind::Boolean,
            }
        };

        Ok(ast::Type {
            kind: result_type,
            span: binary_op.span,
        })
    }
}
