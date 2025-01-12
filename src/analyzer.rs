use crate::ast;
use std::collections::HashMap;

#[derive(Debug)]
struct Variable {
    r#type: ast::Type,
    decl_span: ast::Span,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub return_type: ast::Type,
    pub arguments: Vec<ast::Type>,
    decl_span: ast::Span,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum Error {
    #[error("Expected type '{expected}' but got '{got}'")]
    #[diagnostic(code(floc::type_mismatch))]
    TypeMismatch {
        expected: ast::TypeKind,
        got: ast::TypeKind,
    },

    #[error("variable {var_name} not found")]
    #[diagnostic(code(floc::variable_not_found))]
    VariableNotFound {
        var_name: String,

        #[source_code]
        src: miette::NamedSource<String>,

        #[label("variable used here")]
        here: miette::SourceSpan,
    },

    #[error("Variable '{varname}' is already defined")]
    #[diagnostic(code(floc::variable_already_defined))]
    VariableAlreadyDefined {
        varname: String,

        #[source_code]
        src: miette::NamedSource<String>,

        #[label("here")]
        here: miette::SourceSpan,

        #[label("previous definition here")]
        previous_def: miette::SourceSpan,
    },

    #[error("Function '{fn_name}' is already defined")]
    #[diagnostic(code(floc::function_already_defined))]
    FunctionAlreadyDefined {
        fn_name: String,

        #[source_code]
        src: miette::NamedSource<String>,

        #[label("here")]
        here: miette::SourceSpan,

        #[label("previous definition here")]
        previous_def: miette::SourceSpan,
    },

    #[error("Function '{fn_name}' not found")]
    #[diagnostic(code(floc::function_not_found))]
    FunctionNotFound {
        fn_name: String,

        #[source_code]
        src: miette::NamedSource<String>,

        #[label("used here")]
        here: miette::SourceSpan,
    },

    #[error("Return statement outside of function body")]
    #[diagnostic(code(floc::return_statement_outside_fn_body))]
    ReturnOutsideFunction {
        #[source_code]
        src: miette::NamedSource<String>,

        #[label("here")]
        here: miette::SourceSpan,
    },

    #[error("Not all code paths return")]
    #[diagnostic(code(floc::missing_return))]
    MissingReturn,

    #[error("Extra statements after return")]
    #[diagnostic(code(floc::extra_stmts_after_return))]
    ExtraStmtsAfterReturn {
        #[source_code]
        src: miette::NamedSource<String>,

        #[label("here")]
        stmt_span: miette::SourceSpan,
    },

    #[error("Function '{func}' expected {expected} argument(s) but got {got}")]
    #[diagnostic(code(floc::arg_count_mismatch))]
    ArgumentCountMismatch {
        func: String,

        expected: usize,

        got: usize,

        #[source_code]
        src: miette::NamedSource<String>,

        #[label("function takes {expected} argument(s)")]
        fn_decl_span: miette::SourceSpan,

        #[label("function was called with {got} argument(s)")]
        fn_call_span: miette::SourceSpan,
    },
}

fn match_type(expected: &ast::TypeKind, got: &ast::TypeKind) -> Result<(), Error> {
    if expected == got {
        Ok(())
    } else {
        Err(Error::TypeMismatch {
            expected: expected.clone(),
            got: got.clone(),
        })
    }
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

    fn variable_exists(&self, var_name: &str) -> bool {
        self.variables
            .iter()
            .rev()
            .any(|block| block.contains_key(var_name))
    }

    fn declare_variable(&mut self, var_name: &str, r#type: ast::Type, span: ast::Span) {
        self.variables.last_mut().unwrap().insert(
            var_name.to_string(),
            Variable {
                r#type,
                decl_span: span,
            },
        );
    }

    pub fn analyze_program(&mut self, prog: &ast::Program) -> Result<(), Error> {
        // Register all functions
        for fn_decl in &prog.function_decls {
            if let Some(previous_fn) = self.functions.get(&fn_decl.name.ident) {
                return Err(Error::FunctionAlreadyDefined {
                    fn_name: fn_decl.name.ident.to_string(),
                    src: self.source_code.clone(),
                    here: fn_decl.span.clone().into(),
                    previous_def: previous_fn.decl_span.clone().into(),
                });
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
                    decl_span: fn_decl.span.clone(),
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
        self.parent_function = Some(fn_decl.name.ident.clone());

        self.enter_block();

        for arg in &fn_decl.arguments {
            self.declare_variable(&arg.name, arg.r#type.clone(), arg.span.clone());
        }

        let mut does_return = false;

        for stmt in &fn_decl.statements {
            if does_return {
                return Err(Error::ExtraStmtsAfterReturn {
                    src: self.source_code.clone(),
                    stmt_span: stmt.span().clone().into(),
                });
            }

            does_return = self.analyze_statement(stmt)?;
        }

        if !does_return {
            return Err(Error::MissingReturn);
        }

        self.leave_block();

        self.parent_function = None;

        Ok(())
    }

    fn analyze_statement(&mut self, stmt: &ast::Statement) -> Result<bool, Error> {
        match stmt {
            ast::Statement::Assignment(assign) => self.analyze_assignment(assign),
            ast::Statement::Declaration(decl) => self.analyze_declaration(decl),
            ast::Statement::Write(write) => {
                // TODO: wrap this in dedicated function
                self.analyze_expr(&write.value)?;
                Ok(false)
            }
            ast::Statement::Return(ret) => self.analyze_return(ret),
            ast::Statement::While(whil) => self.analyze_while(whil),
            ast::Statement::If(i) => self.analyze_if(i),
            ast::Statement::DiscardFunctionCall(fn_call) => {
                self.analyze_function_call(fn_call)?;
                Ok(false)
            }
        }
    }

    fn analyze_block(&mut self, stmts: &[ast::Statement]) -> Result<bool, Error> {
        self.enter_block();

        let mut does_return = false;

        for stmt in stmts {
            if does_return {
                return Err(Error::ExtraStmtsAfterReturn {
                    src: self.source_code.clone(),
                    stmt_span: stmt.span().clone().into(),
                });
            }

            if self.analyze_statement(stmt)? {
                does_return = true;
            }
        }

        self.leave_block();

        Ok(does_return)
    }

    fn analyze_return(&mut self, ret: &ast::Return) -> Result<bool, Error> {
        let current_function_name =
            self.parent_function
                .as_deref()
                .ok_or(Error::ReturnOutsideFunction {
                    src: self.source_code.clone(),
                    here: ret.span.clone().into(),
                })?;

        let function_ret_type = self
            .functions
            .get(current_function_name)
            .unwrap()
            .return_type
            .clone();

        let value_type = self.analyze_expr(&ret.value)?;
        match_type(&function_ret_type.kind, &value_type)?;

        Ok(true)
    }

    fn analyze_if(&mut self, i: &ast::If) -> Result<bool, Error> {
        let condition_type = self.analyze_expr(&i.condition)?;
        match_type(&ast::TypeKind::Boolean, &condition_type)?;

        let then_block_returns = self.analyze_block(&i.statements)?;

        if let Some(stmts_else) = &i.statements_else {
            let else_block_returns = self.analyze_block(stmts_else)?;

            Ok(then_block_returns && else_block_returns)
        } else {
            // The else body contains no statement, so it doesn't return
            Ok(false)
        }
    }

    fn analyze_while(&mut self, whil: &ast::While) -> Result<bool, Error> {
        let condition_type = self.analyze_expr(&whil.condition)?;
        match_type(&ast::TypeKind::Boolean, &condition_type)?;

        self.analyze_block(&whil.statements)?;

        Ok(false)
    }

    fn analyze_declaration(&mut self, declaration: &ast::Declaration) -> Result<bool, Error> {
        if let Some(previous_var) = self.get_variable(&declaration.variable) {
            return Err(Error::VariableAlreadyDefined {
                varname: declaration.variable.ident.to_string(),
                src: self.source_code.clone(),
                here: declaration.span.clone().into(),
                previous_def: previous_var.decl_span.clone().into(),
            });
        }

        if let Some(default_value) = &declaration.value {
            let default_value_type = self.analyze_expr(default_value)?;
            match_type(&declaration.r#type.kind, &default_value_type)?;
        }

        self.declare_variable(
            &declaration.variable,
            declaration.r#type.clone(),
            declaration.span.clone(),
        );

        Ok(false)
    }

    fn analyze_assignment(&mut self, assignment: &ast::Assignment) -> Result<bool, Error> {
        let variable_span = assignment.variable.span.clone();

        let variable_type = self
            .get_variable(&assignment.variable)
            .map(|var| var.r#type.kind.clone())
            .ok_or(Error::VariableNotFound {
                var_name: assignment.variable.ident.to_string(),
                src: self.source_code.clone(),
                here: variable_span.into(),
            })?;

        let expr_type = self.analyze_expr(&assignment.value)?;
        match_type(&variable_type, &expr_type)?;

        Ok(false)
    }

    fn analyze_expr(&mut self, expr: &ast::Expression) -> Result<ast::TypeKind, Error> {
        match expr {
            ast::Expression::Integer(_) | ast::Expression::Read => Ok(ast::TypeKind::Integer),
            ast::Expression::Boolean(_) => Ok(ast::TypeKind::Boolean),
            ast::Expression::Variable(var) => self.analyze_variable(var),
            ast::Expression::FunctionCall(fn_call) => self.analyze_function_call(fn_call),
            ast::Expression::BinaryOp(binary_op) => self.analyze_binary_op(binary_op),
            ast::Expression::UnaryOp(unary_op) => self.analyze_unary_op(unary_op),
        }
    }

    fn analyze_function_call(
        &mut self,
        fn_call: &ast::FunctionCall,
    ) -> Result<ast::TypeKind, Error> {
        let function =
            self.functions
                .get(&fn_call.name.ident)
                .cloned()
                .ok_or(Error::FunctionNotFound {
                    fn_name: fn_call.name.ident.to_string(),
                    src: self.source_code.clone(),
                    here: fn_call.span.clone().into(),
                })?;

        if function.arguments.len() != fn_call.arguments.len() {
            return Err(Error::ArgumentCountMismatch {
                func: fn_call.name.ident.clone(),
                expected: function.arguments.len(),
                got: fn_call.arguments.len(),
                src: self.source_code.clone(),
                fn_decl_span: function.decl_span.clone().into(),
                fn_call_span: fn_call.span.clone().into(),
            });
        }

        for (fn_call_arg, expected_type) in fn_call.arguments.iter().zip(function.arguments.iter())
        {
            let arg_type = self.analyze_expr(fn_call_arg)?;
            match_type(&expected_type.kind, &arg_type)?;
        }

        Ok(function.return_type.kind)
    }

    fn analyze_variable(&mut self, var_name: &ast::Identifier) -> Result<ast::TypeKind, Error> {
        let var_span = var_name.span.clone();

        self.get_variable(var_name)
            .map(|var| var.r#type.kind.clone())
            .ok_or(Error::VariableNotFound {
                var_name: var_name.ident.clone(),
                src: self.source_code.clone(),
                here: var_span.into(),
            })
    }

    fn analyze_unary_op(&mut self, unary_op: &ast::UnaryOp) -> Result<ast::TypeKind, Error> {
        let expected_type = match &unary_op.kind {
            ast::UnaryOpKind::Neg => ast::TypeKind::Integer,
            ast::UnaryOpKind::LogicNot => ast::TypeKind::Boolean,
        };

        let operand_type = self.analyze_expr(&unary_op.operand)?;
        match_type(&expected_type, &operand_type)?;

        Ok(expected_type)
    }

    fn analyze_binary_op(&mut self, binary_op: &ast::BinaryOp) -> Result<ast::TypeKind, Error> {
        use ast::BinaryOpKind::*;

        // NOTE: this forbids 'example == Vrai', because:
        // 1. I'm lazy
        // 2. Nobody should ever write that
        let expected_operand_type = match &binary_op.kind {
            Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Lte | Gt | Gte => ast::TypeKind::Integer,
            LogicAnd | LogicOr => ast::TypeKind::Boolean,
        };

        let left_type = self.analyze_expr(&binary_op.left)?;
        match_type(&expected_operand_type, &left_type)?;

        let right_type = self.analyze_expr(&binary_op.right)?;
        match_type(&expected_operand_type, &right_type)?;

        let result_type = match &binary_op.kind {
            Add | Sub | Mul | Div | Mod => ast::TypeKind::Integer,
            Eq | Neq | Lt | Lte | Gt | Gte | LogicOr | LogicAnd => ast::TypeKind::Boolean,
        };

        Ok(result_type)
    }
}

#[cfg(test)]
mod tests {
    macro_rules! good_input_test {
        ($testname:ident) => {
            #[test]
            fn $testname() {
                let source = include_str!(concat!("../tests/good/", stringify!($testname), ".flo"));
                let prog = crate::parser::parse(source).unwrap();

                let mut analyzer = crate::analyzer::Analyzer::new();
                let ret = analyzer.analyze_program(&prog);
                assert!(ret.is_ok());
            }
        };
    }

    macro_rules! bad_input_test {
        ($testname:ident, $expected:pat $(if $guard:expr)?) => {
            #[test]
            fn $testname() {
                let source = include_str!(concat!("../tests/bad/", stringify!($testname), ".flo"));
                let prog = crate::parser::parse(source).unwrap();

                let mut analyzer = crate::analyzer::Analyzer::new();
                let ret = analyzer.analyze_program(&prog);
                assert!(matches!(ret, Err($expected) $(if $guard)?));
            }
        };
    }

    macro_rules! bad_input_test_at_parsing {
        ($testname:ident) => {
            #[test]
            fn $testname() {
                let source = include_str!(concat!("../tests/bad/", stringify!($testname), ".flo"));
                let ret = crate::parser::parse(source);

                assert!(ret.is_err());
            }
        };
    }

    mod good {
        good_input_test!(affectation);
        good_input_test!(arith_1);
        good_input_test!(arith_2);
        good_input_test!(arith_3);
        good_input_test!(arith_4);
        good_input_test!(arith_5);
        good_input_test!(arith_6);
        good_input_test!(arith_7);
        good_input_test!(arith_8);
        good_input_test!(boucle_1);
        good_input_test!(boucle_2);
        good_input_test!(comp_1);
        good_input_test!(comp_2);
        good_input_test!(comp_3);
        good_input_test!(comp_4);
        good_input_test!(comp);
        good_input_test!(eval_lexicale);
        good_input_test!(eval_syntaxique);
        good_input_test!(exemple1);
        good_input_test!(exemple2);
        good_input_test!(exemple);
        good_input_test!(fonction_10);
        good_input_test!(fonction_11);
        good_input_test!(fonction_12);
        good_input_test!(fonction_13);
        good_input_test!(fonction_1);
        good_input_test!(fonction_2);
        good_input_test!(fonction_3);
        good_input_test!(fonction_4);
        good_input_test!(fonction_5);
        good_input_test!(fonction_6);
        good_input_test!(fonction_7);
        good_input_test!(fonction_8);
        good_input_test!(fonction_9);
        good_input_test!(lire);
        good_input_test!(log_1);
        good_input_test!(log_2);
        good_input_test!(log_3);
        good_input_test!(log_4);
        good_input_test!(log);
        good_input_test!(priorite);
        good_input_test!(si_1);
        good_input_test!(si_2);
        good_input_test!(si_3);
        good_input_test!(si_4);
        good_input_test!(si_5);
        good_input_test!(si_6);
        good_input_test!(si_7);
        good_input_test!(si_8);
        good_input_test!(si);
        good_input_test!(variable_1);
        good_input_test!(variable_2);
        good_input_test!(variable);
    }

    mod edge_cases {
        use crate::parser;

        #[test]
        #[ignore]
        fn nona() {
            use crate::ast::*;

            let prog = parser::parse("ecrire(nona);").unwrap();

            let write_arg = match &prog.statements[0] {
                Statement::Write { value } => value,
                _ => panic!("not a write"),
            };

            let var_name = match write_arg {
                Expression::Variable(var_name) => var_name,
                _ => panic!("not a variable"),
            };

            assert_eq!(var_name, "nona");
        }
    }

    mod bad {
        use crate::analyzer::Error::*;
        use crate::ast::TypeKind::*;

        bad_input_test!(
            affectation_1,
            TypeMismatch {
                expected: Integer,
                got: Boolean
            }
        );

        bad_input_test!(
            affectation_2,
            TypeMismatch {
                expected: Boolean,
                got: Integer
            }
        );

        bad_input_test!(affectation_3, VariableAlreadyDefined(name) if name == "a");

        bad_input_test!(affectation_4, VariableNotFound(name) if name == "a");

        bad_input_test!(affectation_5, VariableNotFound(name) if name == "a");

        bad_input_test!(affectation_6, VariableNotFound(name) if name == "rep");

        bad_input_test!(affectation_7, VariableNotFound(name) if name == "a");

        bad_input_test!(affectation_8, VariableNotFound(name) if name == "a");

        bad_input_test!(fonction_1, FunctionNotFound(name) if name == "f");

        bad_input_test!(
            fonction_2,
            TypeMismatch {
                expected: Boolean,
                got: Integer
            }
        );

        bad_input_test!(
            fonction_3,
            TypeMismatch {
                expected: Integer,
                got: Boolean
            }
        );

        bad_input_test!(fonction_4, ReturnOutsideFunction);

        bad_input_test!(
            fonction_5,
            ArgumentCountMismatch {
                func: fn_name,
                expected: 0,
                got: 1
            }
            if fn_name == "f"
        );

        bad_input_test!(
            fonction_6,
            ArgumentCountMismatch {
                func: fn_name,
                expected: 1,
                got: 0
            }
            if fn_name == "f"
        );

        bad_input_test!(
            fonction_7,
            TypeMismatch {
                expected: Integer,
                got: Boolean
            }
        );

        bad_input_test!(
            type_1,
            TypeMismatch {
                expected: Integer,
                got: Boolean
            }
        );

        bad_input_test!(
            type_2,
            TypeMismatch {
                expected: Boolean,
                got: Integer
            }
        );

        bad_input_test!(
            type_3,
            TypeMismatch {
                expected: Boolean,
                got: Integer
            }
        );

        bad_input_test!(
            type_4,
            TypeMismatch {
                expected: Boolean,
                got: Integer
            }
        );

        bad_input_test!(
            type_5,
            TypeMismatch {
                expected: Boolean,
                got: Integer
            }
        );

        bad_input_test_at_parsing!(si_1);
        bad_input_test_at_parsing!(si_2);
        bad_input_test_at_parsing!(si_3);

        bad_input_test!(
            si_4,
            TypeMismatch {
                expected: Boolean,
                got: Integer
            }
        );

        bad_input_test_at_parsing!(boucle_2);
    }
}
