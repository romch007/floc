use crate::ast::*;

pub trait Visitor<'a> {
    fn visit_program(&mut self, program: &'a Program) {
        for func in &program.function_decls {
            self.visit_function_decl(func);
        }
        for stmt in &program.statements {
            self.visit_statement(stmt);
        }
    }

    fn visit_function_decl(&mut self, func: &'a FunctionDeclaration) {
        for arg in &func.arguments {
            self.visit_argument(arg);
        }
        for stmt in &func.statements {
            self.visit_statement(stmt);
        }
    }

    fn visit_argument(&mut self, _arg: &'a Argument) {}

    fn visit_statement_pre(&mut self, _stmt: &'a Statement) {}

    fn visit_statement(&mut self, stmt: &'a Statement) {
        self.visit_statement_pre(stmt);

        match stmt {
            Statement::Assignment(assign) => self.visit_assignment(assign),
            Statement::Declaration(decl) => self.visit_declaration(decl),
            Statement::Write(write) => self.visit_write(write),
            Statement::Return(ret) => self.visit_return(ret),
            Statement::While(whil) => self.visit_while(whil),
            Statement::If(if_stmt) => self.visit_if(if_stmt),
            Statement::DiscardFunctionCall(fn_call) => self.visit_function_call(fn_call),
        }
    }

    fn visit_assignment(&mut self, assign: &'a Assignment) {
        self.visit_expression(&assign.value);
    }

    fn visit_declaration(&mut self, decl: &'a Declaration) {
        if let Some(value) = &decl.value {
            self.visit_expression(value);
        }
    }

    fn visit_write(&mut self, write: &'a Write) {
        self.visit_expression(&write.value);
    }

    fn visit_return(&mut self, ret: &'a Return) {
        self.visit_expression(&ret.value);
    }

    fn visit_while(&mut self, whil: &'a While) {
        self.visit_expression(&whil.condition);
        for stmt in &whil.statements {
            self.visit_statement(stmt);
        }
    }

    fn visit_if(&mut self, if_stmt: &'a If) {
        self.visit_expression(&if_stmt.condition);
        for stmt in &if_stmt.statements {
            self.visit_statement(stmt);
        }
        if let Some(else_block) = &if_stmt.statements_else {
            for stmt in else_block {
                self.visit_statement(stmt);
            }
        }
    }

    fn visit_expression_pre(&mut self, expr: &'a Expression) {}

    fn visit_expression(&mut self, expr: &'a Expression) {
        self.visit_expression_pre(expr);

        match expr {
            Expression::Integer(value, span) => self.visit_integer(*value, *span),
            Expression::Boolean(value, span) => self.visit_boolean(*value, *span),
            Expression::Read(span) => self.visit_read(*span),
            Expression::Variable(ident) => self.visit_variable(ident),
            Expression::FunctionCall(fn_call) => self.visit_function_call(fn_call),
            Expression::BinaryOp(bin) => {
                self.visit_expression(&bin.left);
                self.visit_expression(&bin.right);
            }
            Expression::UnaryOp(un) => self.visit_expression(&un.operand),
        }
    }

    fn visit_integer(&mut self, _value: u64, _span: Span) {}

    fn visit_boolean(&mut self, _value: bool, _span: Span) {}

    fn visit_read(&mut self, _span: Span) {}

    fn visit_variable(&mut self, _var: &'a Identifier) {}

    fn visit_function_call(&mut self, fn_call: &'a FunctionCall) {
        for arg in &fn_call.arguments {
            self.visit_expression(arg);
        }
    }
}
