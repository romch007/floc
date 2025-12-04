use floc::ast::{Expression, Statement, visitor::Visitor};

pub struct Finder<'a> {
    pub offset: usize,
    pub found_expr: Option<&'a Expression>,
    pub found_stmt: Option<&'a Statement>,
}

impl<'a> Finder<'a> {
    pub fn new(offset: usize) -> Self {
        Self {
            offset,
            found_expr: None,
            found_stmt: None,
        }
    }

    fn update_expr(&mut self, expr: &'a Expression) {
        if expr.span().contains(self.offset) {
            if let Some(current) = &self.found_expr {
                if expr.span().len() < current.span().len() {
                    self.found_expr = Some(expr);
                }
            } else {
                self.found_expr = Some(expr)
            }
        }
    }

    fn update_stmt(&mut self, stmt: &'a Statement) {
        if stmt.span().contains(self.offset) {
            if let Some(current) = &self.found_stmt {
                if stmt.span().len() < current.span().len() {
                    self.found_stmt = Some(stmt);
                }
            } else {
                self.found_stmt = Some(stmt)
            }
        }
    }
}

impl<'a> Visitor<'a> for Finder<'a> {
    fn visit_expression_pre(&mut self, expr: &'a Expression) {
        self.update_expr(expr);
    }

    fn visit_statement_pre(&mut self, stmt: &'a Statement) {
        self.update_stmt(stmt);
    }
}
