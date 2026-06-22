use std::collections::HashMap;

use floc_ast::{Expression, Identifier, Program, Statement};
use floc_span::Span;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum RefKind {
    Read,
    Write,
}

pub struct Reference {
    pub binding: usize,
    pub span: Span,
    pub kind: RefKind,
}

/// All variable occurrences in a program, each tagged with the binding it
/// resolves to. Scoping mirrors the analyzer: one scope per function (sharing
/// the body block with its arguments), nested scopes for `if`/`while` bodies and
/// the top-level statements, with inner declarations shadowing outer ones.
pub struct References {
    refs: Vec<Reference>,
}

impl References {
    #[must_use]
    pub fn collect(program: &Program) -> Self {
        let mut collector = Collector {
            scopes: Vec::new(),
            unresolved: HashMap::new(),
            next_binding: 0,
            refs: Vec::new(),
        };

        collector.walk_program(program);

        Self {
            refs: collector.refs,
        }
    }

    /// Every occurrence resolving to the same binding as the occurrence under
    /// `offset`, or empty if the cursor is not on a variable.
    #[must_use]
    pub fn at_offset(&self, offset: usize) -> Vec<&Reference> {
        let Some(target) = self.refs.iter().find(|r| r.span.contains(offset)) else {
            return Vec::new();
        };

        self.refs
            .iter()
            .filter(|r| r.binding == target.binding)
            .collect()
    }
}

struct Collector {
    scopes: Vec<HashMap<String, usize>>,
    unresolved: HashMap<String, usize>,
    next_binding: usize,
    refs: Vec<Reference>,
}

impl Collector {
    fn fresh(&mut self) -> usize {
        let id = self.next_binding;
        self.next_binding += 1;
        id
    }

    fn enter(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn leave(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, ident: &Identifier) {
        let binding = self.fresh();
        self.scopes
            .last_mut()
            .unwrap()
            .insert(ident.ident.clone(), binding);
        self.refs.push(Reference {
            binding,
            span: ident.span,
            kind: RefKind::Write,
        });
    }

    fn resolve(&mut self, name: &str) -> usize {
        if let Some(binding) = self.scopes.iter().rev().find_map(|scope| scope.get(name)) {
            return *binding;
        }

        // Undeclared names are grouped by name so identical typos still light up.
        if let Some(binding) = self.unresolved.get(name) {
            return *binding;
        }

        let binding = self.fresh();
        self.unresolved.insert(name.to_string(), binding);
        binding
    }

    fn reference(&mut self, ident: &Identifier, kind: RefKind) {
        let binding = self.resolve(&ident.ident);
        self.refs.push(Reference {
            binding,
            span: ident.span,
            kind,
        });
    }

    fn walk_program(&mut self, program: &Program) {
        for func in &program.function_decls {
            self.enter();
            for arg in &func.arguments {
                self.declare(&arg.name);
            }
            for stmt in &func.statements {
                self.walk_stmt(stmt);
            }
            self.leave();
        }

        self.enter();
        for stmt in &program.statements {
            self.walk_stmt(stmt);
        }
        self.leave();
    }

    fn walk_block(&mut self, stmts: &[Statement]) {
        self.enter();
        for stmt in stmts {
            self.walk_stmt(stmt);
        }
        self.leave();
    }

    fn walk_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Declaration(decl) => {
                // The value is evaluated before the variable enters scope.
                if let Some(value) = &decl.value {
                    self.walk_expr(value);
                }
                self.declare(&decl.variable);
            }
            Statement::Assignment(assign) => {
                self.reference(&assign.variable, RefKind::Write);
                self.walk_expr(&assign.value);
            }
            Statement::Write(write) => self.walk_expr(&write.value),
            Statement::Return(ret) => self.walk_expr(&ret.value),
            Statement::While(whil) => {
                self.walk_expr(&whil.condition);
                self.walk_block(&whil.statements);
            }
            Statement::If(if_stmt) => {
                self.walk_expr(&if_stmt.condition);
                self.walk_block(&if_stmt.statements);
                if let Some(else_block) = &if_stmt.statements_else {
                    self.walk_block(else_block);
                }
            }
            Statement::DiscardFunctionCall(fn_call) => {
                for arg in &fn_call.arguments {
                    self.walk_expr(arg);
                }
            }
            Statement::Error(_) => {}
        }
    }

    fn walk_expr(&mut self, expr: &Expression) {
        match expr {
            Expression::Variable(ident) => self.reference(ident, RefKind::Read),
            Expression::FunctionCall(fn_call) => {
                for arg in &fn_call.arguments {
                    self.walk_expr(arg);
                }
            }
            Expression::BinaryOp(bin) => {
                self.walk_expr(&bin.left);
                self.walk_expr(&bin.right);
            }
            Expression::UnaryOp(un) => self.walk_expr(&un.operand),
            Expression::Integer(..)
            | Expression::Boolean(..)
            | Expression::Read(_)
            | Expression::Error(_) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use miette::NamedSource;

    fn references(source: &str) -> References {
        let program = floc_parser::parse(NamedSource::new("test.flo", source.to_string())).unwrap();
        References::collect(&program)
    }

    /// Byte offset of the `nth` (0-based) occurrence of `needle` in `source`.
    fn offset_of(source: &str, needle: &str, nth: usize) -> usize {
        source.match_indices(needle).nth(nth).unwrap().0
    }

    #[test]
    fn highlights_all_uses_of_a_variable() {
        let source = "entier x = 1;\nx = x + 2;\necrire(x);\n";
        let refs = references(source);

        let hits = refs.at_offset(offset_of(source, "x", 0));
        // declaration + assignment target + use in `x + 2` + use in `ecrire`
        assert_eq!(hits.len(), 4);
    }

    #[test]
    fn shadowed_variable_is_a_distinct_binding() {
        let source = "entier x = 1;\ntantque (Vrai) {\n  entier x = 2;\n  ecrire(x);\n}\necrire(x);\n";
        let refs = references(source);

        // Inner `x`: its declaration + the `ecrire(x)` inside the loop.
        let inner = refs.at_offset(offset_of(source, "entier x = 2", 0) + "entier ".len());
        assert_eq!(inner.len(), 2);

        // Outer `x`: declaration + the final `ecrire(x)`, not the shadowed ones.
        let outer = refs.at_offset(offset_of(source, "x", 0));
        assert_eq!(outer.len(), 2);
    }

    #[test]
    fn same_name_in_different_functions_does_not_merge() {
        let source =
            "entier f(entier a) { retourner a; }\nentier g(entier a) { retourner a; }\necrire(1);\n";
        let refs = references(source);

        // First function's `a`: the parameter + the single use.
        let hits = refs.at_offset(offset_of(source, "a", 0));
        assert_eq!(hits.len(), 2);
    }
}
