pub mod dot;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

pub trait SpanIterExt {
    /// Merges a sequence of `Span` references into a single `Span`.
    ///
    /// The method computes a new `Span` that spans from the start of the first `Span`
    /// to the end of the last `Span` in the iterator. If the iterator contains only one
    /// `Span`, it returns that `Span` directly. If the iterator is empty, it returns `None`.
    fn merge_spans(self) -> Option<Span>;
}

impl<'a, T> SpanIterExt for T
where
    T: Iterator<Item = &'a Span>,
{
    fn merge_spans(mut self) -> Option<Span> {
        if let Some(first) = self.next() {
            if let Some(last) = self.last() {
                Some(Span {
                    start: first.start,
                    end: last.end,
                })
            } else {
                // Only one span, return it
                Some(*first)
            }
        } else {
            // No spans
            None
        }
    }
}

impl<'a> From<pest::Span<'a>> for Span {
    fn from(value: pest::Span<'a>) -> Self {
        Self {
            start: value.start(),
            end: value.end(),
        }
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(value: Span) -> Self {
        (value.start, value.end - value.start).into()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub ident: String,
    pub span: Span,
}

impl std::ops::Deref for Identifier {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.ident
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    Integer,
    Boolean,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

impl std::fmt::Display for TypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            TypeKind::Integer => "entier",
            TypeKind::Boolean => "booleen",
        };

        write!(f, "{name}")?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct FunctionCall {
    pub name: Identifier,
    pub arguments: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    LogicAnd,
    LogicOr,
}

#[derive(Debug)]
pub struct BinaryOp {
    pub left: Box<Expression>,
    pub kind: BinaryOpKind,
    pub right: Box<Expression>,
    pub span: Span,
    pub operator_span: Span,
}

#[derive(Debug)]
pub enum UnaryOpKind {
    Neg,
    LogicNot,
}

#[derive(Debug)]
pub struct UnaryOp {
    pub kind: UnaryOpKind,
    pub operand: Box<Expression>,
    pub span: Span,
    pub operator_span: Span,
}

#[derive(Debug)]
pub enum Expression {
    Integer(u64, Span),
    Variable(Identifier),
    Boolean(bool, Span),
    Read(Span),
    FunctionCall(FunctionCall),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
}

impl Expression {
    pub fn span(&self) -> &Span {
        match self {
            Expression::Integer(_, span) => span,
            Expression::Variable(ident) => &ident.span,
            Expression::Boolean(_, span) => span,
            Expression::Read(span) => span,
            Expression::FunctionCall(fn_call) => &fn_call.span,
            Expression::BinaryOp(binary_op) => &binary_op.span,
            Expression::UnaryOp(unary_op) => &unary_op.span,
        }
    }
}

#[derive(Debug)]
pub struct Assignment {
    pub variable: Identifier,
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug)]
pub struct Declaration {
    pub r#type: Type,
    pub variable: Identifier,
    pub value: Option<Expression>,
    pub span: Span,
}

#[derive(Debug)]
pub struct While {
    pub condition: Expression,
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug)]
pub struct If {
    pub condition: Expression,
    pub statements: Vec<Statement>,
    pub statements_else: Option<Vec<Statement>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Write {
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug)]
pub struct Return {
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug)]
pub enum Statement {
    Assignment(Assignment),
    Declaration(Declaration),
    Write(Write),
    Return(Return),
    While(While),
    If(If),
    DiscardFunctionCall(FunctionCall),
}

impl Statement {
    pub fn span(&self) -> &Span {
        match self {
            Statement::Assignment(assign) => &assign.span,
            Statement::Declaration(decl) => &decl.span,
            Statement::Write(write) => &write.span,
            Statement::Return(ret) => &ret.span,
            Statement::While(whil) => &whil.span,
            Statement::If(i) => &i.span,
            Statement::DiscardFunctionCall(fn_call) => &fn_call.span,
        }
    }
}

#[derive(Debug)]
pub struct Argument {
    pub r#type: Type,
    pub name: Identifier,
    pub span: Span,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub return_type: Type,
    pub name: Identifier,
    pub arguments: Vec<Argument>,
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Program {
    pub function_decls: Vec<FunctionDeclaration>,
    pub statements: Vec<Statement>,
}

#[cfg(test)]
mod tests {
    use super::{Span, SpanIterExt};

    #[test]
    fn merge_spans_multiple() {
        let spans = vec![
            Span { start: 1, end: 5 },
            Span { start: 6, end: 10 },
            Span { start: 11, end: 15 },
        ];

        let merged_span = spans.iter().merge_spans();
        assert_eq!(merged_span, Some(Span { start: 1, end: 15 }));
    }

    #[test]
    fn merge_spans_single() {
        let spans = vec![Span { start: 3, end: 7 }];

        let merged_span = spans.iter().merge_spans();
        assert_eq!(merged_span, Some(Span { start: 3, end: 7 }));
    }

    #[test]
    fn merge_spans_empty() {
        let spans: Vec<Span> = vec![];

        let merged_span = spans.iter().merge_spans();
        assert_eq!(merged_span, None);
    }
}
