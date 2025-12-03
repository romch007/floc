pub mod dot;
pub mod visitor;

use crate::span::Span;

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
