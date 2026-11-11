pub mod dot;
pub mod visitor;

use floc_span::Span;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub ident: String,
    pub span: Span,
}

impl std::ops::Deref for Identifier {
    type Target = String;

    #[inline]
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
    // Poison node emitted by parser error recovery.
    Error(Span),
}

impl Expression {
    #[inline]
    #[must_use]
    pub fn span(&self) -> &Span {
        match self {
            Expression::Integer(_, span) => span,
            Expression::Variable(ident) => &ident.span,
            Expression::Boolean(_, span) => span,
            Expression::Read(span) => span,
            Expression::FunctionCall(fn_call) => &fn_call.span,
            Expression::BinaryOp(binary_op) => &binary_op.span,
            Expression::UnaryOp(unary_op) => &unary_op.span,
            Expression::Error(span) => span,
        }
    }

    /// Value of a boolean expression known at compile time, following the
    /// runtime semantics (short-circuiting `et`/`ou`).
    #[must_use]
    pub fn const_bool(&self) -> Option<bool> {
        match self {
            Expression::Boolean(value, _) => Some(*value),
            Expression::UnaryOp(UnaryOp {
                kind: UnaryOpKind::LogicNot,
                operand,
                ..
            }) => operand.const_bool().map(|value| !value),
            Expression::BinaryOp(BinaryOp {
                left, kind, right, ..
            }) => match kind {
                BinaryOpKind::LogicAnd => match left.const_bool()? {
                    false => Some(false),
                    true => right.const_bool(),
                },
                BinaryOpKind::LogicOr => match left.const_bool()? {
                    true => Some(true),
                    false => right.const_bool(),
                },
                BinaryOpKind::Eq | BinaryOpKind::Neq => {
                    let equal = match (left.const_bool(), right.const_bool()) {
                        (Some(left), Some(right)) => left == right,
                        _ => left.const_int()? == right.const_int()?,
                    };
                    Some(equal == matches!(kind, BinaryOpKind::Eq))
                }
                BinaryOpKind::Lt => Some(left.const_int()? < right.const_int()?),
                BinaryOpKind::Lte => Some(left.const_int()? <= right.const_int()?),
                BinaryOpKind::Gt => Some(left.const_int()? > right.const_int()?),
                BinaryOpKind::Gte => Some(left.const_int()? >= right.const_int()?),
                _ => None,
            },
            _ => None,
        }
    }

    /// Value of an integer expression known at compile time, wrapping like the
    /// generated code. `None` for division by zero, which traps at runtime.
    #[must_use]
    pub fn const_int(&self) -> Option<i64> {
        match self {
            #[allow(clippy::cast_possible_wrap)]
            Expression::Integer(value, _) => Some(*value as i64),
            Expression::UnaryOp(UnaryOp {
                kind: UnaryOpKind::Neg,
                operand,
                ..
            }) => operand.const_int().map(i64::wrapping_neg),
            Expression::BinaryOp(BinaryOp {
                left, kind, right, ..
            }) => {
                let (left, right) = (left.const_int()?, right.const_int()?);
                match kind {
                    BinaryOpKind::Add => Some(left.wrapping_add(right)),
                    BinaryOpKind::Sub => Some(left.wrapping_sub(right)),
                    BinaryOpKind::Mul => Some(left.wrapping_mul(right)),
                    BinaryOpKind::Div if right != 0 => Some(left.wrapping_div(right)),
                    BinaryOpKind::Mod if right != 0 => Some(left.wrapping_rem(right)),
                    _ => None,
                }
            }
            _ => None,
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
    // Poison node emitted by parser error recovery.
    Error(Span),
}

impl Statement {
    #[inline]
    #[must_use]
    pub fn span(&self) -> &Span {
        match self {
            Statement::Assignment(assign) => &assign.span,
            Statement::Declaration(decl) => &decl.span,
            Statement::Write(write) => &write.span,
            Statement::Return(ret) => &ret.span,
            Statement::While(whil) => &whil.span,
            Statement::If(i) => &i.span,
            Statement::DiscardFunctionCall(fn_call) => &fn_call.span,
            Statement::Error(span) => span,
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
