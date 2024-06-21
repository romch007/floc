#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integer,
    Boolean,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Self::Integer => "entier",
            Self::Boolean => "booleen",
        };

        write!(f, "{name}")?;

        Ok(())
    }
}

#[derive(Debug)]
pub struct FunctionCall {
    pub name: String,
    pub arguments: Vec<Expression>,
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
pub enum UnaryOpKind {
    Neg,
    LogicNot,
}

#[derive(Debug)]
pub enum Expression {
    Integer(u64),
    Variable(String),
    Boolean(bool),
    Read,
    Random {
        max: Box<Expression>,
    },
    FunctionCall(FunctionCall),
    BinaryOp {
        left: Box<Expression>,
        op: BinaryOpKind,
        right: Box<Expression>,
    },
    UnaryOp {
        op: UnaryOpKind,
        operand: Box<Expression>,
    },
}

#[derive(Debug)]
pub struct Assignment {
    pub variable: String,
    pub value: Expression,
}

#[derive(Debug)]
pub struct Declaration {
    pub r#type: Type,
    pub variable: String,
    pub value: Option<Expression>,
}

#[derive(Debug)]
pub struct While {
    pub condition: Expression,
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct If {
    pub condition: Expression,
    pub statements: Vec<Statement>,
    pub statements_else: Option<Vec<Statement>>,
}

#[derive(Debug)]
pub enum Statement {
    Assignment(Assignment),
    Declaration(Declaration),
    Write { value: Expression },
    Return { value: Expression },
    While(While),
    If(If),
    DiscardFunctionCall(FunctionCall),
}

#[derive(Debug)]
pub struct Argument {
    pub r#type: Type,
    pub name: String,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub return_type: Type,
    pub name: String,
    pub arguments: Vec<Argument>,
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct Program {
    pub function_decls: Vec<FunctionDeclaration>,
    pub statements: Vec<Statement>,
}
