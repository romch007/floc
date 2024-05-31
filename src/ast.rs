use crate::parser::Rule;
use lazy_static::lazy_static;
use pest::{iterators::Pair, pratt_parser::PrattParser};

lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use crate::parser::Rule;
        use pest::pratt_parser::Assoc::*;
        use pest::pratt_parser::Op;

        PrattParser::new()
            .op(Op::infix(Rule::r#eq, Left) | Op::infix(Rule::neq, Left))
            .op(Op::infix(Rule::lt, Left)
                | Op::infix(Rule::lte, Left)
                | Op::infix(Rule::gt, Left)
                | Op::infix(Rule::gte, Left))
            .op(Op::infix(Rule::add, Left) | Op::infix(Rule::sub, Left))
            .op(Op::infix(Rule::mul, Left)
                | Op::infix(Rule::div, Left)
                | Op::infix(Rule::r#mod, Left))
            .op(Op::prefix(Rule::neg) | Op::prefix(Rule::not))
    };
}

fn parse_bool(input: &str) -> Option<bool> {
    match input {
        "Vrai" => Some(true),
        "Faux" => Some(false),
        _ => None,
    }
}

pub trait Node {
    fn parse(pair: Pair<Rule>) -> Self;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Integer,
    Boolean,
}

impl Node for Type {
    fn parse(pair: Pair<Rule>) -> Self {
        match pair.as_str() {
            "entier" => Self::Integer,
            "booleen" => Self::Boolean,
            _ => unreachable!("invalid type '{}'", pair.as_str()),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Self::Integer => "integer",
            Self::Boolean => "boolean",
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

impl Node for FunctionCall {
    fn parse(pair: Pair<Rule>) -> Self {
        let mut pairs = pair.into_inner();

        let name = pairs.next().unwrap().as_str().to_string();
        let arguments = pairs
            .next()
            .unwrap()
            .into_inner()
            .map(Expression::parse)
            .collect();

        Self { name, arguments }
    }
}

#[derive(Debug)]
pub enum BinaryOpType {
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
}

impl BinaryOpType {
    pub fn from_pair(pair: Pair<Rule>) -> Self {
        match pair.as_rule() {
            Rule::add => Self::Add,
            Rule::sub => Self::Sub,
            Rule::mul => Self::Mul,
            Rule::div => Self::Div,
            Rule::r#mod => Self::Mod,
            Rule::r#eq => Self::Eq,
            Rule::neq => Self::Neq,
            Rule::lt => Self::Lt,
            Rule::lte => Self::Lte,
            Rule::gt => Self::Gt,
            Rule::gte => Self::Gte,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum UnaryOpType {
    Neg,
    Not,
}

impl UnaryOpType {
    pub fn from_pair(pair: Pair<Rule>) -> Self {
        match pair.as_rule() {
            Rule::neg => Self::Neg,
            Rule::not => Self::Not,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Integer(u32),
    Variable(String),
    Boolean(bool),
    Read,
    FunctionCall(FunctionCall),
    BinaryOp {
        left: Box<Expression>,
        op: BinaryOpType,
        right: Box<Expression>,
    },
    UnaryOp {
        op: UnaryOpType,
        operand: Box<Expression>,
    },
}

impl Node for Expression {
    fn parse(pair: Pair<Rule>) -> Self {
        let pairs = pair.into_inner();

        PRATT_PARSER
            .map_primary(|primary| match primary.as_rule() {
                Rule::expr => Expression::parse(primary), // from "(" ~ expr ~ ")"
                Rule::integer => Self::Integer(primary.as_str().parse().unwrap()),
                Rule::ident => Self::Variable(primary.as_str().to_string()),
                Rule::boolean => Self::Boolean(parse_bool(primary.as_str()).unwrap()),
                Rule::read => Self::Read,
                Rule::function_call => Self::FunctionCall(FunctionCall::parse(primary)),
                _ => unreachable!("invalid primary expr '{:?}'", primary.as_rule()),
            })
            .map_prefix(|op, rhs| Self::UnaryOp {
                op: UnaryOpType::from_pair(op),
                operand: Box::new(rhs),
            })
            .map_infix(|lhs, op, rhs| Self::BinaryOp {
                left: Box::new(lhs),
                op: BinaryOpType::from_pair(op),
                right: Box::new(rhs),
            })
            .parse(pairs)
    }
}

#[derive(Debug)]
pub struct Assignment {
    pub variable: String,
    pub value: Expression,
}

impl Node for Assignment {
    fn parse(pair: Pair<Rule>) -> Self {
        let mut pairs = pair.into_inner();

        let variable = pairs.next().unwrap().as_str().to_string();
        let value = Expression::parse(pairs.next().unwrap());

        Self { variable, value }
    }
}

#[derive(Debug)]
pub struct Declaration {
    pub r#type: Type,
    pub variable: String,
    pub value: Option<Expression>,
}

impl Node for Declaration {
    fn parse(pair: Pair<Rule>) -> Self {
        let mut pairs = pair.into_inner();

        let r#type = Type::parse(pairs.next().unwrap());
        let variable = pairs.next().unwrap().as_str().to_string();
        let mut value = None;

        if matches!(pairs.peek().map(|pair| pair.as_rule()), Some(Rule::expr)) {
            value = Some(Expression::parse(pairs.next().unwrap()));
        }

        Self {
            r#type,
            variable,
            value,
        }
    }
}

#[derive(Debug)]
pub struct While {
    pub condition: Expression,
    pub statements: Vec<Statement>,
}

impl Node for While {
    fn parse(pair: Pair<Rule>) -> Self {
        let mut pairs = pair.into_inner();

        let condition = Expression::parse(pairs.next().unwrap());
        let statements = pairs
            .next()
            .unwrap()
            .into_inner()
            .map(Statement::parse)
            .collect();

        Self {
            condition,
            statements,
        }
    }
}

#[derive(Debug)]
pub struct If {
    pub condition: Expression,
    pub statements: Vec<Statement>,
}

impl Node for If {
    fn parse(pair: Pair<Rule>) -> Self {
        let mut pairs = pair.into_inner();

        let condition = Expression::parse(pairs.next().unwrap());
        let statements = pairs
            .next()
            .unwrap()
            .into_inner()
            .map(Statement::parse)
            .collect();

        Self {
            condition,
            statements,
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Assignment(Assignment),
    Declaration(Declaration),
    Write { value: Expression },
    Return { value: Expression },
    While(While),
    If(If),
}

impl Node for Statement {
    fn parse(pair: Pair<Rule>) -> Self {
        match pair.as_rule() {
            Rule::assignment => Self::Assignment(Assignment::parse(pair)),
            Rule::declaration => Self::Declaration(Declaration::parse(pair)),
            Rule::write => Self::Write {
                value: Expression::parse(pair.into_inner().next().unwrap()),
            },
            Rule::r#return => Self::Return {
                value: Expression::parse(pair.into_inner().next().unwrap()),
            },
            Rule::r#while => Self::While(While::parse(pair)),
            Rule::r#if => Self::If(If::parse(pair)),
            _ => unreachable!("invalid statement '{:?}'", pair.as_rule()),
        }
    }
}

#[derive(Debug)]
pub struct Argument {
    pub r#type: Type,
    pub name: String,
}

impl Node for Argument {
    fn parse(pair: Pair<Rule>) -> Self {
        let mut pairs = pair.into_inner();

        let r#type = Type::parse(pairs.next().unwrap());
        let name = pairs.next().unwrap().as_str().to_string();

        Self { r#type, name }
    }
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub return_type: Type,
    pub name: String,
    pub arguments: Vec<Argument>,
    pub statements: Vec<Statement>,
}

impl Node for FunctionDeclaration {
    fn parse(pair: Pair<Rule>) -> Self {
        let mut pairs = pair.into_inner();

        let return_type = Type::parse(pairs.next().unwrap());
        let name = pairs.next().unwrap().as_str().to_string();
        let arguments = pairs
            .next()
            .unwrap()
            .into_inner()
            .map(Argument::parse)
            .collect();
        let statements = pairs
            .next()
            .unwrap()
            .into_inner()
            .map(Statement::parse)
            .collect();

        Self {
            return_type,
            name,
            arguments,
            statements,
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub function_decls: Vec<FunctionDeclaration>,
    pub statements: Vec<Statement>,
}

impl Node for Program {
    fn parse(pair: Pair<Rule>) -> Self {
        let mut pairs = pair.into_inner();

        let function_decls = pairs
            .next()
            .unwrap()
            .into_inner()
            .map(FunctionDeclaration::parse)
            .collect();
        let statements = pairs
            .next()
            .unwrap()
            .into_inner()
            .map(Statement::parse)
            .collect();

        Self {
            function_decls,
            statements,
        }
    }
}
