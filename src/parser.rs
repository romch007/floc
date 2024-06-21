use crate::ast::*;
use lazy_static::lazy_static;
use pest::{iterators::Pair, pratt_parser::PrattParser, Parser};
use pest_derive::Parser;

lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use crate::parser::Rule;
        use pest::pratt_parser::Assoc::Left;
        use pest::pratt_parser::Op;

        PrattParser::new()
            .op(Op::infix(Rule::logic_or, Left) | Op::infix(Rule::logic_and, Left))
            .op(Op::prefix(Rule::logic_not))
            .op(Op::infix(Rule::r#eq, Left) | Op::infix(Rule::neq, Left))
            .op(Op::infix(Rule::lt, Left)
                | Op::infix(Rule::lte, Left)
                | Op::infix(Rule::gt, Left)
                | Op::infix(Rule::gte, Left))
            .op(Op::infix(Rule::add, Left) | Op::infix(Rule::sub, Left))
            .op(Op::infix(Rule::mul, Left)
                | Op::infix(Rule::div, Left)
                | Op::infix(Rule::r#mod, Left))
            .op(Op::prefix(Rule::neg))
    };
}

#[derive(Parser)]
#[grammar = "src/grammar.pest"]
struct PestParser;

fn parse_bool(input: &str) -> Option<bool> {
    match input {
        "Vrai" => Some(true),
        "Faux" => Some(false),
        _ => None,
    }
}

trait Node {
    fn parse(pair: Pair<Rule>) -> Self;
}

impl Node for Type {
    fn parse(pair: Pair<Rule>) -> Self {
        match pair.as_str() {
            "entier" => Self::Integer,
            "booleen" => Self::Boolean,
            pair => unreachable!("invalid type '{pair}'"),
        }
    }
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

impl Node for BinaryOpKind {
    fn parse(pair: Pair<Rule>) -> Self {
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
            Rule::logic_or => Self::LogicOr,
            Rule::logic_and => Self::LogicAnd,
            rule => unreachable!("invalid binary op {rule:?}"),
        }
    }
}

impl Node for UnaryOpKind {
    fn parse(pair: Pair<Rule>) -> Self {
        match pair.as_rule() {
            Rule::neg => Self::Neg,
            Rule::logic_not => Self::LogicNot,
            rule => unreachable!("invalid unary op {rule:?}"),
        }
    }
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
                rule => unreachable!("invalid primary expr '{rule:?}'"),
            })
            .map_prefix(|op, rhs| {
                Self::UnaryOp(UnaryOp {
                    kind: UnaryOpKind::parse(op),
                    operand: Box::new(rhs),
                })
            })
            .map_infix(|lhs, op, rhs| {
                Self::BinaryOp(BinaryOp {
                    left: Box::new(lhs),
                    kind: BinaryOpKind::parse(op),
                    right: Box::new(rhs),
                })
            })
            .parse(pairs)
    }
}

impl Node for Assignment {
    fn parse(pair: Pair<Rule>) -> Self {
        let mut pairs = pair.into_inner();

        let variable = pairs.next().unwrap().as_str().to_string();
        let value = Expression::parse(pairs.next().unwrap());

        Self { variable, value }
    }
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
        let mut statements_else = None;

        let next_pair = pairs.peek().map(|pair| pair.as_rule());

        match next_pair {
            Some(Rule::stmt_list) => {
                statements_else = Some(
                    pairs
                        .next()
                        .unwrap()
                        .into_inner()
                        .map(Statement::parse)
                        .collect(),
                );
            }
            Some(Rule::r#if) => {
                statements_else = Some(vec![Statement::If(If::parse(pairs.next().unwrap()))]);
            }
            _ => {}
        };

        Self {
            condition,
            statements,
            statements_else,
        }
    }
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
            Rule::discard_fn_call => {
                Self::DiscardFunctionCall(FunctionCall::parse(pair.into_inner().next().unwrap()))
            }
            rule => unreachable!("invalid statement '{rule:?}'"),
        }
    }
}

impl Node for Argument {
    fn parse(pair: Pair<Rule>) -> Self {
        let mut pairs = pair.into_inner();

        let r#type = Type::parse(pairs.next().unwrap());
        let name = pairs.next().unwrap().as_str().to_string();

        Self { r#type, name }
    }
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

pub fn parse(source: &str) -> Result<Program, Box<pest::error::Error<Rule>>> {
    let mut pest_output = PestParser::parse(Rule::prog, source).map_err(Box::new)?;

    let program = Program::parse(pest_output.next().unwrap());

    Ok(program)
}
