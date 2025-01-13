use crate::ast::*;
use miette::Context;
use pest::{iterators::Pair, pratt_parser::PrattParser, Parser};
use pest_derive::Parser;
use std::sync::OnceLock;

static PRATT_PARSER: OnceLock<PrattParser<Rule>> = OnceLock::new();

fn initialize_pratt_parser() -> PrattParser<Rule> {
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
        .op(Op::infix(Rule::mul, Left) | Op::infix(Rule::div, Left) | Op::infix(Rule::r#mod, Left))
        .op(Op::prefix(Rule::neg))
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

impl Node for Identifier {
    fn parse(pair: Pair<Rule>) -> Self {
        let span = pair.as_span().into();
        let name = pair.as_str().to_string();

        Self { ident: name, span }
    }
}

impl Node for Type {
    fn parse(pair: Pair<Rule>) -> Self {
        let span = pair.as_span().into();

        let kind = match pair.as_str() {
            "entier" => TypeKind::Integer,
            "booleen" => TypeKind::Boolean,
            pair => unreachable!("invalid type '{pair}'"),
        };

        Self { kind, span }
    }
}

impl Node for FunctionCall {
    fn parse(pair: Pair<Rule>) -> Self {
        let span = pair.as_span().into();
        let mut pairs = pair.into_inner();

        let name = Identifier::parse(pairs.next().unwrap());
        let arguments = pairs
            .next()
            .unwrap()
            .into_inner()
            .map(Expression::parse)
            .collect();

        Self {
            name,
            arguments,
            span,
        }
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
            .get_or_init(initialize_pratt_parser)
            .map_primary(|primary| match primary.as_rule() {
                Rule::expr => Expression::parse(primary), // from "(" ~ expr ~ ")"
                Rule::integer => {
                    Self::Integer(primary.as_str().parse().unwrap(), primary.as_span().into())
                }
                Rule::ident => Self::Variable(Identifier::parse(primary)),
                Rule::boolean => Self::Boolean(
                    parse_bool(primary.as_str()).unwrap(),
                    primary.as_span().into(),
                ),
                Rule::read => Self::Read(primary.as_span().into()),
                Rule::function_call => Self::FunctionCall(FunctionCall::parse(primary)),
                rule => unreachable!("invalid primary expr '{rule:?}'"),
            })
            .map_prefix(|op, rhs| {
                let span = op.as_span().into();

                Self::UnaryOp(UnaryOp {
                    kind: UnaryOpKind::parse(op),
                    operand: Box::new(rhs),
                    span,
                })
            })
            .map_infix(|lhs, op, rhs| {
                let span = op.as_span().into();

                Self::BinaryOp(BinaryOp {
                    left: Box::new(lhs),
                    kind: BinaryOpKind::parse(op),
                    right: Box::new(rhs),
                    span,
                })
            })
            .parse(pairs)
    }
}

impl Node for Assignment {
    fn parse(pair: Pair<Rule>) -> Self {
        let span = pair.as_span().into();
        let mut pairs = pair.into_inner();

        let variable = Identifier::parse(pairs.next().unwrap());
        let value = Expression::parse(pairs.next().unwrap());

        Self {
            variable,
            value,
            span,
        }
    }
}

impl Node for Declaration {
    fn parse(pair: Pair<Rule>) -> Self {
        let span = pair.as_span().into();
        let mut pairs = pair.into_inner();

        let r#type = Type::parse(pairs.next().unwrap());
        let variable = Identifier::parse(pairs.next().unwrap());
        let mut value = None;

        if matches!(pairs.peek().map(|pair| pair.as_rule()), Some(Rule::expr)) {
            value = Some(Expression::parse(pairs.next().unwrap()));
        }

        Self {
            r#type,
            variable,
            value,
            span,
        }
    }
}

impl Node for While {
    fn parse(pair: Pair<Rule>) -> Self {
        let span = pair.as_span().into();
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
            span,
        }
    }
}

impl Node for If {
    fn parse(pair: Pair<Rule>) -> Self {
        let span = pair.as_span().into();
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
            span,
        }
    }
}

impl Node for Write {
    fn parse(pair: Pair<Rule>) -> Self {
        let span = pair.as_span().into();
        let value = Expression::parse(pair.into_inner().next().unwrap());

        Self { value, span }
    }
}

impl Node for Return {
    fn parse(pair: Pair<Rule>) -> Self {
        let span = pair.as_span().into();
        let value = Expression::parse(pair.into_inner().next().unwrap());

        Self { value, span }
    }
}

impl Node for Statement {
    fn parse(pair: Pair<Rule>) -> Self {
        match pair.as_rule() {
            Rule::assignment => Self::Assignment(Assignment::parse(pair)),
            Rule::declaration => Self::Declaration(Declaration::parse(pair)),
            Rule::write => Self::Write(Write::parse(pair)),
            Rule::r#return => Self::Return(Return::parse(pair)),
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
        let span = pair.as_span().into();
        let mut pairs = pair.into_inner();

        let r#type = Type::parse(pairs.next().unwrap());
        let name = Identifier::parse(pairs.next().unwrap());

        Self { r#type, name, span }
    }
}

impl Node for FunctionDeclaration {
    fn parse(pair: Pair<Rule>) -> Self {
        let span = pair.as_span().into();
        let mut pairs = pair.into_inner();

        let return_type = Type::parse(pairs.next().unwrap());
        let name = Identifier::parse(pairs.next().unwrap());
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
            span,
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

pub fn parse(named_source: miette::NamedSource<String>) -> miette::Result<Program> {
    let mut pest_output = PestParser::parse(Rule::prog, named_source.inner())
        .map_err(|pest_err| pest_err.into_miette())
        .wrap_err("cannot parse program")?;

    let program = Program::parse(pest_output.next().unwrap());

    Ok(program)
}

#[cfg(test)]
mod tests {
    use miette::NamedSource;

    use super::*;

    #[test]
    fn tricky_or_with_neg() {
        let source = NamedSource::new("test.flo", "ecrire(1 - 2 ou -6);".to_string());
        let prog = parse(source).unwrap();

        let Statement::Write(write_stmt) = &prog.statements[0] else {
            panic!("not a write statement");
        };

        let arg = &write_stmt.value;

        // Check that the parsed expression is structured as expected.
        if let Expression::BinaryOp(BinaryOp {
            left,
            kind: BinaryOpKind::LogicOr,
            right,
            span: _,
        }) = arg
        {
            // Validate the left operand
            if let Expression::BinaryOp(BinaryOp {
                left: left_inner,
                kind: BinaryOpKind::Sub,
                right: right_inner,
                span: _,
            }) = &**left
            {
                // Validate left_inner = 1
                assert!(matches!(**left_inner, Expression::Integer(1, _)));
                // Validate right_inner = 2
                assert!(matches!(**right_inner, Expression::Integer(2, _)));
            } else {
                panic!("Left side of 'ou' is not a subtraction expression");
            }

            // Validate the right operand
            if let Expression::UnaryOp(UnaryOp {
                kind: UnaryOpKind::Neg,
                operand,
                span: _,
            }) = &**right
            {
                // Validate operand = 6
                assert!(matches!(**operand, Expression::Integer(6, _)));
            } else {
                panic!("Right side of 'ou' is not a negation expression");
            }
        } else {
            panic!("Write statement argument is not a logical 'ou' expression");
        }
    }

    #[test]
    fn nona_as_ident() {
        let source = NamedSource::new("test.flo", "ecrire(nona);".to_string());
        let prog = parse(source).unwrap();

        // Ensure the first statement is a Write statement
        let Statement::Write(write_stmt) = &prog.statements[0] else {
            panic!("not a write statement");
        };

        // Check the value being written is a variable
        let arg = &write_stmt.value;
        if let Expression::Variable(var) = arg {
            assert_eq!(var.ident, "nona"); // Validate the variable name
        } else {
            panic!("Write statement argument is not a variable");
        }
    }

    #[test]
    fn non_a_as_op() {
        let source = NamedSource::new("test.flo", "ecrire(non a);".to_string());
        let prog = parse(source).unwrap();

        // Ensure the first statement is a Write statement
        let Statement::Write(write_stmt) = &prog.statements[0] else {
            panic!("not a write statement");
        };

        // Check the value being written is a unary operation
        let arg = &write_stmt.value;
        if let Expression::UnaryOp(UnaryOp {
            kind: UnaryOpKind::LogicNot,
            operand,
            span: _,
        }) = arg
        {
            // Validate that the operand is a variable
            if let Expression::Variable(var) = &**operand {
                assert_eq!(var.ident, "a"); // Validate the variable name
            } else {
                panic!("Operand of 'non' is not a variable");
            }
        } else {
            panic!("Write statement argument is not a unary 'non' expression");
        }
    }

    #[test]
    fn non_a_with_parent_as_op() {
        let source = NamedSource::new("test.flo", "ecrire(non(a));".to_string());
        let prog = parse(source).unwrap();

        // Ensure the first statement is a Write statement
        let Statement::Write(write_stmt) = &prog.statements[0] else {
            panic!("not a write statement");
        };

        // Check the value being written is a unary operation
        let arg = &write_stmt.value;
        if let Expression::UnaryOp(UnaryOp {
            kind: UnaryOpKind::LogicNot,
            operand,
            span: _,
        }) = arg
        {
            // Validate that the operand is a variable (inside parentheses)
            if let Expression::Variable(var) = &**operand {
                assert_eq!(var.ident, "a"); // Validate the variable name
            } else {
                panic!("Operand of 'non' is not a variable");
            }
        } else {
            panic!("Write statement argument is not a unary 'non' expression");
        }
    }

    #[test]
    fn underscore_ou_a_as_op() {
        let source = NamedSource::new("test.flo", "ecrire(_oua);".to_string());
        let prog = parse(source).unwrap();

        // Ensure the first statement is a Write statement
        let Statement::Write(write_stmt) = &prog.statements[0] else {
            panic!("not a write statement");
        };

        // Check the value being written is a variable
        let arg = &write_stmt.value;
        if let Expression::Variable(var) = arg {
            assert_eq!(var.ident, "_oua"); // Validate the variable name
        } else {
            panic!("Write statement argument is not a variable");
        }
    }

    #[test]
    fn underscore_ou_a_with_parent_as_ident() {
        let source = NamedSource::new("test.flo", "ecrire((_)ou(a));".to_string());
        let prog = parse(source).unwrap();

        // Ensure the first statement is a Write statement
        let Statement::Write(write_stmt) = &prog.statements[0] else {
            panic!("not a write statement");
        };

        // Check the value being written is a binary operation
        let arg = &write_stmt.value;
        if let Expression::BinaryOp(BinaryOp {
            left,
            kind: BinaryOpKind::LogicOr,
            right,
            span: _,
        }) = arg
        {
            // Validate the left operand
            if let Expression::Variable(var) = &**left {
                assert_eq!(var.ident, "_"); // Validate the variable name
            } else {
                panic!("Left operand of 'ou' is not a variable");
            }

            // Validate the right operand
            if let Expression::Variable(var) = &**right {
                assert_eq!(var.ident, "a"); // Validate the variable name
            } else {
                panic!("Right operand of 'ou' is not a variable");
            }
        } else {
            panic!("Write statement argument is not a binary 'ou' expression");
        }
    }

    #[test]
    fn vraioufaux_as_ident() {
        let source = NamedSource::new("test.flo", "ecrire(VraiouFaux);".to_string());
        let prog = parse(source).unwrap();

        // Ensure the first statement is a Write statement
        let Statement::Write(write_stmt) = &prog.statements[0] else {
            panic!("not a write statement");
        };

        // Check the value being written is a variable
        let arg = &write_stmt.value;
        if let Expression::Variable(var) = arg {
            assert_eq!(var.ident, "VraiouFaux"); // Validate the variable name
        } else {
            panic!("Write statement argument is not a variable");
        }
    }

    #[test]
    fn nonfaux_as_ident() {
        let source = NamedSource::new("test.flo", "ecrire(nonFaux);".to_string());
        let prog = parse(source).unwrap();

        // Ensure the first statement is a Write statement
        let Statement::Write(write_stmt) = &prog.statements[0] else {
            panic!("not a write statement");
        };

        // Check the value being written is a variable
        let arg = &write_stmt.value;
        if let Expression::Variable(var) = arg {
            assert_eq!(var.ident, "nonFaux"); // Validate the variable name
        } else {
            panic!("Write statement argument is not a variable");
        }
    }

    #[test]
    fn non7_as_ident() {
        let source = NamedSource::new("test.flo", "ecrire(non7);".to_string());
        let prog = parse(source).unwrap();

        // Ensure the first statement is a Write statement
        let Statement::Write(write_stmt) = &prog.statements[0] else {
            panic!("not a write statement");
        };

        // Check the value being written is a variable
        let arg = &write_stmt.value;
        if let Expression::Variable(var) = arg {
            assert_eq!(var.ident, "non7"); // Validate the variable name
        } else {
            panic!("Write statement argument is not a variable");
        }
    }
}
