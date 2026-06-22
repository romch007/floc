use chumsky::input::{Stream, ValueInput};
use chumsky::pratt::{infix, left, prefix};
use chumsky::prelude::*;
use chumsky::span::SimpleSpan;
use floc_ast::{
    Argument, Assignment, BinaryOp, BinaryOpKind, Declaration, Expression, FunctionCall,
    FunctionDeclaration, Identifier, If, Program, Return, Statement, Type, TypeKind, UnaryOp,
    UnaryOpKind, While, Write,
};
use floc_lexer::Token;
use logos::Logos;

type Extra<'tok, 'src> = extra::Err<Rich<'tok, Token<'src>>>;

// Concrete `SimpleSpan` parameter forces normalisation of `I::Span` that a bare
// `.into()` won't trigger.
#[inline]
fn to_span(span: SimpleSpan) -> floc_span::Span {
    span.into()
}

fn program_parser<'tok, 'src: 'tok, I>() -> impl Parser<'tok, I, Program, Extra<'tok, 'src>>
where
    I: ValueInput<'tok, Token = Token<'src>, Span = SimpleSpan>,
{
    let ident = select! { Token::Identifier(name) => name }
        .map_with(|name, e| Identifier {
            ident: name.to_string(),
            span: to_span(e.span()),
        })
        .labelled("identifier");

    let r#type = select! {
        Token::IntegerType => TypeKind::Integer,
        Token::BooleanType => TypeKind::Boolean,
    }
    .map_with(|kind, e| Type {
        kind,
        span: to_span(e.span()),
    })
    .labelled("type");

    let expr = recursive(|expr| {
        let call_args = expr
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>();

        let function_call = ident
            .then(call_args.delimited_by(just(Token::LParen), just(Token::RParen)))
            .map_with(|(name, arguments), e| FunctionCall {
                name,
                arguments,
                span: to_span(e.span()),
            });

        let read = just(Token::Read)
            .then(just(Token::LParen))
            .then(just(Token::RParen))
            .map_with(|_, e| Expression::Read(to_span(e.span())));

        let atom = choice((
            // must precede `ident`: both start with an identifier
            function_call.map(Expression::FunctionCall),
            read,
            select! { Token::Integer(n) => n }
                .map_with(|n, e| Expression::Integer(n, to_span(e.span()))),
            select! { Token::Boolean(b) => b }
                .map_with(|b, e| Expression::Boolean(b, to_span(e.span()))),
            ident.map(Expression::Variable),
            expr.delimited_by(just(Token::LParen), just(Token::RParen)),
        ))
        .recover_with(via_parser(nested_delimiters(
            Token::LParen,
            Token::RParen,
            [(Token::LBrace, Token::RBrace)],
            |span| Expression::Error(to_span(span)),
        )));

        macro_rules! binary {
            ($prec:expr, $tok:expr, $kind:expr) => {
                infix(
                    left($prec),
                    just($tok).map_with(|_, e| -> SimpleSpan { e.span() }),
                    |left, op_span: SimpleSpan, right, e| {
                        Expression::BinaryOp(BinaryOp {
                            left: Box::new(left),
                            kind: $kind,
                            right: Box::new(right),
                            span: to_span(e.span()),
                            operator_span: to_span(op_span),
                        })
                    },
                )
            };
        }

        macro_rules! unary {
            ($prec:expr, $tok:expr, $kind:expr) => {
                prefix(
                    $prec,
                    just($tok).map_with(|_, e| -> SimpleSpan { e.span() }),
                    |op_span: SimpleSpan, operand, e| {
                        Expression::UnaryOp(UnaryOp {
                            kind: $kind,
                            operand: Box::new(operand),
                            span: to_span(e.span()),
                            operator_span: to_span(op_span),
                        })
                    },
                )
            };
        }

        atom.pratt((
            binary!(1, Token::LogicOr, BinaryOpKind::LogicOr),
            binary!(1, Token::LogicAnd, BinaryOpKind::LogicAnd),
            unary!(2, Token::LogicNot, UnaryOpKind::LogicNot),
            binary!(3, Token::Eq, BinaryOpKind::Eq),
            binary!(3, Token::Neq, BinaryOpKind::Neq),
            binary!(4, Token::Lt, BinaryOpKind::Lt),
            binary!(4, Token::Lte, BinaryOpKind::Lte),
            binary!(4, Token::Gt, BinaryOpKind::Gt),
            binary!(4, Token::Gte, BinaryOpKind::Gte),
            binary!(5, Token::Add, BinaryOpKind::Add),
            binary!(5, Token::Sub, BinaryOpKind::Sub),
            binary!(6, Token::Mul, BinaryOpKind::Mul),
            binary!(6, Token::Div, BinaryOpKind::Div),
            binary!(6, Token::Mod, BinaryOpKind::Mod),
            unary!(7, Token::Sub, UnaryOpKind::Neg),
        ))
        .labelled("expression")
    });

    let stmt = recursive(|stmt| {
        let braced = stmt
            .clone()
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace));

        let declaration = r#type
            .then(ident)
            .then(just(Token::Assignment).ignore_then(expr.clone()).or_not())
            .then_ignore(just(Token::SemiColon))
            .map_with(|((r#type, variable), value), e| {
                Statement::Declaration(Declaration {
                    r#type,
                    variable,
                    value,
                    span: to_span(e.span()),
                })
            });

        let assignment = ident
            .then_ignore(just(Token::Assignment))
            .then(expr.clone())
            .then_ignore(just(Token::SemiColon))
            .map_with(|(variable, value), e| {
                Statement::Assignment(Assignment {
                    variable,
                    value,
                    span: to_span(e.span()),
                })
            });

        let write = just(Token::Write)
            .ignore_then(
                expr.clone()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .then_ignore(just(Token::SemiColon))
            .map_with(|value, e| {
                Statement::Write(Write {
                    value,
                    span: to_span(e.span()),
                })
            });

        let r#return = just(Token::Return)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::SemiColon))
            .map_with(|value, e| {
                Statement::Return(Return {
                    value,
                    span: to_span(e.span()),
                })
            });

        let r#while = just(Token::While)
            .ignore_then(
                expr.clone()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .then(braced.clone())
            .map_with(|(condition, statements), e| {
                Statement::While(While {
                    condition,
                    statements,
                    span: to_span(e.span()),
                })
            });

        let r#if = recursive(|r#if| {
            just(Token::If)
                .ignore_then(
                    expr.clone()
                        .delimited_by(just(Token::LParen), just(Token::RParen)),
                )
                .then(braced.clone())
                .then(
                    just(Token::Else)
                        .ignore_then(braced.clone().or(r#if.map(|s| vec![s])))
                        .or_not(),
                )
                .map_with(|((condition, statements), statements_else), e| {
                    Statement::If(If {
                        condition,
                        statements,
                        statements_else,
                        span: to_span(e.span()),
                    })
                })
        });

        let discard_fn_call = ident
            .then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            )
            .map_with(|(name, arguments), e| FunctionCall {
                name,
                arguments,
                span: to_span(e.span()),
            })
            .then_ignore(just(Token::SemiColon))
            .map(Statement::DiscardFunctionCall);

        choice((
            declaration,
            r#while,
            r#if,
            write,
            r#return,
            discard_fn_call,
            assignment,
        ))
        .recover_with(via_parser(
            nested_delimiters(
                Token::LBrace,
                Token::RBrace,
                [(Token::LParen, Token::RParen)],
                |span| Statement::Error(to_span(span)),
            )
            .or(none_of([Token::SemiColon, Token::RBrace])
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>()
                .then(just(Token::SemiColon).or_not())
                .map_with(|_, e| Statement::Error(to_span(e.span())))),
        ))
    });

    let argument = r#type.then(ident).map_with(|(r#type, name), e| Argument {
        r#type,
        name,
        span: to_span(e.span()),
    });

    let function_decl = r#type
        .then(ident)
        .then(
            argument
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then(
            stmt.clone()
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .map_with(
            |(((return_type, name), arguments), statements), e| FunctionDeclaration {
                return_type,
                name,
                arguments,
                statements,
                span: to_span(e.span()),
            },
        );

    function_decl
        .repeated()
        .collect::<Vec<_>>()
        .then(stmt.repeated().at_least(1).collect::<Vec<_>>())
        .map(|(function_decls, statements)| Program {
            function_decls,
            statements,
        })
        .then_ignore(end())
}

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("cannot parse program")]
#[diagnostic(code(floc::parse_error))]
pub struct ParseError {
    #[source_code]
    src: miette::NamedSource<String>,

    #[label(collection)]
    labels: Vec<miette::LabeledSpan>,
}

pub fn parse(named_source: miette::NamedSource<String>) -> miette::Result<Program> {
    let result = {
        let source = named_source.inner().as_str();

        let token_iter = Token::lexer(source).spanned().map(|(tok, span)| match tok {
            Ok(tok) => (tok, span.into()),
            Err(()) => (Token::Error, span.into()),
        });

        let token_stream =
            Stream::from_iter(token_iter).map((0..source.len()).into(), |(t, s)| (t, s));

        program_parser()
            .parse(token_stream)
            .into_result()
            .map_err(|errors| {
                errors
                    .into_iter()
                    .map(|error| {
                        let span = error.span();
                        miette::LabeledSpan::new(
                            Some(error.to_string()),
                            span.start,
                            span.end - span.start,
                        )
                    })
                    .collect::<Vec<_>>()
            })
    };

    result.map_err(|labels| {
        miette::Report::new(ParseError {
            src: named_source,
            labels,
        })
    })
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
            ..
        }) = arg
        {
            // Validate the left operand
            if let Expression::BinaryOp(BinaryOp {
                left: left_inner,
                kind: BinaryOpKind::Sub,
                right: right_inner,
                ..
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
                ..
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
            ..
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
            ..
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
            ..
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
