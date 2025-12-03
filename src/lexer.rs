//! This lexer is only used for syntax highlighting in error reports as of now

use logos::Logos;

#[derive(Debug, Logos, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    #[token("+")]
    Add,

    #[token("-")]
    Sub,

    #[token("*")]
    Mul,

    #[token("/")]
    Div,

    #[token("%")]
    Mod,

    #[token("==")]
    Eq,

    #[token("!=")]
    Neq,

    #[token("<")]
    Lt,

    #[token(">")]
    Gt,

    #[token("<=")]
    Lte,

    #[token(">=")]
    Gte,

    #[token("||")]
    LogicOr,

    #[token("&&")]
    LogicAnd,

    #[token("not")]
    LogicNot,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("=")]
    Assignment,

    #[token(",")]
    Comma,

    #[token(";")]
    SemiColon,

    #[regex("[A-Za-z0-9_]+", |lex| lex.slice().to_string())]
    Identifier(String),

    #[regex("[0-9]+", |lex| lex.slice().parse::<u64>().unwrap(), priority = 3)]
    Integer(u64),

    #[token("Faux", |_| false)]
    #[token("Vrai", |_| true)]
    Boolean(bool),

    #[token("entier")]
    IntegerType,

    #[token("booleen")]
    BooleanType,

    #[token("lire")]
    Read,

    #[token("ecrire")]
    Write,

    #[token("si")]
    If,

    #[token("sinon")]
    Else,

    #[token("retourner")]
    Return,

    #[token("tantque")]
    While,
}
