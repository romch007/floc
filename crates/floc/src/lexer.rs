use logos::Logos;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Logos)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum Token<'source> {
    // Sentinel substituted for lexing errors so parsing stays recoverable.
    Error,

    #[regex(r"#[^\n]*", logos::skip, allow_greedy = true)]
    Comment,

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

    #[token("<=")]
    Lte,

    #[token(">=")]
    Gte,

    #[token("<")]
    Lt,

    #[token(">")]
    Gt,

    #[token("ou")]
    LogicOr,

    #[token("et")]
    LogicAnd,

    #[token("non")]
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

    #[token("Vrai", |_| true)]
    #[token("Faux", |_| false)]
    Boolean(bool),

    #[regex("[0-9]+", |lex| lex.slice().parse::<u64>().unwrap())]
    Integer(u64),

    #[regex("[A-Za-z_][A-Za-z0-9_]*", |lex| lex.slice())]
    Identifier(&'source str),
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Error => write!(f, "<invalid token>"),
            Token::Comment => write!(f, "<comment>"),
            Token::Add => write!(f, "+"),
            Token::Sub => write!(f, "-"),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::Mod => write!(f, "%"),
            Token::Eq => write!(f, "=="),
            Token::Neq => write!(f, "!="),
            Token::Lte => write!(f, "<="),
            Token::Gte => write!(f, ">="),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::LogicOr => write!(f, "ou"),
            Token::LogicAnd => write!(f, "et"),
            Token::LogicNot => write!(f, "non"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Assignment => write!(f, "="),
            Token::Comma => write!(f, ","),
            Token::SemiColon => write!(f, ";"),
            Token::IntegerType => write!(f, "entier"),
            Token::BooleanType => write!(f, "booleen"),
            Token::Read => write!(f, "lire"),
            Token::Write => write!(f, "ecrire"),
            Token::If => write!(f, "si"),
            Token::Else => write!(f, "sinon"),
            Token::Return => write!(f, "retourner"),
            Token::While => write!(f, "tantque"),
            Token::Boolean(b) => write!(f, "{}", if *b { "Vrai" } else { "Faux" }),
            Token::Integer(n) => write!(f, "{n}"),
            Token::Identifier(name) => write!(f, "{name}"),
        }
    }
}
