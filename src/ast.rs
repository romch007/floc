use std::io;

use crate::parser;
use pest::Span;

fn span_into_str(span: Span) -> &str {
    span.as_str()
}

fn span_into_string(span: Span) -> String {
    span.as_str().to_string()
}

fn span_into_boolean(span: Span) -> bool {
    match span.as_str() {
        "Vrai" => true,
        "False" => false,
        _ => panic!("invalid boolean '{}'", span.as_str()),
    }
}

fn debug_print<T>(w: &mut impl io::Write, value: T, indent: usize) -> Result<(), io::Error>
where
    T: std::fmt::Display,
{
    writeln!(w, "{}{}", " ".repeat(indent), value)?;

    Ok(())
}

pub trait Node {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error>;
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::r#type))]
pub struct Type {
    #[pest_ast(outer(with(span_into_string)))]
    pub name: String,
}

impl Node for Type {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        debug_print(w, &self.name, indent)?;

        Ok(())
    }
}

impl AsRef<str> for Type {
    fn as_ref(&self) -> &str {
        &self.name
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::integer))]
pub struct Integer {
    #[pest_ast(outer(with(span_into_str), with(str::parse), with(Result::unwrap)))]
    pub value: u32,
}

impl Node for Integer {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        debug_print(w, self.value, indent)?;

        Ok(())
    }
}

impl AsRef<u32> for Integer {
    fn as_ref(&self) -> &u32 {
        &self.value
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::boolean))]
pub struct Boolean {
    #[pest_ast(outer(with(span_into_boolean)))]
    pub value: bool,
}

impl Node for Boolean {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        debug_print(w, self.value, indent)?;

        Ok(())
    }
}

impl AsRef<bool> for Boolean {
    fn as_ref(&self) -> &bool {
        &self.value
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::read))]
pub struct Read;

impl Node for Read {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        debug_print(w, "Read", indent)?;

        Ok(())
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::function_call))]
pub struct FunctionCall {
    pub name: Identifier,
    pub arguments: Vec<Expression>,
}

impl Node for FunctionCall {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        debug_print(w, "FunctionCall", indent)?;

        self.name.debug_print(w, indent + 2)?;

        for arg in &self.arguments {
            arg.debug_print(w, indent + 2)?;
        }

        Ok(())
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::expr))]
pub enum Expression {
    Integer(Integer),
    Variable(Identifier),
    Boolean(Boolean),
    Read(Read),
    FunctionCall(FunctionCall),
}

impl Node for Expression {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        match self {
            Expression::Integer(integer) => integer.debug_print(w, indent),
            Expression::Variable(variable) => variable.debug_print(w, indent),
            Expression::Boolean(boolean) => boolean.debug_print(w, indent),
            Expression::Read(read) => read.debug_print(w, indent),
            Expression::FunctionCall(fn_call) => fn_call.debug_print(w, indent),
        }?;

        Ok(())
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::ident))]
pub struct Identifier {
    #[pest_ast(outer(with(span_into_string)))]
    pub value: String,
}

impl Node for Identifier {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        debug_print(w, &self.value, indent)?;

        Ok(())
    }
}

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        &self.value
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::assignment))]
pub struct Assignment {
    pub variable: Identifier,
    pub value: Expression,
}

impl Node for Assignment {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        debug_print(w, "Assignment", indent)?;
        self.variable.debug_print(w, indent + 2)?;
        debug_print(w, "=", indent + 2)?;
        self.value.debug_print(w, indent + 2)?;

        Ok(())
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::declaration))]
pub struct Declaration {
    pub r#type: Type,
    pub variable: Identifier,
    pub value: Option<Expression>,
}

impl Node for Declaration {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        debug_print(w, "Declaration", indent)?;
        self.r#type.debug_print(w, indent + 2)?;
        self.variable.debug_print(w, indent + 2)?;
        if let Some(default_value) = &self.value {
            debug_print(w, "=", indent + 2)?;
            default_value.debug_print(w, indent + 2)?;
        }

        Ok(())
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::write))]
pub struct Write {
    pub value: Expression,
}

impl Node for Write {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        debug_print(w, "Write", indent)?;
        self.value.debug_print(w, indent + 2)?;

        Ok(())
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::r#return))]
pub struct Return {
    pub value: Expression,
}

impl Node for Return {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        debug_print(w, "Return", indent)?;
        self.value.debug_print(w, indent + 2)?;

        Ok(())
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::r#while))]
pub struct While {
    pub condition: Expression,
    pub statements: Vec<Statement>,
}

impl Node for While {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        debug_print(w, "While", indent)?;
        self.condition.debug_print(w, indent + 2)?;
        for stmt in &self.statements {
            stmt.debug_print(w, indent + 2)?;
        }

        Ok(())
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::r#if))]
pub struct If {
    pub condition: Expression,
    pub statements: Vec<Statement>,
}

impl Node for If {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        debug_print(w, "If", indent)?;
        self.condition.debug_print(w, indent + 2)?;
        for stmt in &self.statements {
            stmt.debug_print(w, indent + 2)?;
        }

        Ok(())
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::stmt))]
pub enum Statement {
    Assignment(Assignment),
    Declaration(Declaration),
    Write(Write),
    Return(Return),
    While(While),
    If(If),
}

impl Node for Statement {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        match self {
            Statement::Assignment(assign) => assign.debug_print(w, indent),
            Statement::Declaration(decl) => decl.debug_print(w, indent),
            Statement::Write(write) => write.debug_print(w, indent),
            Statement::Return(ret) => ret.debug_print(w, indent),
            Statement::While(whil) => whil.debug_print(w, indent),
            Statement::If(i) => i.debug_print(w, indent),
        }?;

        Ok(())
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::argument))]
pub struct Argument {
    pub r#type: Type,
    pub name: Identifier,
}

impl Node for Argument {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        debug_print(w, "Argument", indent)?;
        self.r#type.debug_print(w, indent + 2)?;
        self.name.debug_print(w, indent + 2)?;

        Ok(())
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::function_decl))]
pub struct FunctionDeclaration {
    pub return_type: Type,
    pub name: Identifier,
    pub arguments: Vec<Argument>,
    pub statements: Vec<Statement>,
}

impl Node for FunctionDeclaration {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        debug_print(w, "FunctionDeclaration", indent)?;
        self.return_type.debug_print(w, indent + 2)?;
        self.name.debug_print(w, indent + 2)?;

        for arg in &self.arguments {
            arg.debug_print(w, indent + 2)?;
        }

        for stmt in &self.statements {
            stmt.debug_print(w, indent + 2)?;
        }

        Ok(())
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::prog))]
pub struct Program {
    pub function_decls: Vec<FunctionDeclaration>,
    pub statements: Vec<Statement>,
    _eoi: Eoi,
}

impl Node for Program {
    fn debug_print(&self, w: &mut impl io::Write, indent: usize) -> Result<(), io::Error> {
        debug_print(w, "Program", indent)?;

        for fn_decl in &self.function_decls {
            fn_decl.debug_print(w, indent + 2)?;
        }

        for stmt in &self.statements {
            stmt.debug_print(w, indent + 2)?;
        }

        Ok(())
    }
}

#[derive(Debug, FromPest)]
#[pest_ast(rule(parser::Rule::EOI))]
struct Eoi;
