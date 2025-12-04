use std::{
    cell::Cell,
    io::{self, BufWriter, Write},
};

use crate::ast::*;

pub fn dump_graph(prog: &Program) -> Result<(), io::Error> {
    let mut stdout = BufWriter::new(io::stdout().lock());
    let name_helper = NodeNameHelper::new();

    writeln!(stdout, "digraph AST {{")?;

    prog.visit(&mut stdout, &name_helper)?;

    writeln!(stdout, "}}")?;

    Ok(())
}

trait ToGraph {
    fn visit<W>(&self, w: &mut W, name_helper: &NodeNameHelper) -> Result<String, io::Error>
    where
        W: io::Write;
}

impl ToGraph for Expression {
    fn visit<W>(&self, w: &mut W, name_helper: &NodeNameHelper) -> Result<String, io::Error>
    where
        W: io::Write,
    {
        let node_name = match self {
            Expression::FunctionCall(function_call) => Some(function_call.visit(w, name_helper)?),
            Expression::BinaryOp(binary_op) => Some(binary_op.visit(w, name_helper)?),
            Expression::UnaryOp(unary_op) => Some(unary_op.visit(w, name_helper)?),
            _ => None,
        };

        if let Some(node_name) = node_name {
            return Ok(node_name);
        }

        let node_name = name_helper.get_next_node_name();

        let node_label = match self {
            Expression::Integer(val, _) => val.to_string(),
            Expression::Variable(var_name) => var_name.ident.clone(),
            Expression::Boolean(val, _) => val.to_string(),
            Expression::Read(_) => "read()".to_string(),
            _ => unreachable!(),
        };

        write_node(w, &node_name, &node_label, "box")?;

        Ok(node_name)
    }
}

impl ToGraph for UnaryOp {
    fn visit<W>(&self, w: &mut W, name_helper: &NodeNameHelper) -> Result<String, io::Error>
    where
        W: io::Write,
    {
        let node_name = name_helper.get_next_node_name();
        let sign = match self.kind {
            UnaryOpKind::Neg => "-",
            UnaryOpKind::LogicNot => "!",
        };

        write_node(w, &node_name, sign, "circle")?;

        let operand_node = self.operand.visit(w, name_helper)?;

        write_edge(w, &node_name, &operand_node, None)?;

        Ok(node_name)
    }
}

impl ToGraph for BinaryOp {
    fn visit<W>(&self, w: &mut W, name_helper: &NodeNameHelper) -> Result<String, io::Error>
    where
        W: io::Write,
    {
        let node_name = name_helper.get_next_node_name();
        let sign = match self.kind {
            BinaryOpKind::Add => "+",
            BinaryOpKind::Sub => "-",
            BinaryOpKind::Mul => "*",
            BinaryOpKind::Div => "/",
            BinaryOpKind::Mod => "%",
            BinaryOpKind::Eq => "==",
            BinaryOpKind::Neq => "!=",
            BinaryOpKind::Lt => "<",
            BinaryOpKind::Lte => "<=",
            BinaryOpKind::Gt => ">",
            BinaryOpKind::Gte => ">=",
            BinaryOpKind::LogicAnd => "&&",
            BinaryOpKind::LogicOr => "||",
        };
        write_node(w, &node_name, sign, "circle")?;

        let left_node = self.left.visit(w, name_helper)?;
        let right_node = self.right.visit(w, name_helper)?;

        write_edge(w, &node_name, &left_node, None)?;
        write_edge(w, &node_name, &right_node, None)?;

        Ok(node_name)
    }
}

impl ToGraph for FunctionCall {
    fn visit<W>(&self, w: &mut W, name_helper: &NodeNameHelper) -> Result<String, io::Error>
    where
        W: io::Write,
    {
        let node_name = name_helper.get_next_node_name();
        write_node(
            w,
            &node_name,
            &format!("{}(...)", self.name.ident),
            "ellipse",
        )?;

        for (i, arg) in self.arguments.iter().enumerate() {
            let arg_node_name = arg.visit(w, name_helper)?;
            write_edge(w, &node_name, &arg_node_name, Some(&format!("arg {i}")))?;
        }

        Ok(node_name)
    }
}

impl ToGraph for Statement {
    fn visit<W>(&self, w: &mut W, name_helper: &NodeNameHelper) -> Result<String, io::Error>
    where
        W: io::Write,
    {
        let node_name = match self {
            Statement::Assignment(assignment) => Some(assignment.visit(w, name_helper)?),
            Statement::Declaration(declaration) => Some(declaration.visit(w, name_helper)?),
            Statement::While(whil) => Some(whil.visit(w, name_helper)?),
            Statement::If(i) => Some(i.visit(w, name_helper)?),
            Statement::DiscardFunctionCall(function_call) => {
                Some(function_call.visit(w, name_helper)?)
            }
            _ => None,
        };

        if let Some(node_name) = node_name {
            return Ok(node_name);
        }

        let node_name = name_helper.get_next_node_name();

        // TODO: wrap these in dedicated functions
        let node_label = match self {
            Statement::Write(write) => {
                let value_node_name = write.value.visit(w, name_helper)?;

                write_edge(w, &node_name, &value_node_name, None)?;

                "write(...)".to_string()
            }
            Statement::Return(ret) => {
                let value_node_name = ret.value.visit(w, name_helper)?;

                write_edge(w, &node_name, &value_node_name, None)?;

                "return".to_string()
            }
            _ => unreachable!(),
        };

        write_node(w, &node_name, &node_label, "ellipse")?;

        Ok(node_name)
    }
}

impl ToGraph for While {
    fn visit<W>(&self, w: &mut W, name_helper: &NodeNameHelper) -> Result<String, io::Error>
    where
        W: io::Write,
    {
        let node_name = name_helper.get_next_node_name();
        write_node(w, &node_name, "while", "ellipse")?;

        let condition_node_name = self.condition.visit(w, name_helper)?;
        write_edge(w, &node_name, &condition_node_name, Some("condition"))?;

        let block_node_name = write_block(w, name_helper, &self.statements)?;
        write_edge(w, &node_name, &block_node_name, Some("body"))?;

        Ok(node_name)
    }
}

impl ToGraph for If {
    fn visit<W>(&self, w: &mut W, name_helper: &NodeNameHelper) -> Result<String, io::Error>
    where
        W: io::Write,
    {
        let node_name = name_helper.get_next_node_name();
        write_node(w, &node_name, "if", "ellipse")?;

        let condition_node_name = self.condition.visit(w, name_helper)?;
        write_edge(w, &node_name, &condition_node_name, Some("condition"))?;

        let then_block_node_name = write_block(w, name_helper, &self.statements)?;
        write_edge(w, &node_name, &then_block_node_name, Some("then"))?;

        if let Some(stmts_else) = &self.statements_else {
            let else_block_name = write_block(w, name_helper, stmts_else)?;
            write_edge(w, &node_name, &else_block_name, Some("else"))?;
        }

        Ok(node_name)
    }
}

impl ToGraph for Declaration {
    fn visit<W>(&self, w: &mut W, name_helper: &NodeNameHelper) -> Result<String, io::Error>
    where
        W: io::Write,
    {
        let node_name = name_helper.get_next_node_name();
        write_node(w, &node_name, "var declaration", "ellipse")?;

        let type_node_name = name_helper.get_next_node_name();
        write_node(w, &type_node_name, &self.r#type.kind.to_string(), "diamond")?;
        write_edge(w, &node_name, &type_node_name, Some("type"))?;

        let name_node_name = name_helper.get_next_node_name();
        write_node(w, &name_node_name, &self.variable, "ellipse")?;
        write_edge(w, &node_name, &name_node_name, Some("name"))?;

        if let Some(value) = &self.value {
            let value_node_name = value.visit(w, name_helper)?;
            write_edge(w, &node_name, &value_node_name, Some("value"))?;
        }

        Ok(node_name)
    }
}

impl ToGraph for Assignment {
    fn visit<W>(&self, w: &mut W, name_helper: &NodeNameHelper) -> Result<String, io::Error>
    where
        W: io::Write,
    {
        let node_name = name_helper.get_next_node_name();
        write_node(w, &node_name, "var assignment", "ellipse")?;

        let name_node_name = name_helper.get_next_node_name();
        write_node(w, &name_node_name, &self.variable, "ellipse")?;
        write_edge(w, &node_name, &name_node_name, Some("name"))?;

        let value_node_name = self.value.visit(w, name_helper)?;
        write_edge(w, &node_name, &value_node_name, Some("value"))?;

        Ok(node_name)
    }
}

impl ToGraph for FunctionDeclaration {
    fn visit<W>(&self, w: &mut W, name_helper: &NodeNameHelper) -> Result<String, io::Error>
    where
        W: io::Write,
    {
        writeln!(
            w,
            "subgraph cluster{fn_name} {{\nstyle=filled;color=lightgrey;label=\"function {fn_name}\";",
            fn_name = self.name.ident
        )?;

        let node_name = name_helper.get_next_node_name();
        write_node(
            w,
            &node_name,
            &format!("declaring {}(...)", self.name.ident),
            "ellipse",
        )?;

        let return_type_node_name = name_helper.get_next_node_name();
        write_node(
            w,
            &return_type_node_name,
            &self.return_type.kind.to_string(),
            "diamond",
        )?;
        write_edge(w, &node_name, &return_type_node_name, Some("return type"))?;

        let args_node_name = write_arguments(w, name_helper, &self.arguments)?;
        write_edge(w, &node_name, &args_node_name, Some("args"))?;

        let block_node_name = write_block(w, name_helper, &self.statements)?;
        write_edge(w, &node_name, &block_node_name, Some("body"))?;

        writeln!(w, "}}")?;

        Ok(node_name)
    }
}

impl ToGraph for Program {
    fn visit<W>(&self, w: &mut W, name_helper: &NodeNameHelper) -> Result<String, io::Error>
    where
        W: io::Write,
    {
        let node_name = name_helper.get_next_node_name();
        write_node(w, &node_name, "program", "ellipse")?;

        for fn_decl in &self.function_decls {
            let _fn_decl_node_name = fn_decl.visit(w, name_helper)?;
            // write_edge(w, &node_name, &fn_decl_node_name, None)?;
        }

        let block_node_name = write_block(w, name_helper, &self.statements)?;
        write_edge(w, &node_name, &block_node_name, None)?;

        Ok(node_name)
    }
}

fn write_node<W>(w: &mut W, name: &str, label: &str, shape: &str) -> Result<(), io::Error>
where
    W: io::Write,
{
    writeln!(w, "\"{name}\"[shape={shape},label=\"{label}\"];")?;

    Ok(())
}

fn write_node_unescaped<W>(w: &mut W, name: &str, label: &str, shape: &str) -> Result<(), io::Error>
where
    W: io::Write,
{
    writeln!(w, "\"{name}\"[label={label},shape={shape}];")?;

    Ok(())
}

fn write_edge<W>(w: &mut W, from: &str, to: &str, label: Option<&str>) -> Result<(), io::Error>
where
    W: io::Write,
{
    write!(w, "\"{from}\" -> \"{to}\"")?;

    if let Some(label) = label {
        write!(w, "[label=\"{label}\"]")?;
    }

    writeln!(w, ";")?;

    Ok(())
}

fn write_block<W>(
    w: &mut W,
    name_helper: &NodeNameHelper,
    stmts: &[Statement],
) -> Result<String, io::Error>
where
    W: io::Write,
{
    let block_node_name = name_helper.get_next_node_name();
    write_node_unescaped(w, &block_node_name, "<{<I>block</I>}>", "ellipse")?;

    for stmt in stmts {
        let stmt_node_name = stmt.visit(w, name_helper)?;
        write_edge(w, &block_node_name, &stmt_node_name, None)?;
    }

    Ok(block_node_name)
}

fn write_arguments<W>(
    w: &mut W,
    name_helper: &NodeNameHelper,
    args: &[Argument],
) -> Result<String, io::Error>
where
    W: io::Write,
{
    let arg_node_name = name_helper.get_next_node_name();
    write_node_unescaped(w, &arg_node_name, "<{<I>args</I>}>", "ellipse")?;

    for arg in args {
        let type_node_name = name_helper.get_next_node_name();
        write_node(w, &type_node_name, &arg.r#type.kind.to_string(), "diamond")?;
        write_edge(w, &arg_node_name, &type_node_name, Some("type"))?;

        let name_node_name = name_helper.get_next_node_name();
        write_node(w, &name_node_name, &arg.name, "ellipse")?;
        write_edge(w, &arg_node_name, &name_node_name, Some("name"))?;
    }

    Ok(arg_node_name)
}

struct NodeNameHelper {
    count: Cell<usize>,
}

impl NodeNameHelper {
    pub fn new() -> Self {
        Self {
            count: Cell::new(0),
        }
    }

    pub fn get_next_node_name(&self) -> String {
        let id = self.count.replace(self.count.get() + 1);
        let name = format!("node{id}");
        name
    }
}
