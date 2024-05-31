mod ast;
mod cli;
mod codegen;
mod parser;

use std::{
    fs,
    io::{self, Write},
    process,
};

use ast::Node;
use clap::Parser as ClapParser;
use pest::Parser;

use parser::FloParser;

fn main() {
    let args = cli::Args::parse();
    let source = fs::read_to_string(&args.source_file).expect("cannot read file");

    let pest_output = FloParser::parse(parser::Rule::prog, &source);
    let mut pest_output = match pest_output {
        Ok(output) => output,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    };

    let ast_prog = ast::Program::parse(pest_output.next().unwrap());

    if args.emit_ast {
        ast_prog
            .debug_print(&mut io::stderr(), 0)
            .expect("cannot write debug output");

        return;
    }

    let module_name = args
        .source_file
        .file_name()
        .expect("no filename")
        .to_str()
        .expect("invalid filename");

    let llvm_context = inkwell::context::Context::create();
    let mut codegen = codegen::CodeGen::new(&llvm_context, module_name);
    codegen.emit_program(&ast_prog).expect("codegen error");

    if args.emit_ir {
        codegen.dump_to_stderr();
        return;
    }

    let ir = codegen.dump_to_string();

    let mut child = process::Command::new("clang")
        .args(["-x", "ir", "-"])
        .args(&args.clang_params)
        .stdin(process::Stdio::piped())
        .spawn()
        .expect("cannot run clang");

    let child_stdin = child.stdin.as_mut().expect("no stdin");
    child_stdin
        .write_all(ir.to_bytes())
        .expect("cannot write to stdin");

    let _status = child.wait().expect("cannot wait for child");
}
