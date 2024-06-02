mod ast;

mod cli;
mod codegen;
mod parser;

use std::{
    fs,
    io::{self, Read, Write},
    path::Path,
    process,
};

use ast::Node;
use pest::Parser;

use parser::FloParser;

fn main() {
    let args = cli::parse();
    let source = match args.source_file.as_str() {
        "-" => {
            let mut input = String::new();
            io::stdin()
                .read_to_string(&mut input)
                .expect("cannot read stdin");
            input
        }
        file => fs::read_to_string(file).expect("cannot read file"),
    };

    let pest_output = FloParser::parse(parser::Rule::prog, &source);
    let mut pest_output = match pest_output {
        Ok(output) => output,
        Err(err) => {
            eprintln!("{err}");
            process::exit(1);
        }
    };

    let ast_prog = ast::Program::parse(pest_output.next().unwrap());

    if args.emit_ast {
        eprintln!("{ast_prog:#?}");
        return;
    }

    let module_name = Path::new(&args.source_file)
        .file_name()
        .expect("no filename")
        .to_str()
        .expect("invalid filename");

    let llvm_context = inkwell::context::Context::create();
    let mut codegen = codegen::CodeGen::new(&llvm_context, module_name);
    if let Err(error) = codegen.emit_program(&ast_prog) {
        eprintln!("Compilation error: ");
        eprintln!("{error}");
        process::exit(1);
    }

    if args.emit_ir {
        codegen.dump_to_stderr();
        return;
    }

    let ir = codegen.dump_to_string();
    let mut compilation_params = vec!["-x", "ir", "-", "-Wno-override-module"];

    if cfg!(target_os = "windows") {
        // See https://learn.microsoft.com/en-us/cpp/porting/visual-cpp-change-history-2003-2015?view=msvc-170#stdio_and_conio
        compilation_params.push("-llegacy_stdio_definitions");
    }

    let mut child = process::Command::new("clang")
        .args(&compilation_params)
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
