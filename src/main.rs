mod analyzer;
mod ast;
mod cli;
mod codegen;
mod parser;

use codegen::OptimizationLevelConvert;

use std::{
    fs,
    io::{self, Read},
    path::Path,
    process,
};

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("OS error:\n{0}")]
    Io(#[from] io::Error),

    #[error("Parsing error:\n{0}")]
    Parsing(#[from] Box<pest::error::Error<parser::Rule>>),

    #[error("Analyze error:\n{0}")]
    Analyze(#[from] analyzer::Error),

    #[error("Compiler error: {0}")]
    Compiler(#[from] codegen::Error),
}

fn main() {
    if let Err(err) = run() {
        eprintln!("{err}");
        process::exit(1);
    }
}

fn run() -> Result<(), Error> {
    let args = cli::parse();
    let source = match args.source_file.as_str() {
        "-" => {
            let mut input = String::new();
            io::stdin().read_to_string(&mut input).map(|_| input)?
        }
        file => fs::read_to_string(file)?,
    };

    let ast_prog = parser::parse(&source)?;

    if args.emit_ast {
        println!("{ast_prog:#?}");
        return Ok(());
    }

    let mut analyzer = analyzer::Analyzer::new();
    analyzer.analyze_program(&ast_prog)?;

    let module_name = Path::new(&args.source_file)
        .file_name()
        .expect("no filename")
        .to_str()
        .expect("invalid filename");

    let llvm_context = inkwell::context::Context::create();
    let mut codegen = codegen::Compiler::new(&llvm_context, module_name);

    let declared_functions = analyzer.functions.values().collect::<Vec<_>>();

    codegen.declare_functions(&declared_functions[..]);
    codegen.emit_program(&ast_prog)?;

    if args.emit_ir {
        codegen.dump_to_stderr();
        return Ok(());
    }

    codegen.verify()?;

    let object_file = tempfile::Builder::new().suffix(".o").tempfile()?;

    codegen.compile(
        args.target_triple.as_deref(),
        args.target_cpu.as_deref(),
        args.optimization_level.map(|opti| opti.to_inkwell()),
        object_file.path(),
    )?;

    let mut compilation_params = vec![object_file.path().as_os_str()];

    if cfg!(target_os = "windows") {
        // See https://learn.microsoft.com/en-us/cpp/porting/visual-cpp-change-history-2003-2015?view=msvc-170#stdio_and_conio
        compilation_params.push("-llegacy_stdio_definitions".as_ref());
    }

    let mut child = process::Command::new("clang")
        .args(&compilation_params)
        .args(&args.link_params)
        .spawn()?;

    let _status = child.wait()?;

    Ok(())
}
