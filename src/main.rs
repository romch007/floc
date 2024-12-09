mod analyzer;
mod ast;
mod cli;
mod codegen;
mod parser;
mod utils;

use codegen::OptimizationLevelConvert;

use std::{
    ffi::OsStr,
    fs, io,
    path::{Path, PathBuf},
    process,
};

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("OS error: {0}")]
    Io(#[from] io::Error),

    #[error("Parsing error:\n{0}")]
    Parsing(#[from] Box<pest::error::Error<parser::Rule>>),

    #[error("Analyze error: {0}")]
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

macro_rules! os {
    ($val:expr) => {{
        OsStr::new($val)
    }};
}

fn run() -> Result<(), Error> {
    let args = cli::parse();
    let source = fs::read_to_string(&args.source_file)?;

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
        .expect("invalid filename")
        .strip_suffix(".flo")
        .unwrap_or("<unknown>");

    let llvm_context = inkwell::context::Context::create();
    let mut codegen = codegen::Compiler::new(&llvm_context, module_name);

    let declared_functions = analyzer.functions().values().collect::<Vec<_>>();

    codegen.declare_functions(&declared_functions[..]);
    codegen.emit_program(&ast_prog)?;

    if args.emit_ir {
        codegen.dump_to_stderr();
        return Ok(());
    }

    codegen.verify()?;

    let target_machine = codegen::Compiler::create_target_machine(
        args.target_triple.as_deref(),
        args.target_cpu.as_deref(),
        args.optimization_level.to_inkwell(),
    )?;

    codegen.optimize(&target_machine)?;

    if args.emit_optimized_ir {
        codegen.dump_to_stderr();
        return Ok(());
    }

    let target_triple = target_machine.get_triple();
    let target_triple = target_triple
        .as_str()
        .to_str()
        .expect("invalid utf8 in target triple");

    let on_windows = target_triple.contains("windows");

    let object_file_ext = if on_windows { "obj" } else { "o" };
    let object_file_name = format!("{module_name}.{object_file_ext}");
    let object_file = if args.compile {
        args.output
            .clone()
            .unwrap_or(PathBuf::from(object_file_name))
    } else {
        std::env::temp_dir().join(utils::tmpname(os!(""), os!(".o"), 10))
    };

    codegen.compile(&target_machine, &object_file)?;

    if args.compile {
        return Ok(());
    }

    let output_file = args.output.unwrap_or_else(|| {
        let default_path = if on_windows {
            format!("{module_name}.exe")
        } else {
            module_name.to_owned()
        };

        PathBuf::from(default_path)
    });

    let mut link_params = vec![
        object_file.as_os_str(),
        os!("-target"),
        target_triple.as_ref(),
        os!("-o"),
        output_file.as_ref(),
    ];

    if on_windows {
        // See https://learn.microsoft.com/en-us/cpp/porting/visual-cpp-change-history-2003-2015?view=msvc-170#stdio_and_conio
        link_params.push(os!("-llegacy_stdio_definitions"));
    }

    if let Some(additional_link_params) = &args.link_params {
        link_params.extend(additional_link_params.split(' ').map(OsStr::new));
    }

    let mut child = match process::Command::new("clang").args(&link_params).spawn() {
        Ok(child) => child,
        Err(ref e) if e.kind() == io::ErrorKind::NotFound => {
            eprintln!("Link failed: cannot find `clang`");
            process::exit(1);
        }
        Err(e) => return Err(e.into()),
    };

    let status = child.wait()?;

    if !status.success() {
        eprintln!("Link failed");
        process::exit(1);
    }

    let _ = fs::remove_file(&object_file);

    Ok(())
}
