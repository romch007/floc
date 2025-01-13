mod analyzer;
mod ast;
mod cli;
mod codegen;
mod parser;
mod utils;

use std::{ffi::OsStr, fs, io, path::Path, process};

use miette::{IntoDiagnostic, WrapErr};
use scopeguard::defer;

macro_rules! os {
    ($val:expr) => {{
        OsStr::new($val)
    }};
}

fn main() -> miette::Result<()> {
    let args = cli::parse();
    let source = fs::read_to_string(&args.source_file)
        .into_diagnostic()
        .wrap_err("cannot open source file")?;

    let ast_prog = parser::parse(&source)
        .into_diagnostic()
        .wrap_err("cannot parse source code")?;

    if args.emit_ast {
        println!("{ast_prog:#?}");
        return Ok(());
    }

    if args.emit_ast_as_dot {
        ast::dot::dump_graph(&ast_prog).unwrap();
        return Ok(());
    }

    let filename = Path::new(&args.source_file)
        .file_name()
        .expect("no filename")
        .to_str()
        .expect("invalid filename");

    let mut analyzer = analyzer::Analyzer::new(miette::NamedSource::new(filename, source));
    analyzer
        .analyze_program(&ast_prog)
        .map_err(|boxed_err| *boxed_err)?;

    for warning in analyzer.warnings() {
        eprintln!("{warning:?}");
    }

    let module_name = filename.strip_suffix(".flo").unwrap_or("<unknown>");

    let llvm_context = inkwell::context::Context::create();
    let mut codegen = codegen::Compiler::new(&llvm_context, module_name);

    let declared_functions = analyzer.functions().values().collect::<Vec<_>>();

    codegen.declare_functions(&declared_functions[..]);
    codegen
        .emit_program(&ast_prog)
        .into_diagnostic()
        .wrap_err("cannot compile program")?;

    if args.emit_ir {
        codegen.dump_to_stderr();
        return Ok(());
    }

    codegen
        .verify()
        .into_diagnostic()
        .wrap_err("cannot compile program")?;

    let target_machine = codegen::Compiler::create_target_machine(
        args.target_triple.as_deref(),
        args.target_cpu.as_deref(),
        args.target_features.as_deref(),
        args.optimization_level.into(),
    )
    .into_diagnostic()
    .wrap_err("cannot create target matchine")?;

    codegen
        .optimize(&target_machine)
        .into_diagnostic()
        .wrap_err("cannot compile program")?;

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

    let (llvm_file_type, llvm_output_file, exec_output_file) =
        utils::get_output_files(&args, module_name, on_windows);

    codegen
        .compile(&target_machine, &llvm_output_file, llvm_file_type)
        .into_diagnostic()
        .wrap_err("cannot compile program")?;

    if let Some(exec_output_file) = exec_output_file {
        let mut link_params = vec![
            llvm_output_file.as_os_str(),
            os!("-target"),
            target_triple.as_ref(),
            os!("-o"),
            exec_output_file.as_ref(),
        ];

        if on_windows {
            // See https://learn.microsoft.com/en-us/cpp/porting/visual-cpp-change-history-2003-2015?view=msvc-170#stdio_and_conio
            link_params.push(os!("-llegacy_stdio_definitions"));
        }

        if let Some(additional_link_params) = &args.link_params {
            link_params.extend(additional_link_params.split(' ').map(OsStr::new));
        }

        // Remove the object file whatever happens
        defer! {
            let _ = fs::remove_file(llvm_output_file.clone());
        }

        let mut child = match process::Command::new("clang").args(&link_params).spawn() {
            Ok(child) => child,
            Err(ref e) if e.kind() == io::ErrorKind::NotFound => {
                eprintln!("Link failed: cannot find `clang`");
                process::exit(1);
            }
            Err(e) => Err(e).into_diagnostic().wrap_err("cannot link program")?,
        };

        let status = child
            .wait()
            .into_diagnostic()
            .wrap_err("cannot link program")?;

        if !status.success() {
            eprintln!("Link failed");
            process::exit(1);
        }
    }

    Ok(())
}
