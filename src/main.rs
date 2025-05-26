mod analyzer;
mod ast;
mod cli;
mod codegen;
mod linker;
mod llvm_wrapper;
mod parser;
mod utils;

use std::{fs, io::Read, path::Path};

use miette::{IntoDiagnostic, WrapErr};
use scopeguard::defer;

fn main() -> miette::Result<()> {
    let args = cli::parse();

    let (source, filename) = if let Some("-") = args.source_file.to_str() {
        // Read from stdin
        let mut source = String::new();
        std::io::stdin()
            .read_to_string(&mut source)
            .into_diagnostic()
            .wrap_err("cannot read stdin")?;

        (source, "flo_out.flo")
    } else {
        // Read regular file

        let source = fs::read_to_string(&args.source_file)
            .into_diagnostic()
            .wrap_err_with(|| {
                format!("cannot open source file '{}'", args.source_file.display())
            })?;

        let filename = Path::new(&args.source_file)
            .file_name()
            .expect("no filename")
            .to_str()
            .expect("invalid filename");

        (source, filename)
    };

    let named_source = miette::NamedSource::new(filename, source);

    let ast_prog = parser::parse(named_source.clone())?;

    if args.emit_ast {
        println!("{ast_prog:#?}");
        return Ok(());
    }

    if args.emit_ast_as_dot {
        ast::dot::dump_graph(&ast_prog).unwrap();
        return Ok(());
    }

    let mut analyzer = analyzer::Analyzer::new(named_source.clone());
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

    let target_arch = utils::get_arch_from_target_triple(target_triple);
    let is_msvc = utils::is_msvc(target_triple);

    let (llvm_file_type, llvm_output_file, exec_output_file) =
        utils::get_output_files(&args, module_name, is_msvc);

    codegen
        .compile(&target_machine, &llvm_output_file, llvm_file_type)
        .into_diagnostic()
        .wrap_err("cannot compile program")?;

    if let Some(exec_output_file) = exec_output_file {
        // Remove the object file whatever happens
        defer! {
            let _ = fs::remove_file(llvm_output_file.clone());
        }

        if is_msvc {
            linker::link_msvc(
                &llvm_output_file,
                &exec_output_file,
                args.link_static,
                &target_arch,
            )
        } else {
            linker::link_cc(
                args.linker.as_deref().unwrap_or("cc"),
                &llvm_output_file,
                &exec_output_file,
                args.link_static,
            )
        }
        .wrap_err("cannot link")?;
    }

    Ok(())
}
