use std::{fs, io::Read, path::Path, sync::Arc};

use clap::CommandFactory;
use inkwell::targets::TargetMachine;
use miette::{IntoDiagnostic, WrapErr};
use scopeguard::defer;

use floc::{analyzer, ast, cli, codegen, linker, llvm, parser, utils};

fn print_targets() -> miette::Result<()> {
    println!("Available LLVM targets:");

    let mut target = inkwell::targets::Target::get_first();

    while let Some(t) = target {
        let name = t
            .get_name()
            .to_str()
            .into_diagnostic()
            .wrap_err("invalid utf8 in target name")?;

        let description = t
            .get_description()
            .to_str()
            .into_diagnostic()
            .wrap_err("invalid utf8 in target description")?;

        println!("{name:<15} - {description}");

        target = t.get_next();
    }

    Ok(())
}

fn main() -> miette::Result<()> {
    let args = cli::parse();

    miette::set_hook(Box::new(|_| {
        Box::new(
            miette::MietteHandlerOpts::new()
                .with_syntax_highlighting(utils::SyntaxHighlighter)
                .build(),
        )
    }))?;

    inkwell::targets::Target::initialize_all(&inkwell::targets::InitializationConfig::default());

    if let Some(shell) = args.generate_shell_completion {
        let mut cmd = cli::Args::command();

        clap_complete::aot::generate(shell, &mut cmd, "floc", &mut std::io::stdout());

        return Ok(());
    }

    if args.print_targets {
        print_targets()?;

        return Ok(());
    }

    let source_file = args
        .source_file
        .clone()
        .wrap_err("no input file provided")?;

    let (source, filename) = if let Some("-") = source_file.to_str() {
        // Read from stdin
        let mut source = String::new();
        std::io::stdin()
            .read_to_string(&mut source)
            .into_diagnostic()
            .wrap_err("cannot read stdin")?;

        (source, "flo_out.flo")
    } else {
        // Read regular file

        let source = fs::read_to_string(&source_file)
            .into_diagnostic()
            .wrap_err_with(|| format!("cannot open source file '{}'", source_file.display()))?;

        let filename = Path::new(&source_file)
            .file_name()
            .expect("no filename")
            .to_str()
            .expect("invalid filename");

        (source, filename)
    };

    if args.verbose {
        eprintln!("-- parsing");
    }

    let named_source = miette::NamedSource::new(filename, source);

    let parser_timer = utils::Timer::start(args.verbose);

    if args.emit_ast || args.emit_ast_as_dot {
        let (ast_prog, diagnostics) = parser::parse_recover(named_source.inner().as_str());
        parser_timer.stop();

        let had_errors = !diagnostics.is_empty();
        if let Some(report) = parser::diagnostics_report(named_source.clone(), diagnostics) {
            eprintln!("{report:?}");
        }

        if let Some(ast_prog) = ast_prog {
            if args.emit_ast {
                println!("{ast_prog:#?}");
            }
            if args.emit_ast_as_dot {
                ast::dot::dump_graph(&ast_prog)
                    .into_diagnostic()
                    .wrap_err("cannot dump graph")?;
            }
        }

        if had_errors {
            std::process::exit(1);
        }
        return Ok(());
    }

    let (ast_prog, diagnostics) = parser::parse_recover(named_source.inner().as_str());

    parser_timer.stop();

    let had_parse_errors = !diagnostics.is_empty();
    if let Some(report) = parser::diagnostics_report(named_source.clone(), diagnostics) {
        eprintln!("{report:?}");
    }

    let Some(ast_prog) = ast_prog else {
        std::process::exit(1);
    };

    if args.verbose {
        eprintln!("-- analyzing");
    }

    let analyzer_timer = utils::Timer::start(args.verbose);

    let mut analyzer = analyzer::Analyzer::new(Arc::new(named_source));
    analyzer.analyze_program(&ast_prog);

    analyzer_timer.stop();

    for warning in analyzer.warnings() {
        eprintln!("{:?}", miette::Report::new(warning.clone()));
    }

    for error in analyzer.errors() {
        eprintln!("{:?}", miette::Report::new(error.clone()));
    }

    // Bail before codegen on any error, but only after every parse and analysis
    // diagnostic has been printed so the user sees them all at once.
    if had_parse_errors || !analyzer.errors().is_empty() {
        std::process::exit(1);
    }

    let module_name = filename.strip_suffix(".flo").unwrap_or(filename);

    if args.verbose {
        eprintln!("-- compiling");
    }

    let compiler_timer = utils::Timer::start(args.verbose);

    let default_target_triple = TargetMachine::get_default_triple();
    let target_triple = args
        .target_triple
        .as_deref()
        .unwrap_or_else(|| default_target_triple.as_str().to_str().unwrap());

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
        .optimize(&target_machine, args.optimization_level)
        .into_diagnostic()
        .wrap_err("cannot compile program")?;

    if args.emit_optimized_ir {
        codegen.dump_to_stderr();
        return Ok(());
    }

    if args.verbose {
        eprintln!("LLVM target triple is {target_triple}");
    }

    let target_arch = llvm::get_arch_from_target_triple(target_triple)
        .wrap_err_with(|| format!("unknown arch from target triple {target_triple}"))?;

    let is_msvc = llvm::is_msvc(target_triple);

    let (llvm_file_type, llvm_output_file, exec_output_file) =
        utils::get_output_files(&args, module_name, is_msvc);

    if args.verbose {
        eprintln!("output file is {}", llvm_output_file.display());
    }

    codegen
        .compile(&target_machine, &llvm_output_file, llvm_file_type)
        .into_diagnostic()
        .wrap_err("cannot compile program")?;

    compiler_timer.stop();

    if let Some(exec_output_file) = exec_output_file {
        // Remove the object file whatever happens
        defer! {
            let _ = fs::remove_file(llvm_output_file.clone());
        }

        if args.verbose {
            eprintln!("-- linking");
        }

        let linker_timer = utils::Timer::start(args.verbose);

        if is_msvc {
            linker::link_msvc(
                &llvm_output_file,
                &exec_output_file,
                args.link_static,
                &target_arch,
                args.verbose,
            )
        } else {
            linker::link_cc(
                &llvm_output_file,
                &exec_output_file,
                args.link_static,
                args.use_ld.as_deref(),
                args.verbose,
            )
        }
        .wrap_err("cannot link")?;

        linker_timer.stop();
    }

    Ok(())
}
