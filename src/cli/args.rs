use std::path::PathBuf;

use clap::Parser;
use clap::ValueEnum;

#[derive(Debug, Parser, Default)]
#[clap(author, about, version)]
pub struct Args {
    /// Print the parsed AST and exit
    #[arg(long, default_value_t = false)]
    pub emit_ast: bool,

    /// Generate a dot graph for the AST
    #[arg(long, default_value_t = false)]
    pub emit_ast_as_dot: bool,

    /// Print the generated LLVM IR and exit
    #[arg(long, default_value_t = false)]
    pub emit_ir: bool,

    /// Print the optimized LLVM IR and exit
    #[arg(long, default_value_t = false)]
    pub emit_optimized_ir: bool,

    /// Print available targets and exit
    #[arg(long, default_value_t = false)]
    pub print_targets: bool,

    /// Generate shell completion for the given shell
    #[arg(long)]
    pub generate_shell_completion: Option<clap_complete::aot::Shell>,

    /// LLVM target triple
    #[arg(short, long)]
    pub target_triple: Option<String>,

    /// LLVM target CPU
    #[arg(long)]
    pub target_cpu: Option<String>,

    /// LLVM target features
    #[arg(long)]
    pub target_features: Option<String>,

    /// Optimization level
    #[arg(short = 'O', long, value_enum, default_value_t = OptimizationLevel::Default)]
    pub optimization_level: OptimizationLevel,

    /// Only compile, do not link
    #[arg(short, long)]
    pub compile: bool,

    /// Only assemble, do not compile nor link
    #[arg(short = 'S', long)]
    pub assemble: bool,

    /// Verbose output
    #[arg(short = 'v', long)]
    pub verbose: bool,

    /// Linker to execute (ignored when using MSVC)
    #[arg(long)]
    pub linker: Option<String>,

    /// Statically link program
    #[arg(long = "static", default_value_t = false)]
    pub link_static: bool,

    /// Output executable
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    /// Source file to compile
    pub source_file: Option<PathBuf>,
}

#[derive(Debug, Clone, Copy, Default, ValueEnum)]
pub enum OptimizationLevel {
    /// None
    #[value(name = "0")]
    None,

    /// Less
    #[value(name = "1")]
    Less,

    /// Default
    #[value(name = "2")]
    #[default]
    Default,

    /// Aggressive
    #[value(name = "3")]
    Aggressive,
}
