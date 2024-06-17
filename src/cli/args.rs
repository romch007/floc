use std::path::PathBuf;

use clap::Parser;
use clap::ValueEnum;

#[derive(Debug, Parser)]
#[clap(author, about, version)]
pub struct Args {
    /// Print the parsed AST and exit
    #[arg(long, default_value_t = false)]
    pub emit_ast: bool,

    /// Print the generated LLVM IR and exit
    #[arg(long, default_value_t = false)]
    pub emit_ir: bool,

    /// Print the optimized LLVM IR and exit
    #[arg(long, default_value_t = false)]
    pub emit_optimized_ir: bool,

    /// LLVM target triple
    #[arg(short, long)]
    pub target_triple: Option<String>,

    /// LLVM target CPU
    #[arg(long)]
    pub target_cpu: Option<String>,

    /// Optimization level
    #[arg(short = 'O', long, value_enum, default_value_t = OptimizationLevel::Default)]
    pub optimization_level: OptimizationLevel,

    // Only compile, do not link
    #[arg(short)]
    pub compile: bool,

    /// Output executable
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    /// Additional params to pass to clang at link time
    #[arg(long)]
    pub link_params: Option<String>,

    /// Source file to compile
    pub source_file: PathBuf,
}

#[derive(Debug, Clone, Default, ValueEnum)]
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
