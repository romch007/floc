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

    /// LLVM target triple
    #[arg(short, long)]
    pub target_triple: Option<String>,

    /// LLVM target CPU
    #[arg(long)]
    pub target_cpu: Option<String>,

    /// Optimization level
    #[arg(short = 'O', long, value_enum, default_value_t = OptimizationLevel::Default)]
    pub optimization_level: OptimizationLevel,

    /// Source file to compile
    pub source_file: PathBuf,

    /// Additional params to pass to clang at link time
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    pub link_params: Vec<String>,
}

#[derive(Debug, Clone, Default, ValueEnum)]
pub enum OptimizationLevel {
    #[value(name = "0")]
    None,

    #[value(name = "1")]
    Less,

    #[value(name = "2")]
    #[default]
    Default,

    #[value(name = "3")]
    Aggressive,
}

pub fn parse() -> Args {
    Args::parse()
}
