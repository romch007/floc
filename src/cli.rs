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
    #[arg(short = 'O', long)]
    pub optimization_level: Option<OptimizationLevel>,

    /// Source file to compile
    pub source_file: String,

    /// Additional params to pass to clang at link time
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    pub link_params: Vec<String>,
}

#[derive(Debug, Clone, Default, ValueEnum)]
pub enum OptimizationLevel {
    None,
    Less,
    #[default]
    Default,
    Aggressive,
}

pub fn parse() -> Args {
    Args::parse()
}
