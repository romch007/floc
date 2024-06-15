use clap::crate_version;
use clap::CommandFactory;
use clap::FromArgMatches;
use clap::Parser;
use clap::ValueEnum;
use lazy_static::lazy_static;

lazy_static! {
    static ref VERSION: String = get_version();
}

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

pub fn get_version() -> String {
    let llvm_version = inkwell::support::get_llvm_version();

    format!(
        "{} (using LLVM {}.{}.{})",
        crate_version!(),
        llvm_version.0,
        llvm_version.1,
        llvm_version.2
    )
}

#[derive(Debug, Clone, Default, ValueEnum)]
pub enum OptimizationLevel {
    None,
    Less,
    #[default]
    Default,
    Aggressive,
}

impl OptimizationLevel {
    pub fn to_inkwell(&self) -> inkwell::OptimizationLevel {
        match self {
            OptimizationLevel::None => inkwell::OptimizationLevel::None,
            OptimizationLevel::Less => inkwell::OptimizationLevel::Less,
            OptimizationLevel::Default => inkwell::OptimizationLevel::Default,
            OptimizationLevel::Aggressive => inkwell::OptimizationLevel::Aggressive,
        }
    }
}

pub fn parse() -> Args {
    let command = Args::command();

    Args::from_arg_matches(&command.version(VERSION.as_str()).get_matches())
        .unwrap_or_else(|e| e.exit())
}
