use std::path::PathBuf;

use clap::crate_version;
use clap::CommandFactory;
use clap::FromArgMatches;
use clap::Parser;
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

    /// Source file to compile
    pub source_file: PathBuf,

    /// Additional params to pass to clang
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    pub clang_params: Vec<String>,
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

pub fn parse() -> Args {
    let command = Args::command();

    Args::from_arg_matches(&command.version(VERSION.as_str()).get_matches())
        .unwrap_or_else(|e| e.exit())
}
