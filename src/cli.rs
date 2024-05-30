use std::path::PathBuf;

use clap::Parser;

#[derive(Debug, Parser)]
#[clap(author, version, about)]
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
