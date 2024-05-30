use std::path::PathBuf;

use clap::Parser;

#[derive(Debug, Parser)]
#[clap(author, version, about)]
pub struct Args {
    #[arg(long, default_value_t = false)]
    pub emit_ast: bool,

    #[arg(long, default_value_t = false)]
    pub emit_ir: bool,

    pub source_file: PathBuf,

    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    pub clang_params: Vec<String>,
}
