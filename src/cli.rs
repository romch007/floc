pub mod args;

use args::Args;
use clap::crate_version;
use clap::CommandFactory;
use clap::FromArgMatches;
use std::sync::OnceLock;

static VERSION: OnceLock<String> = OnceLock::new();

fn get_version() -> String {
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
    let version = VERSION.get_or_init(get_version);
    let command = Args::command();

    Args::from_arg_matches(&command.version(version.as_str()).get_matches())
        .unwrap_or_else(|e| e.exit())
}
