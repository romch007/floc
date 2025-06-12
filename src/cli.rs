pub mod args;

pub use args::Args;
use clap::CommandFactory;
use clap::FromArgMatches;
use clap::crate_version;
use std::sync::LazyLock;

static VERSION: LazyLock<String> = LazyLock::new(get_version);

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
    let command = Args::command();

    Args::from_arg_matches(&command.version(VERSION.as_str()).get_matches())
        .unwrap_or_else(|e| e.exit())
}
