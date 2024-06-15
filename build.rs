include!("src/cli.rs");

fn main() {
    use std::{env, fs};

    use clap::{CommandFactory, ValueEnum};
    use clap_complete::{generate_to, Shell};

    let out_dir = env::var("SHELL_COMPLETIONS_DIR")
        .or_else(|_| env::var("OUT_DIR"))
        .unwrap();

    fs::create_dir_all(&out_dir).unwrap();

    let mut cmd = Args::command();
    for &shell in Shell::value_variants() {
        generate_to(shell, &mut cmd, "floc", &out_dir).unwrap();
    }
}
