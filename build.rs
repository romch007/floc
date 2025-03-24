include!("src/cli/args.rs");

fn main() {
    use std::{env, fs};

    use clap::{CommandFactory, ValueEnum};
    use clap_complete::{generate_to, Shell};

    let shell_comp_out_dir = env::var("SHELL_COMPLETIONS_DIR")
        .or_else(|_| env::var("OUT_DIR"))
        .unwrap();

    let man_pages_out_dir = env::var("MAN_PAGES_DIR")
        .or_else(|_| env::var("OUT_DIR"))
        .unwrap();

    fs::create_dir_all(&shell_comp_out_dir).unwrap();

    let mut cmd = Args::command();
    for &shell in Shell::value_variants() {
        generate_to(shell, &mut cmd, "floc", &shell_comp_out_dir).unwrap();
    }

    fs::create_dir_all(&man_pages_out_dir).unwrap();

    let man = clap_mangen::Man::new(cmd);
    let mut buf = Vec::new();
    man.render(&mut buf).unwrap();

    std::fs::write(PathBuf::from(&man_pages_out_dir).join("floc.1"), buf).unwrap();
}
