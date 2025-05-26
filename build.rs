include!("src/cli/args.rs");

fn generate_shell_completion() {
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

use bindgen::callbacks::ParseCallbacks;

#[derive(Debug)]
struct CustomCallbacks;

impl ParseCallbacks for CustomCallbacks {
    fn enum_variant_name(
        &self,
        enum_name: Option<&str>,
        original_variant_name: &str,
        _variant_value: bindgen::callbacks::EnumVariantValue,
    ) -> Option<String> {
        if let Some("arch_t") = enum_name {
            if let Some(stripped) = original_variant_name.strip_prefix("arch_") {
                return Some(stripped.to_string());
            }
        }
        None
    }
}

fn generate_wrapper() {
    use std::env;

    let callbacks = CustomCallbacks;

    let bindings = bindgen::Builder::default()
        .header("llvm-wrapper/wrapper.h")
        .parse_callbacks(Box::new(callbacks))
        .default_enum_style(bindgen::EnumVariation::Rust {
            non_exhaustive: true,
        })
        .generate()
        .expect("cannot generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("llvm_wrapper.rs"))
        .expect("could not write bindings");
}

fn compile_wrapper() {
    cc::Build::new()
        .cpp(true)
        .file("llvm-wrapper/wrapper.cpp")
        .compile("llvm-wrapper");
}

fn main() {
    generate_wrapper();
    compile_wrapper();
    generate_shell_completion();
}
