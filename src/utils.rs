use std::env;
use std::ffi::{OsStr, OsString};
use std::iter::repeat_with;
use std::path::PathBuf;

use inkwell::targets::FileType;

use crate::cli;

pub fn tmpname(prefix: &OsStr, suffix: &OsStr, rand_len: usize) -> OsString {
    let capacity = prefix
        .len()
        .saturating_add(suffix.len())
        .saturating_add(rand_len);
    let mut buf = OsString::with_capacity(capacity);
    buf.push(prefix);
    let mut char_buf = [0u8; 4];
    for c in repeat_with(fastrand::alphanumeric).take(rand_len) {
        buf.push(c.encode_utf8(&mut char_buf));
    }
    buf.push(suffix);
    buf
}

/// Determine LLVM file output type, LLVM file output path and linker output path based on cli args
pub fn get_output_files(
    args: &cli::args::Args,
    module_name: &str,
    on_windows: bool,
) -> (FileType, PathBuf, Option<PathBuf>) {
    if args.assemble {
        (
            FileType::Assembly,
            args.output
                .clone()
                .unwrap_or_else(|| format!("{module_name}.S").into()),
            None,
        )
    } else {
        let object_file_ext = if on_windows { "obj" } else { "o" };
        let object_file = if args.compile {
            args.output
                .clone()
                .unwrap_or_else(|| format!("{module_name}.{object_file_ext}").into())
        } else {
            env::temp_dir().join(tmpname(
                OsStr::new(""),
                OsStr::new(&format!(".{object_file_ext}")),
                10,
            ))
        };

        let exec_file = if args.compile {
            None
        } else {
            let file_path = args.output.clone().unwrap_or_else(|| {
                let mut path = PathBuf::from(module_name);

                if on_windows {
                    path.set_extension("exe");
                }

                path
            });

            Some(file_path)
        };

        (FileType::Object, object_file, exec_file)
    }
}

#[cfg(test)]
mod tests {
    use std::{ffi::OsStr, path::PathBuf};

    use inkwell::targets::FileType;

    use super::get_output_files;
    use crate::cli;

    #[test]
    fn get_output_file_assembly() {
        let args = cli::args::Args {
            compile: false,
            assemble: true,
            output: None,
            source_file: "dontcare".into(),
            ..Default::default()
        };

        let (file_type, llvm_output, link_output) = get_output_files(&args, "testing", false);
        assert_eq!(file_type, FileType::Assembly);
        assert_eq!(llvm_output, PathBuf::from("testing.S"));
        assert!(link_output.is_none());
    }

    #[test]
    fn get_output_file_assembly_custom() {
        let args = cli::args::Args {
            compile: false,
            assemble: true,
            output: Some("afile.txt".into()),
            source_file: "dontcare".into(),
            ..Default::default()
        };

        let (file_type, llvm_output, link_output) = get_output_files(&args, "testing", false);
        assert_eq!(file_type, FileType::Assembly);
        assert_eq!(llvm_output, PathBuf::from("afile.txt"));
        assert!(link_output.is_none());
    }

    #[test]
    fn get_output_file_compile() {
        let args = cli::args::Args {
            compile: true,
            assemble: false,
            output: None,
            source_file: "dontcare".into(),
            ..Default::default()
        };

        let (file_type, llvm_output, link_output) = get_output_files(&args, "testing", false);
        assert_eq!(file_type, FileType::Object);
        assert_eq!(llvm_output, PathBuf::from("testing.o"));
        assert!(link_output.is_none());
    }

    #[test]
    fn get_output_file_compile_windows() {
        let args = cli::args::Args {
            compile: true,
            assemble: false,
            output: None,
            source_file: "dontcare".into(),
            ..Default::default()
        };

        let (file_type, llvm_output, link_output) = get_output_files(&args, "testing", true);
        assert_eq!(file_type, FileType::Object);
        assert_eq!(llvm_output, PathBuf::from("testing.obj"));
        assert!(link_output.is_none());
    }

    #[test]
    fn get_output_file_compile_custom() {
        let args = cli::args::Args {
            compile: true,
            assemble: false,
            output: Some("something.mp4".into()),
            source_file: "dontcare".into(),
            ..Default::default()
        };

        let (file_type, llvm_output, link_output) = get_output_files(&args, "testing", false);
        assert_eq!(file_type, FileType::Object);
        assert_eq!(llvm_output, PathBuf::from("something.mp4"));
        assert!(link_output.is_none());
    }

    #[test]
    fn get_output_file_link() {
        let args = cli::args::Args {
            compile: false,
            assemble: false,
            output: None,
            source_file: "dontcare".into(),
            ..Default::default()
        };

        let (file_type, llvm_output, link_output) = get_output_files(&args, "testing", false);
        assert_eq!(file_type, FileType::Object);
        assert_eq!(llvm_output.extension(), Some(OsStr::new("o")));
        assert_eq!(link_output, Some(PathBuf::from("testing")));
    }

    #[test]
    fn get_output_file_link_windows() {
        let args = cli::args::Args {
            compile: false,
            assemble: false,
            output: None,
            source_file: "dontcare".into(),
            ..Default::default()
        };

        let (file_type, llvm_output, link_output) = get_output_files(&args, "testing", true);
        assert_eq!(file_type, FileType::Object);
        assert_eq!(llvm_output.extension(), Some(OsStr::new("obj")));
        assert_eq!(link_output, Some(PathBuf::from("testing.exe")));
    }

    #[test]
    fn get_output_file_link_custom() {
        let args = cli::args::Args {
            compile: false,
            assemble: false,
            output: Some("anything.docx".into()),
            source_file: "dontcare".into(),
            ..Default::default()
        };

        let (file_type, _llvm_output, link_output) = get_output_files(&args, "testing", true);
        assert_eq!(file_type, FileType::Object);
        assert_eq!(link_output, Some(PathBuf::from("anything.docx")));
    }
}
