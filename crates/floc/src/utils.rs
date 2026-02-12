use std::ffi::{OsStr, OsString};
use std::iter::repeat_with;
use std::time::Instant;

use crate::lexer;

#[cfg(feature = "codegen")]
use {crate::cli, std::path::PathBuf};

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

pub fn closest_str<'a, I>(iter: I, name: &str, max_distance: usize) -> Option<&'a str>
where
    I: Iterator<Item = &'a str>,
{
    let mut distances = iter
        .map(|existing_name| (existing_name, levenshtein(existing_name, name)))
        .filter(|pair| pair.1 < max_distance)
        .collect::<Vec<_>>();

    distances.sort_unstable_by_key(|pair| pair.1);

    distances.first().map(|pair| pair.0)
}

#[must_use]
pub fn levenshtein(a: &str, b: &str) -> usize {
    let mut result = 0;

    /* Shortcut optimizations / degenerate cases. */
    if a == b {
        return result;
    }

    let length_a = a.chars().count();
    let length_b = b.chars().count();

    if length_a == 0 {
        return length_b;
    }

    if length_b == 0 {
        return length_a;
    }

    /* Initialize the vector.
     *
     * This is why it’s fast, normally a matrix is used,
     * here we use a single vector. */
    let mut cache: Vec<usize> = (1..).take(length_a).collect();
    let mut distance_a;
    let mut distance_b;

    /* Loop. */
    for (index_b, code_b) in b.chars().enumerate() {
        result = index_b;
        distance_a = index_b;

        for (index_a, code_a) in a.chars().enumerate() {
            distance_b = if code_a == code_b {
                distance_a
            } else {
                distance_a + 1
            };

            distance_a = cache[index_a];

            result = if distance_a > result {
                if distance_b > result {
                    result + 1
                } else {
                    distance_b
                }
            } else if distance_b > distance_a {
                distance_a + 1
            } else {
                distance_b
            };

            cache[index_a] = result;
        }
    }

    result
}

/// Determine LLVM file output type, LLVM file output path and linker output path based on cli args
#[cfg(feature = "codegen")]
#[must_use]
pub fn get_output_files(
    args: &cli::args::Args,
    module_name: &str,
    is_msvc: bool,
) -> (inkwell::targets::FileType, PathBuf, Option<PathBuf>) {
    use std::env;

    if args.assemble {
        (
            inkwell::targets::FileType::Assembly,
            args.output
                .clone()
                .unwrap_or_else(|| format!("{module_name}.S").into()),
            None,
        )
    } else {
        let object_file_ext = if is_msvc { "obj" } else { "o" };
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

                if is_msvc {
                    path.set_extension("exe");
                }

                path
            });

            Some(file_path)
        };

        (inkwell::targets::FileType::Object, object_file, exec_file)
    }
}

pub struct Timer {
    start: Instant,
    verbose: bool,
}

impl Timer {
    #[must_use]
    pub fn start(verbose: bool) -> Self {
        Self {
            start: Instant::now(),
            verbose,
        }
    }

    pub fn stop(self) {
        if self.verbose {
            let elapsed = self.start.elapsed();

            println!("++ took {elapsed:?}");
        }
    }
}

pub struct SyntaxHighlighter;

struct SyntaxHighlighterState;

impl miette::highlighters::Highlighter for SyntaxHighlighter {
    fn start_highlighter_state<'h>(
        &'h self,
        _source: &dyn miette::SpanContents<'_>,
    ) -> Box<dyn miette::highlighters::HighlighterState + 'h> {
        Box::new(SyntaxHighlighterState)
    }
}

const DEFAULT_STYLE: owo_colors::Style = owo_colors::Style::new();
const INTEGER_STYLE: owo_colors::Style = owo_colors::Style::new().blue();
const OPERATOR_STYLE: owo_colors::Style = owo_colors::Style::new().yellow();
const KEYWORD_STYLE: owo_colors::Style = owo_colors::Style::new().magenta().bold();
const TYPE_STYLE: owo_colors::Style = owo_colors::Style::new().green();
const INVALID_STYLE: owo_colors::Style = owo_colors::Style::new().red().underline();

impl miette::highlighters::HighlighterState for SyntaxHighlighterState {
    fn highlight_line<'s>(&mut self, line: &'s str) -> Vec<owo_colors::Styled<&'s str>> {
        use logos::Logos;

        let mut styled = Vec::new();
        let mut lexer = lexer::Token::lexer(line);

        let mut last_end = 0;

        while let Some(res) = lexer.next() {
            let slice = &line[lexer.span()];
            let span = lexer.span();

            if last_end < span.end {
                let skipped = &line[last_end..span.start];
                styled.push(DEFAULT_STYLE.style(skipped));
            }

            let style = match res {
                Ok(lexer::Token::Integer(_) | lexer::Token::Boolean(_)) => INTEGER_STYLE,
                Ok(
                    lexer::Token::Eq
                    | lexer::Token::Neq
                    | lexer::Token::Add
                    | lexer::Token::Sub
                    | lexer::Token::Mul
                    | lexer::Token::Div
                    | lexer::Token::Mod
                    | lexer::Token::Lt
                    | lexer::Token::Lte
                    | lexer::Token::Gt
                    | lexer::Token::Gte
                    | lexer::Token::LogicOr
                    | lexer::Token::LogicAnd,
                ) => OPERATOR_STYLE,
                Ok(
                    lexer::Token::Read
                    | lexer::Token::Write
                    | lexer::Token::If
                    | lexer::Token::Else
                    | lexer::Token::Return
                    | lexer::Token::While,
                ) => KEYWORD_STYLE,
                Ok(lexer::Token::IntegerType | lexer::Token::BooleanType) => TYPE_STYLE,
                Ok(_) => DEFAULT_STYLE,
                Err(_) => INVALID_STYLE,
            };

            styled.push(style.style(slice));

            last_end = span.end;
        }

        styled
    }
}

#[cfg(test)]
mod tests {
    use std::{ffi::OsStr, path::PathBuf};

    use inkwell::targets::FileType;

    use super::*;
    use crate::cli;

    #[test]
    fn get_output_file_assembly() {
        let args = cli::args::Args {
            compile: false,
            assemble: true,
            output: None,
            source_file: Some("dontcare".into()),
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
            source_file: Some("dontcare".into()),
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
            source_file: Some("dontcare".into()),
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
            source_file: Some("dontcare".into()),
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
            source_file: Some("dontcare".into()),
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
            source_file: Some("dontcare".into()),
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
            source_file: Some("dontcare".into()),
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
            source_file: Some("dontcare".into()),
            ..Default::default()
        };

        let (file_type, _llvm_output, link_output) = get_output_files(&args, "testing", true);
        assert_eq!(file_type, FileType::Object);
        assert_eq!(link_output, Some(PathBuf::from("anything.docx")));
    }
}
