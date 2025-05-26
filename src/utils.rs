use std::env;
use std::ffi::{CString, OsStr, OsString};
use std::iter::repeat_with;
use std::path::PathBuf;

use inkwell::targets::FileType;

use crate::{cli, llvm_wrapper};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

pub trait SpanIterExt {
    /// Merges a sequence of `Span` references into a single `Span`.
    ///
    /// The method computes a new `Span` that spans from the start of the first `Span`
    /// to the end of the last `Span` in the iterator. If the iterator contains only one
    /// `Span`, it returns that `Span` directly. If the iterator is empty, it returns `None`.
    fn merge_spans(self) -> Option<Span>;
}

impl<'a, T> SpanIterExt for T
where
    T: Iterator<Item = &'a Span>,
{
    fn merge_spans(mut self) -> Option<Span> {
        if let Some(first) = self.next() {
            if let Some(last) = self.last() {
                Some(Span {
                    start: first.start,
                    end: last.end,
                })
            } else {
                // Only one span, return it
                Some(*first)
            }
        } else {
            // No spans
            None
        }
    }
}

impl<'a> From<pest::Span<'a>> for Span {
    fn from(value: pest::Span<'a>) -> Self {
        Self {
            start: value.start(),
            end: value.end(),
        }
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(value: Span) -> Self {
        (value.start, value.end - value.start).into()
    }
}

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
pub fn get_output_files(
    args: &cli::args::Args,
    module_name: &str,
    is_msvc: bool,
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

        (FileType::Object, object_file, exec_file)
    }
}

pub type Arch = llvm_wrapper::arch_t;

pub fn get_arch_from_target_triple(target_triple: &str) -> Arch {
    let target_triple = CString::new(target_triple).unwrap();

    unsafe { llvm_wrapper::arch_from_target_triple(target_triple.as_ptr()) }
}

pub fn is_msvc(target_triple: &str) -> bool {
    let target_triple = CString::new(target_triple).unwrap();

    let ret = unsafe { llvm_wrapper::is_msvc(target_triple.as_ptr()) };

    ret != 0
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

    use super::{Span, SpanIterExt};

    #[test]
    fn merge_spans_multiple() {
        let spans = [
            Span { start: 1, end: 5 },
            Span { start: 6, end: 10 },
            Span { start: 11, end: 15 },
        ];

        let merged_span = spans.iter().merge_spans();
        assert_eq!(merged_span, Some(Span { start: 1, end: 15 }));
    }

    #[test]
    fn merge_spans_single() {
        let spans = [Span { start: 3, end: 7 }];

        let merged_span = spans.iter().merge_spans();
        assert_eq!(merged_span, Some(Span { start: 3, end: 7 }));
    }

    #[test]
    fn merge_spans_empty() {
        let spans = [];

        let merged_span = spans.iter().merge_spans();
        assert_eq!(merged_span, None);
    }
}
