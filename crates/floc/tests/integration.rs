use std::{
    env,
    ffi::OsStr,
    fs,
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Output, Stdio},
};

use floc::utils::tmpname;

fn random_exec_name() -> PathBuf {
    let random_name = tmpname(
        OsStr::new("floc_test_"),
        if cfg!(target_os = "windows") {
            OsStr::new(".exe")
        } else {
            OsStr::new("")
        },
        10,
    );

    env::temp_dir().join(random_name)
}

type TempFile = scopeguard::ScopeGuard<PathBuf, fn(PathBuf)>;

fn remove_file(path: PathBuf) {
    let _ = fs::remove_file(path);
}

/// Compile `program` (fed through stdin) to a temporary executable.
fn compile(program: &str, flags: &[&str]) -> TempFile {
    let exec_path: TempFile = scopeguard::guard(random_exec_name(), remove_file);

    let mut compiler = Command::new(env!("CARGO_BIN_EXE_floc"))
        .arg("-")
        .args(flags)
        .arg("-o")
        .arg(exec_path.as_path())
        .stdin(Stdio::piped())
        .stderr(Stdio::inherit()) // show errors in test output
        .stdout(Stdio::null())
        .spawn()
        .expect("cannot run compiler");

    compiler
        .stdin
        .take()
        .expect("cannot get stdin of compiler process")
        .write_all(program.as_bytes())
        .expect("cannot write program to stdin");

    let status = compiler.wait().expect("failed to wait on compiler");
    assert!(status.success(), "compiler returned non-zero exit code");

    exec_path
}

/// Run `exec_path`, feeding each `input` item on its own line, then closing stdin.
fn run(exec_path: &Path, input: &[String]) -> Output {
    let mut binary = Command::new(exec_path)
        .stdin(Stdio::piped())
        .stderr(Stdio::null())
        .stdout(Stdio::piped())
        .spawn()
        .expect("cannot run compiled binary");

    let mut stdin = binary.stdin.take().expect("cannot get stdin of binary");
    let input = input
        .iter()
        .map(|line| format!("{line}\n"))
        .collect::<String>();

    // The binary may exit without reading everything, so write errors are fine.
    let writer = std::thread::spawn(move || {
        let _ = stdin.write_all(input.as_bytes());
    });

    let output = binary
        .wait_with_output()
        .expect("failed to wait for binary");
    writer.join().unwrap();

    output
}

fn output_numbers(output: &Output) -> Vec<i64> {
    String::from_utf8(output.stdout.clone())
        .expect("invalid UTF-8 in program output")
        .lines()
        .map(|line| line.parse::<i64>())
        .collect::<Result<Vec<_>, _>>()
        .expect("output is not a number")
}

macro_rules! testprogram {
    ($name:ident, flags = [$($flag:expr),*], $program:expr, $expected:expr $(, $input:expr)?) => {
        #[test]
        fn $name() {
            let exec_path = compile($program, &[$($flag),*]);

            let input: Vec<String> = None
                $(.or(Some($input.iter().map(ToString::to_string).collect())))?
                .unwrap_or_default();

            let output = run(&exec_path, &input);

            assert_eq!($expected.to_vec(), output_numbers(&output));
        }
    };
    ($name:ident, $program:expr, $expected:expr $(, $input:expr)?) => {
        testprogram! { $name, flags = [], $program, $expected $(, $input)? }
    };
}

testprogram! { simple_write, "ecrire(5); ecrire(4);", [5, 4] }
testprogram! { simple_read, "ecrire(-lire());", [5], [-5] }

testprogram! { big_expr_ints, "ecrire(25 * 2 + 10 / 5 - 40 % 7 + 15 * 2 - 8 * 3 + 12 * 2 - 10 / 4 + 5 + 100 / 25 - 18 % 5 + 9 * 2 - 45 / 9 + 3 * 3);", [103] }
testprogram! { big_expr_bools, "ecrire(Vrai et Faux ou non Vrai et (Faux ou Vrai) et (non (Vrai ou Faux)) et (Faux et non Vrai) ou (Vrai et (Faux ou Vrai)) et non (Faux ou Vrai) et Vrai);", [0] }

testprogram! { example_ackermann, include_str!("../../../flo-examples/ackermann.flo"), [1021] }

testprogram! { nested_ifs, "
entier test_case(entier m, entier n) {
  si (m == 0) {
    ecrire(n + 5);
    retourner n * 2;
  } sinon si (n == 0) {
    ecrire(m - 2);
    retourner test_case(m - 1, 5);
  } sinon si (m == 1) {
    si (n == 1) {
      ecrire(10);
      retourner 100;
    } sinon si (n == 2) {
      ecrire(20);
      retourner 200;
    } sinon {
      ecrire(30);
      retourner 300;
    }
  } sinon si (m == 2) {
    ecrire(40);
    retourner test_case(m - 1, n - 1);
  } sinon {
    ecrire(50);
    retourner test_case(m - 2, n - 2);
  }
}

ecrire(test_case(3, 3));", [50, 10, 100] }

testprogram! { example_power, include_str!("../../../flo-examples/power.flo"), [65536], [2, 16] }

testprogram! { write_booleans, "ecrire(Vrai); ecrire(Faux); ecrire(1 == 1);", [1, 0, 1] }
testprogram! { et_before_ou, "ecrire(Vrai ou Vrai et Faux); ecrire(Faux et Vrai ou Vrai);", [1, 1] }
testprogram! { integer_bounds, "ecrire(9223372036854775807); ecrire(-9223372036854775808);", [9223372036854775807i64, -9223372036854775808i64] }

const NO_INPUT: [i64; 0] = [];

testprogram! { declaration_in_long_loop, flags = ["-O", "0"], "
entier i = 0;
tantque (i < 10000000) {
  entier x = i;
  i = x + 1;
}
ecrire(i);", [10000000] }

testprogram! { read_in_long_loop, "
entier i = 0;
entier s = 0;
tantque (i < 1000000) {
  s = s + lire();
  i = i + 1;
}
ecrire(s);", [0], NO_INPUT }

testprogram! { read_invalid_input_returns_zero, "ecrire(lire()); ecrire(lire());", [0, 0], ["abc"] }

testprogram! { declaration_without_value_is_zero, "
booleen b;
ecrire(b);
entier i = 0;
tantque (i < 3) {
  entier x;
  ecrire(x);
  x = 5;
  i = i + 1;
}", [0, 0, 0, 0] }

testprogram! { logic_evaluates_right_side_only_when_needed, "
booleen t(entier n) {
  ecrire(n);
  retourner Vrai;
}

booleen r = Faux et t(1);
r = Vrai et t(2);
r = Vrai ou t(3);
r = Faux ou t(4);
ecrire(r);", [2, 4, 1] }

testprogram! { logic_short_circuits_division, "
entier b = 0;
si (b != 0 et 10 / b > 1) { ecrire(1); } sinon { ecrire(2); }
si (b == 0 ou 10 / b > 1) { ecrire(3); } sinon { ecrire(4); }", [2, 3] }

testprogram! { division_min_by_minus_one_wraps, "
entier a = lire();
entier b = lire();
ecrire(a / b);
ecrire(a % b);", [i64::MIN, 0], [i64::MIN, -1] }

testprogram! { division_by_negative_divisor, "
ecrire(lire() / lire());
ecrire(lire() % lire());", [-3, 1], [7, -2, 7, -2] }

#[test]
fn division_by_zero_aborts() {
    for program in ["ecrire(10 / lire());", "ecrire(10 % lire());"] {
        let exec_path = compile(program, &[]);
        let output = run(&exec_path, &["0".to_string()]);
        assert!(!output.status.success(), "{program} did not abort");
    }
}

#[test]
fn division_by_zero_keeps_previous_output() {
    let exec_path = compile(
        "ecrire(1);
ecrire(2 / lire());",
        &[],
    );
    let output = run(&exec_path, &["0".to_string()]);

    assert!(!output.status.success());
    assert_eq!(output_numbers(&output), [1]);
}

testprogram! { user_functions_named_like_libc, "
entier exit(entier n) { retourner n + 1; }
entier puts(entier n) { retourner n * 2; }
entier fflush(entier n) { retourner n - 1; }
ecrire(exit(1));
ecrire(puts(5));
ecrire(fflush(5));
ecrire(1 / lire());", [2, 10, 4, 1], [1] }

#[test]
fn refuses_to_overwrite_source() {
    let dir = env::temp_dir().join(tmpname(OsStr::new("floc_test_"), OsStr::new(""), 10));
    fs::create_dir(&dir).unwrap();
    let _cleanup = scopeguard::guard(dir.clone(), |dir| {
        let _ = fs::remove_dir_all(dir);
    });

    let source = "ecrire(1);";
    fs::write(dir.join("prog"), source).unwrap();

    // Spelled differently on purpose: paths are compared canonically.
    let status = Command::new(env!("CARGO_BIN_EXE_floc"))
        .args(["prog", "-o", "./prog"])
        .current_dir(&dir)
        .stderr(Stdio::null())
        .status()
        .expect("cannot run compiler");

    assert!(!status.success());
    assert_eq!(fs::read_to_string(dir.join("prog")).unwrap(), source);
}

#[test]
fn verbose_logs_go_to_stderr() {
    let mut compiler = Command::new(env!("CARGO_BIN_EXE_floc"))
        .args(["-v", "--emit-ast-as-dot", "-"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("cannot run compiler");

    compiler
        .stdin
        .take()
        .unwrap()
        .write_all(b"ecrire(1);")
        .unwrap();

    let output = compiler.wait_with_output().unwrap();
    let stdout = String::from_utf8(output.stdout).unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();

    assert!(output.status.success());
    assert!(stderr.contains("++ took"));
    assert!(!stdout.contains("++ took"));
    assert!(!stdout.contains("-- parsing"));
}

testprogram! { function_ending_in_endless_loop, "
entier first_divisor(entier n) {
  entier d = 2;
  tantque (Vrai) {
    si (n % d == 0) {
      retourner d;
    }
    d = d + 1;
  }
}

ecrire(first_divisor(35));", [5] }

testprogram! { unreachable_code_after_endless_loop_is_skipped, flags = ["-O", "0"], "
entier f(entier n) {
  tantque (1 == 1) {
    si (n > 3) {
      retourner n;
    }
    n = n + 1;
  }
  ecrire(99);
}

ecrire(f(1));", [4] }
