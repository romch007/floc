use std::{
    env,
    ffi::{OsStr, OsString},
    fs,
    io::Write,
    iter::repeat_with,
    path::PathBuf,
    process::{Command, Stdio},
};

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

fn random_exec_name() -> PathBuf {
    let temp_dir = env::temp_dir();

    // Use the tmpname function to generate the random executable name
    let random_name = tmpname(
        OsStr::new("floc_test_"),
        if cfg!(target_os = "windows") {
            OsStr::new(".exe")
        } else {
            OsStr::new("")
        },
        10,
    );

    // Combine the temp directory with the random name
    temp_dir.join(random_name)
}

macro_rules! testprogram {
    ($name:ident, $program:expr, $expected:expr $(, $input:expr)*) => {
        #[test]
        fn $name() {
            let program = $program;

            let tmp_exec_path = scopeguard::guard(random_exec_name(), |path| {
                let _ = fs::remove_file(&path);
            });

            let mut compiler = Command::new(env!("CARGO_BIN_EXE_floc"))
                .arg("-")
                .arg("-o")
                .arg(tmp_exec_path.as_path())
                .stdin(Stdio::piped())
                .stderr(Stdio::inherit()) // show errors in test output
                .stdout(Stdio::null())
                .spawn()
                .expect("cannot run compiler");

            let mut compiler_stdin = compiler
                .stdin
                .take()
                .expect("cannot get stdin of compiler process");

            std::thread::spawn(move || {
                compiler_stdin
                    .write_all(program.as_bytes())
                    .expect("cannot write program to stdin");
            });

            let compiler_status = compiler.wait().expect("failed to wait on compiler");

            if !compiler_status.success() {
                panic!("compiler returned non-zero exit code");
            }

            let mut binary = Command::new(tmp_exec_path.as_path())
                .stdin(Stdio::piped())
                .stderr(Stdio::null())
                .stdout(Stdio::piped())
                .spawn()
                .expect("cannot run compiled binary");

            let mut _binary_stdin = binary.stdin.take().expect("cannot get stdin of binary");

            $(
                std::thread::spawn(move || {
                    for num in $input {
                        writeln!(_binary_stdin, "{num}").expect("cannot write to stdin of binary");
                    }
                });
            )*

            let output = binary.wait_with_output().expect("failed to wait for binary");

            let output = String::from_utf8(output.stdout).expect("invalid UTF-8 in program output");
            let output_numbers = output
                .trim()
                .lines()
                .map(|line| line.parse::<i64>())
                .collect::<Result<Vec<_>, _>>()
                .expect("output is not a number");

            let expected_numbers = $expected.to_vec();

            assert_eq!(expected_numbers, output_numbers);
        }
    };
}

testprogram! { simple_write, "ecrire(5); ecrire(4);", [5, 4] }
testprogram! { simple_read, "ecrire(-lire());", [5], [-5] }

testprogram! { big_expr_ints, "ecrire(25 * 2 + 10 / 5 - 40 % 7 + 15 * 2 - 8 * 3 + 12 * 2 - 10 / 4 + 5 + 100 / 25 - 18 % 5 + 9 * 2 - 45 / 9 + 3 * 3);", [103] }
testprogram! { big_expr_bools, "ecrire(Vrai et Faux ou non Vrai et (Faux ou Vrai) et (non (Vrai ou Faux)) et (Faux et non Vrai) ou (Vrai et (Faux ou Vrai)) et non (Faux ou Vrai) et Vrai);", [0] }

testprogram! { example_ackermann, include_str!("../flo-examples/ackermann.flo"), [1021] }

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

testprogram! { example_power, include_str!("../flo-examples/power.flo"), [65536], [2, 16] }
