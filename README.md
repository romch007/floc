# floc

Compiler for the Flo programming language.

## The Flo programming language

The Flo programming language is an imperative and statically-typed language designed by [Florian Bridoux](https://webusers.i3s.unice.fr/~bridoux/) as part of the "Langages, Compilation et Automates" course at Polytech Nice Sophia. The final project of this course involves creating a compiler (in Python or C) that compiles Flo code to ARMv7. The code generation part of this project is rather simple and the compiled programs are unoptimized. I took the initiative to recreate the compiler in Rust, using LLVM to enable excellent run-time performance of programs and availability on a wide range of systems and architectures.

## How to build

What you need:
- Rust
- LLVM 17 with `clang` in path

### Building and running:
```bash
$ cargo build --release
```

```bash
$ target/release/floc[.exe] flo_examples/factorial.flo
```

Note: If you are on Windows, make sure you are in a MSVC x64 native tools command prompt, or clang will fail to compile programs.

## How to code in Flo
[Link to the reference](FLO_REFERENCE.md)

See some example programs [here](https://github.com/romch007/floc/tree/main/flo_examples)
