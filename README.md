# floc

Compiler for the Flo programming language.

## The Flo programming language

The Flo programming language is an imperative and statically-typed language designed by [Florian Bridoux](https://webusers.i3s.unice.fr/~bridoux/) as part of the "Langages, Compilation et Automates" course at Polytech Nice Sophia. The final project of this course involves creating a compiler (in Python or C) that compiles Flo code to ARMv7. The code generation part of this project is rather simple and the compiled programs are unoptimized. I took the initiative to recreate the compiler in Rust, using LLVM to enable excellent run-time performance of programs and availability on a wide range of systems and architectures.

## How to build

What you need:
- Rust (at least 1.85.0)
- A C++ compiler
- LLVM 21
- libclang (usually included with clang)

### Building and running:
```bash
$ cargo build --release --bin=floc
```

(if you are building on Alpine Linux you need to run `RUSTFLAGS="-C target-feature=-crt-static" cargo b -r --bin=floc`)

```bash
$ target/release/floc[.exe] flo-examples/factorial.flo
```

Note: On Windows, make sure `link.exe` is available by installing Visual Studio with the "Desktop development with C++" workload.

### AUR

There is an AUR package called [floc-git](https://aur.archlinux.org/packages/floc-git) btw.

## Usage
Some cool stuff to do:

- Render the AST as a dot graph:
```bash
$ floc --emit-ast-as-dot flo-examples/prime.flo | dot -Tsvg > ast.svg
```

- Dump the generated LLVM IR:
```bash
$ floc --emit-ir flo-examples/prime.flo
```

- Dump the generated LLVM IR after optimization passes:
```bash
$ floc --emit-optimized-ir flo-examples/prime.flo
```

(use `--help` to get all available options)

## How to code in Flo

See some example programs [here](https://github.com/romch007/floc/tree/main/flo-examples)
