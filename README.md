Jarvil 0.1.0
============

Jarvil is a statically-typed programming language that seamlessly transpiles to Python, offering developers the ability to catch bugs at compile time 
rather than runtime. With its expressive syntax and powerful type system, Jarvil enhances code reliability and helps create robust applications. 
Embrace the safety and efficiency of Jarvil for your Python projects today!

## Prerequisites
To build and run Jarvil, you'll need:

**Rust and Cargo**
- Install Rust:  
    1. For Unix-like systems, run:
        ```bash
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
        ```
    2. For Windows, download and run [rustup-init.exe](https://win.rustup.rs/)
- Then run:  
    ```bash
    . "$HOME/.cargo/env" 
    ```
- Verify installation with:
    ```bash 
    rustc --version
    cargo --version
    ```


## 🚀 Getting Started

### 1. Clone the Repository

```bash
git clone https://github.com/spino17/jarvil.git
cd jarvil
code .  # (optional, open in VSCode)
```
### 2. Load Cargo Environment

```bash
. "$HOME/.cargo/env"
```

### 3. Build the Project

```bash
cargo build --release
```

### 4. Confirm Installation

```bash
ls ./target/release/    
```

The CLI is called `anyon`. You should see the `anyon` binary in the `target/release/` directory.

### 5. Run the CLI Tool

```bash
./target/release/anyon --help
```

You should see the help message for the CLI tool.

### 6. Create a Basic Project

```bash
./target/release/anyon new MyProject
```

This generates a new folder with a starter `.jv` file and config.

### 7. Run the Project

```bash
cd MyProject
../target/release/anyon run 
```

This will compile the `.jv` source file and execute the generated Python code. You should see the output `Hello, World!` printed to the console.

## 📦 Project Structure
Here’s what your project will look like:

```
MyProject/
├── main.jv
├── __transpiled_main_py_code__.py
└── __ast_main.json
```

## 🛠 Working on the compiler

The repository is a Cargo workspace:

| Crate | What it is |
|---|---|
| `compiler` | the language itself — lexer, parser, resolver, type checker, Python emitter |
| `tools/anyon` | the `anyon` CLI |
| `tools/jarvil-lsp` | the language server |
| `tools/jarvil-wasm` | browser bindings |
| `compiler/jarvil-macros` | derive macros generating the syntax tree's boilerplate |
| `extensions/jarvil-lang` | the VS Code extension ([setup](extensions/jarvil-lang/README.md)) |

### API documentation

```bash
cargo doc --workspace --no-deps --open
```

Start at the `compiler` crate docs: the crate-level page describes the five-pass
pipeline and both entry points, and each module explains its role in it.

### Checks

These four should all pass before a commit:

```bash
cargo fmt --check
cargo clippy --workspace --all-targets -- -D warnings
cargo test --workspace
RUSTDOCFLAGS="-D warnings" cargo doc --workspace --no-deps   # catches broken doc links
```

### Tests

Unit tests live beside the code; the substantial suites are in
`compiler/tests/`, which runs a corpus of `.jv` files through each pass and
snapshots the result. Adding a case usually means adding a file rather than
writing Rust — see [`compiler/tests/README.md`](compiler/tests/README.md).

## 🤝 Contributions

Pull requests and experiments welcome! Feel free to fork and play around with the language.
