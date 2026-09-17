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
| `crates/jarvil-parser` | the front end — lexer, parser, name resolution, type checking |
| `crates/jarvil-py` | the Python backend, and the `build_code` entry point |
| `crates/jarvil-lsp` | the language server |
| `crates/anyon` | the `anyon` CLI |
| `crates/jarvil-wasm` | browser bindings |
| `crates/jarvil-macros` | derive macros generating the syntax tree's boilerplate |
| `extensions/jarvil-vscode` | the VS Code extension ([setup](extensions/jarvil-vscode/README.md)) |

The front end knows nothing about Python: `jarvil-py` depends on
`jarvil-parser`, never the reverse. That is what lets `jarvil-lsp` depend on the
front end alone, without linking a code generator it never calls.

### API documentation

```bash
cargo doc --workspace --no-deps --open
```

Start at the `jarvil-parser` crate docs: the crate-level page describes the
four-pass pipeline and its entry points, and each module explains its role in
it. `jarvil-py` documents the final pass.

### Checks

These four should all pass before a commit:

```bash
cargo fmt --check
cargo clippy --workspace --all-targets -- -D warnings
cargo test --workspace
RUSTDOCFLAGS="-D warnings" cargo doc --workspace --no-deps   # catches broken doc links
```

### Tests

Unit tests live beside the code; the substantial suites are the corpora in
`crates/jarvil-parser/tests/` and `crates/jarvil-py/tests/`, which run `.jv`
files through each pass and snapshot the result. Adding a case usually means
adding a file rather than writing Rust — see
[`crates/jarvil-parser/tests/README.md`](crates/jarvil-parser/tests/README.md).

## 🤝 Contributions

Pull requests and experiments welcome! Feel free to fork and play around with the language.
