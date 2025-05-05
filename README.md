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

## 🤝 Contributions

Pull requests and experiments welcome! Feel free to fork and play around with the language.
