# Getting started

Requires [Rust](https://rustup.rs) 1.88+ and `python3` on your `PATH`.

```bash
git clone https://github.com/spino17/jarvil.git
cd jarvil
cargo install --path crates/jarvil-cli
```

That puts `jarvil` on your `PATH`.

## Your first program

```bash
jarvil new hello
cd hello
jarvil run
```

```
Hello, World!
```

`jarvil new` writes a `main.jv`; `jarvil run` compiles and executes it.

## Commands

| Command | Effect |
|---|---|
| `jarvil new <name>` | create a project directory with a starter `main.jv` |
| `jarvil build` | compile `main.jv` to Python beside it |
| `jarvil run` | build, then execute the generated Python |

`jarvil build` leaves the Python next to the source, so you can read it, commit
it, or ship it. A failed build exits non-zero, so it composes with scripts and
CI.

## Trying it without installing

The [playground](/playground) runs the compiler as WebAssembly in your browser —
no toolchain needed.
