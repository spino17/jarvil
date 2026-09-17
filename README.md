# Jarvil

**A statically typed language that compiles to Python.**

Write Python-shaped code, catch the bugs before you ship it, and deploy ordinary
`.py` files that run anywhere Python does — no runtime, no bindings, no FFI.

```jarvil
def greet(name: str) -> str:
    return "Hello, " + name

def main():
    print(greet(42))
```

```
TypeCheckError

  x mismatched types
   ,-[5:17]
 4 | def main():
 5 |     print(greet(42))
   :                 ^|
   :                  `-- expected type `str`, got `int`
   `----
```

That's a `TypeError` Python would have raised in production, reported before the
program ever ran.

---

## Why

Python's tooling checks types by convention. `mypy` is opt-in, gradual, and
unsound by design — it will let plenty through. Jarvil takes the other position:
types are part of the language, the checker is the compiler, and a program that
does not type-check does not produce output.

What you get back is plain Python. There is no Jarvil runtime to install and
nothing for your deployment target to know about.

## Install

Requires [Rust](https://rustup.rs) 1.88+ and `python3` on your `PATH`.

```bash
git clone https://github.com/spino17/jarvil.git
cd jarvil
cargo install --path crates/jarvil-cli
```

That puts `jarvil` on your `PATH`. For the editor integration, see
[Editor support](#editor-support).

## Quick start

```bash
jarvil new hello
cd hello
jarvil run
```

```
Hello, World!
```

`jarvil build` compiles without running, leaving the generated Python beside the
source so you can read it, commit it, or ship it.

## A tour

### Generics with interface bounds

Inference is real: neither call below names its type argument.

```jarvil
interface Shape:
    def area() -> int

type Square struct implements Shape:
    side: int
    def __init__(side: int):
        self.side = side
    def area() -> int:
        return self.side * self.side

type Rect struct implements Shape:
    w: int
    h: int
    def __init__(w: int, h: int):
        self.w = w
        self.h = h
    def area() -> int:
        return self.w * self.h

def total_area<S: Shape>(shapes: [S]) -> int:
    let sum = 0
    for shape in shapes:
        sum = sum + shape.area()
    return sum

def main():
    print(total_area([Square(2), Square(3)]))   // 13
    print(total_area([Rect(2, 5)]))             // 10
```

Pass a type that does not implement `Shape` and the bound is what rejects it.

### Enums, payloads and exhaustive matching

```jarvil
type Result<T, E> enum:
    Ok(T)
    Err(E)

def square_of(side: int) -> Result<int, str>:
    if 0 > side:
        return Result<int, str>::Err("side must be non-negative")
    return Result<int, str>::Ok(side * side)

def main():
    match square_of(7):
        case Result::Ok(area):
            print(area)                 // 49
        case Result::Err(why):
            print(why)
```

Drop the `Err` branch and the compiler stops you:

```
SemanticError

  x enum variants missing from match-case statement
   :           `-- variants `Err` not handled inside the match-case statement
```

### Types

`int` `float` `str` `bool`, arrays `[T]`, hashmaps `{K: V}`, tuples `(A, B)`,
structs, enums, interfaces, and lambdas. The standard library covers the common
surface of each — see [`std/README.md`](std/README.md).

```jarvil
def main():
    let sentence = "the quick brown fox"
    let words = sentence.split(" ")
    print(len(words))                   // 4

    let counts = {"a": 1}
    counts.update({"b": 2})
    for key in counts:
        print(key.upper())              // A, B
```

## What it compiles to

```jarvil
def larger(a: int, b: int) -> int:
    if a > b:
        return a
    return b

def main():
    print(larger(3, 9))
```

```python
def larger_0_func(a_0_var, b_1_var):
    if a_0_var > b_1_var:
        return a_0_var
    return b_1_var

def main_1_func():
    print(larger_0_func(3, 9))


main_1_func()
```

Statements map to statements. Names are mangled because Jarvil's scoping permits
shadowing that Python's does not — but builtins are left alone, so `len(x)`
stays `len(x)`.

## Editor support

A VS Code extension provides syntax highlighting plus a language server with
live diagnostics, go-to-definition and hover.

```bash
cargo install --path crates/jarvil-lsp

cd extensions/jarvil-vscode
npm install
npm run install-extension
```

Full setup and troubleshooting:
[`extensions/jarvil-vscode/README.md`](extensions/jarvil-vscode/README.md).

## Project status

Jarvil is a **working compiler and an incomplete language**. Everything above is
implemented and tested; the list below is what you will hit if you try to build
something real with it.

**Works today** — generic inference with interface bounds · enums with payloads
and exhaustiveness checking · structs, interfaces, lambdas, tuples · a standard
library over `str`, `[T]` and `{K: V}` · diagnostics with source spans · a
language server · syntax highlighting

**Not there yet**

| Gap | Detail |
|---|---|
| **No modules** | A program is one file. No `import`, no cross-file resolution. |
| **`a < b` does not parse** | `<` after an identifier is always read as the start of generic type arguments. Write `b > a`, or use `<=`. |
| **No conversions** | No `int("5")` or `str(5)` — those names are type keywords, so a call to one is a syntax error. |
| **No methods on literals** | `"a,b".split(",")` is a parse error; bind it to a name first. |
| **Comments are stripped** | The generated Python keeps the source's vertical spacing but loses the prose. |
| **One error at a time** | The CLI reports only the first diagnostic. The language server reports all of them. |

Contributions toward any of these are welcome; modules would unblock the most.

## Architecture

A Cargo workspace:

| Crate | What it is |
|---|---|
| `crates/jarvil-parser` | the front end — lexer, parser, name resolution, type checking |
| `crates/jarvil-py` | the Python backend, and the `build_code` entry point |
| `crates/jarvil-lsp` | the language server |
| `crates/jarvil-cli` | the `jarvil` command |
| `crates/jarvil-wasm` | browser bindings |
| `crates/jarvil-macros` | derive macros generating the syntax tree's boilerplate |
| `extensions/jarvil-vscode` | the VS Code extension |

The front end knows nothing about Python: `jarvil-py` depends on
`jarvil-parser`, never the reverse. That is what lets `jarvil-lsp` depend on the
front end alone, without linking a code generator it never calls.

Compilation is four passes, then emission:

```
source → lexer → parser → name resolution → type checking → Python
```

Diagnostics from every pass accumulate in one collector, so a failed pass does
not stop the next — which is how several errors get reported at once, and how
the language server answers questions about code that does not compile.

## Development

```bash
cargo doc --workspace --no-deps --open
```

Start at `jarvil-parser`: the crate page describes the pipeline, and each module
explains its role in it.

These four should pass before a commit:

```bash
cargo fmt --check
cargo clippy --workspace --all-targets -- -D warnings
cargo test --workspace
RUSTDOCFLAGS="-D warnings" cargo doc --workspace --no-deps
```

### Tests

Unit tests sit beside the code. The substantial suites are the corpora in
`crates/jarvil-parser/tests/` and `crates/jarvil-py/tests/`, which run `.jv`
files through each pass and snapshot the result — including executing the
generated Python and asserting on its real output.

Adding a case usually means adding a file, not writing Rust. See
[`crates/jarvil-parser/tests/README.md`](crates/jarvil-parser/tests/README.md).

## Contributing

Pull requests and experiments welcome. The [gaps above](#project-status) are the
most useful places to start; `crates/jarvil-parser/tests/` is where you prove a
fix works.

## License

MIT — see [LICENSE](LICENSE).
