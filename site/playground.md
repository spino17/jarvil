---
title: Playground
aside: false
---

# Playground

The compiler runs as WebAssembly in this tab — diagnostics, hover and
go-to-definition come from the same code the language server uses, with no
server and nothing installed.

<ClientOnly>
  <Playground height="440px" />
</ClientOnly>

::: tip Running the program
**▶ Run** compiles to Python, then executes it with
[Pyodide](https://pyodide.org) — CPython itself compiled to WebAssembly. That is
about 10 MB, so it downloads on the first run only. Compiling and the editor
intelligence need none of it.
:::

## Things to try

Hover any identifier for its type. ⌘/Ctrl-click one to jump to its declaration.

**Break the types.** Change `greet("world")` to `greet(42)` and watch the
squiggle appear as you type — every error is reported, not just the first.

**Exhaustive matching.** Delete a `case` branch:

```jarvil
type Result<T, E> enum:
    Ok(T)
    Err(E)

def half(n: int) -> Result<int, str>:
    if 0 > n:
        return Result<int, str>::Err("negative")
    return Result<int, str>::Ok(n)

def main():
    match half(10):
        case Result::Ok(v):
            print(v)
        case Result::Err(e):
            print(e)
```

**Inference with interface bounds.** Neither call names its type argument:

```jarvil
interface Shape:
    def area() -> int

type Square struct implements Shape:
    side: int
    def __init__(side: int):
        self.side = side
    def area() -> int:
        return self.side * self.side

def total<S: Shape>(shapes: [S]) -> int:
    let sum = 0
    for shape in shapes:
        sum = sum + shape.area()
    return sum

def main():
    print(total([Square(2), Square(3)]))
```

**Show Python** reveals what the compiler emitted.

## Limitations here

Same as the language itself — see [Limitations](/reference/limitations). Two
worth remembering while typing: `a < b` does not parse (write `b > a`), and
methods cannot be called on a literal (bind it to a name first).
