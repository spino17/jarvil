# Why Jarvil

## The problem

Python's type annotations are checked by convention. `mypy` and its peers are
opt-in, gradual, and unsound *by design* — gradual typing deliberately admits
programs it cannot verify, because the alternative is refusing to run code that
is probably fine.

That is the right trade for adding types to an existing Python codebase. It is
the wrong trade when you are writing new code and would rather the compiler
simply said no.

```python
def greet(name: str) -> str:
    return "Hello, " + name

greet(42)        # mypy flags it; python runs it anyway, then raises
```

The annotation is documentation the runtime ignores.

## The position Jarvil takes

Types are part of the language, the checker *is* the compiler, and a program
that does not type-check produces no output at all.

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

There is no `# type: ignore`, because there is no runtime that would have let
it through.

## Why compile to Python

Because the deployment story is the hardest part of adopting a new language, and
this one does not have it.

The compiler emits ordinary `.py` files. There is no runtime to install, no
extension module to build, no FFI boundary, no container to rebuild. Whatever
already runs your Python — Lambda, a notebook, an embedded interpreter — runs
Jarvil's output without knowing Jarvil exists.

You can also read the output, which matters more than it sounds:

```jarvil
def larger(a: int, b: int) -> int:
    if a > b:
        return a
    return b
```

```python
def larger_0_func(a_0_var, b_1_var):
    if a_0_var > b_1_var:
        return a_0_var
    return b_1_var
```

Statements map to statements. Nothing is inlined, reordered or desugared beyond
recognition, so a stack trace from production still points at code you can find.

## What you give up

Honestly: quite a lot, for now. There is no module system, the standard library
is small, and the language has rough edges that a mature toolchain would not.
[Limitations](/reference/limitations) is a complete list rather than a polite
summary.

What is *not* rough is the type system itself — generic inference with interface
bounds, exhaustive matching on payload-carrying enums, real diagnostics with
source spans. That part works, and it is the part that took the longest.

## Who this is for

Someone who wants Python's deployment story and a real type system, and is
willing to trade breadth of library support for it. Today that is mostly
self-contained programs: scripts, exercises, algorithmic work, the inner core of
something larger.

If you need to import `requests`, this is not ready for you yet.
