# The Jarvil standard library

Everything here is available to every program with no import.

**This directory is documentation, not source.** The prelude is defined in Rust,
not in `.jv` files — see "Where this actually lives" below. `inbuilt.jv` shows
the shape the prelude should eventually take, and is kept in sync by hand.

Two separate things stand between that file and being real: there is no module
system to load it, and **`declare def` is inert** — the parser builds a node for
it, but the resolver has no case for it and both the type checker and code
generator skip it, so a `declare def` registers nothing.

## Free functions

| Signature | Notes |
|---|---|
| `print<V>(obj: V)` | |
| `len<V>(obj: V) -> int` | works on `str`, `[T]`, `{K: V}` |
| `range(start: int, end: int) -> [int]` | half-open, as in Python |
| `abs(x: int) -> int` | |
| `min(a: int, b: int) -> int` | |
| `max(a: int, b: int) -> int` | |
| `sum(l: [int]) -> int` | |
| `round(x: float) -> int` | |
| `ord(c: str) -> int` | |
| `chr(code: int) -> str` | |
| `input() -> str` | |

`print` and `len` take an *unbounded* generic, meaning unchecked: `len(5)`
type-checks and then fails in Python. Jarvil has no interface bound expressing
"has a length" yet; when it does, these should be tightened.

## `str` methods

`upper` `lower` `capitalize` `title` `strip` `lstrip` `rstrip`
`startswith(s) -> bool` `endswith(s) -> bool` `find(s) -> int` `count(s) -> int`
`isdigit` `isalpha` `isspace` `islower` `isupper`
`replace(old, new) -> str` `split(sep) -> [str]` `join([str]) -> str`

## `[T]` methods

`append(T)` `extend([T])` `insert(int, T)` `pop() -> T` `count(T) -> int`
`reverse()` `sort()` `clear()`

## `{K: V}` methods

`pop(K) -> V` `update({K: V})` `clear()`

Iterate a hashmap directly to get its keys: `for key in d:`. Index with
`d[key]`.

## Why the surface is exactly this

Code generation emits builtin names **verbatim** — builtins are registered
without a unique id, so they skip name mangling, and a method name is copied
straight through to Python. `s.upper()` becomes `s.upper()`; `len(x)` becomes
`len(x)`.

That has a hard consequence: **a builtin may only exist when a Python function
or method of the same name has the same arity, semantics and return type.**
Otherwise a program type-checks and then fails at runtime, which is the opposite
of what this language is for.

So these are omitted on purpose:

- **`dict.keys/values/items`** — Python returns *views*, not lists. Typing them
  as `[K]` would make `d.keys()[0]` type-check and then fail. Iterating the
  hashmap directly already yields keys.
- **`dict.get`** — returns `None` when absent, and no return type can say that.
- **`list.remove` / `list.index` / `str.index`** — raise when the element is
  missing. (`pop` is included: it fails on an *empty* receiver, the same way
  indexing past the end already can.)
- **`str.format`** — variadic.

Lifting these needs one of: an `Option`-like type in the prelude, an iterator
type, or code generation that can rewrite a call rather than copy it.

## Known gaps

**There are no conversion functions** — no `int("5")`, `str(5)`, `bool(x)`.
`int`, `float`, `str` and `bool` are atomic *type* keywords, so the lexer emits
`<atomic-type>` and `bool(x)` is a syntax error before name resolution ever
runs. Fixing this needs either a parser that reads an atomic type in expression
position as a call, or differently-named conversions (`to_int`) — which would
give up the property that builtin names pass through to Python unchanged.

**Methods cannot be called on a literal.** `"a,b".split(",")` is a *parse*
error — the grammar only lets an atom start with an identifier or `self`, so
`[1, 2].append(3)` fails the same way. Bind it to a name first. This is a parser
limitation, not a library one.

## Where this actually lives

| Surface | File |
|---|---|
| free functions | `compiler/src/builtin.rs` |
| `str` methods | `compiler/src/types/atomic/builtin.rs` |
| `[T]` methods | `compiler/src/types/array/builtin.rs` |
| `{K: V}` methods | `compiler/src/types/hashmap/builtin.rs` |

Tests live in `compiler/tests/corpus/codegen/stdlib_*.jv`, which assert both the
generated Python and its real output, and
`compiler/tests/corpus/type_checker/err/stdlib_misuse.jv`, which asserts that
misuse is rejected.

Once modules land — and once the resolver actually declares `declare def`
prototypes — this should move into real `.jv` source, at which point
`inbuilt.jv` becomes the actual prelude rather than a sketch of one. Note that
methods would need more than that: there is no syntax for declaring a method on
a built-in type at all.
