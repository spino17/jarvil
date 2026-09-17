# Standard library

Available in every program without an import. Each maps to a Python function or
method of the same name, which is why the surface is exactly this — see
[Limitations](/reference/limitations#no-conversions) for what that rules out.

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

`print` and `len` take an unbounded generic, which means unchecked: `len(5)`
type-checks and then fails in Python. There is no interface expressing "has a
length" to bound them with yet.

## `str`

Case — `upper` `lower` `capitalize` `title`

Trimming — `strip` `lstrip` `rstrip`

Searching — `startswith(s) -> bool` `endswith(s) -> bool` `find(s) -> int`
`count(s) -> int`

Testing — `isdigit` `isalpha` `isspace` `islower` `isupper`

Transforming — `replace(old, new) -> str` `split(sep) -> [str]`
`join(parts: [str]) -> str`

```jarvil
def main():
    let sentence = "  the quick brown fox  "
    let trimmed = sentence.strip()

    print(trimmed.upper())
    print(trimmed.split(" "))
```

## `[T]`

`append(T)` `extend([T])` `insert(int, T)` `pop() -> T` `count(T) -> int`
`reverse()` `sort()` `clear()`

## `{K: V}`

`pop(K) -> V` `update({K: V})` `clear()`

Iterate a hashmap directly for its keys; index with `d[key]`.

## Deliberately absent

- **`dict.keys/values/items`** — Python returns *views*, not lists. Typing them
  as `[K]` would make `d.keys()[0]` type-check and then fail at runtime.
- **`dict.get`** — returns `None` when absent, which no return type can express.
- **`list.remove`, `list.index`, `str.index`** — raise when the element is
  missing.

Lifting these needs an `Option`-like type in the prelude, an iterator type, or
code generation that can rewrite a call rather than copy it.
