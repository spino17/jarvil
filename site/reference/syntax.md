# Syntax

Jarvil is indentation-sensitive and Python-shaped: blocks open with `:` and are
delimited by indentation, not braces.

Comments use `//` and `/* */` — not Python's `#`.

```jarvil
// a line comment

/* a block
   comment */

def main():
    print(1)
```

## Entry point

Every program needs a `main` taking no arguments and returning nothing. The
compiler appends the call, so the generated Python runs on import.

```jarvil
def main():
    print("hello")
```

## Bindings

`let` introduces a binding. The type is inferred unless annotated.

```jarvil
def main():
    let x = 1                 // inferred int
    let name: str = "jarvil"  // annotated
    x = x + 1                 // rebinding, no `let`
    print(x)
```

Empty collections cannot be inferred and must be annotated — there is nothing to
infer the element type from.

## Control flow

```jarvil
def main():
    let n = 5

    if n > 3:
        print("big")
    elif n > 1:
        print("medium")
    else:
        print("small")

    let i = 0
    while i <= 2:
        print(i)
        i = i + 1

    for value in range(0, 3):
        print(value)
```

`break` and `continue` work as expected, and are rejected outside a loop.

## Operators

| Kind | Operators |
|---|---|
| Arithmetic | `+` `-` `*` `/` `**` |
| Comparison | `==` `!=` `<` `<=` `>` `>=` |
| Logical | `and` `or` `not` |

Division follows Python 3: `/` always yields `float`, even on two `int`s.

::: warning `a < b` does not parse
`<` immediately after an identifier is read as the start of generic type
arguments, so `n < 0` is a syntax error. Write `0 > n`, or use `<=`. See
[Limitations](/reference/limitations).
:::
