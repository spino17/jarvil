# Functions

Parameters and return type are always explicit. A function with no `->` returns
nothing.

```jarvil
def add(a: int, b: int) -> int:
    return a + b

def log(message: str):
    print(message)
```

Functions may nest, and an inner function is visible only within its parent.

```jarvil
def outer() -> int:
    def inner() -> int:
        return 7

    return inner()

def main():
    print(outer())          // 7
```

## Return checking

A function declaring a return type must return on every path — the compiler
rejects one that can fall off the end.

## Name resolution

An identifier resolves across three namespaces, tried in order:

1. functions
2. types
3. variables

So a name can exist as both a type and a variable without ambiguity, and the
"not declared" diagnostic names all three when a lookup fails.
