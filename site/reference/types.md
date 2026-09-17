# Types

## Primitives

`int` `float` `str` `bool` — mapping to Python's `int`, `float`, `str`, `bool`.

```jarvil
def main():
    let count: int = 42
    let ratio: float = 2.5
    let name: str = "jarvil"
    let ready: bool = True
```

`True` and `False` are capitalised, as in Python.

Mixing `int` and `float` in arithmetic promotes to `float`, again matching
Python, so the generated code means what the source says.

## Arrays

`[T]` — a Python `list`. Every element must have the same type.

```jarvil
def main():
    let numbers = [3, 1, 2]
    numbers.append(4)
    numbers.sort()
    print(numbers)          // [1, 2, 3, 4]
    print(numbers[0])       // 1
```

## Hashmaps

`{K: V}` — a Python `dict`. Keys must be hashable: `int`, `float`, `str`, or a
tuple of hashable types.

```jarvil
def main():
    let ages = {"ada": 36}
    ages["alan"] = 41
    print(ages["ada"])

    for name in ages:       // iterating yields keys
        print(name)
```

## Tuples

`(A, B)` — fixed length, mixed types, indexed with a literal.

```jarvil
def main():
    let pair = (1, "two")
    print(pair[0])          // 1
```

The index must be a literal, because the element type depends on which element
you are reading.

## Lambdas

```jarvil
def main():
    let double = lambda(x: int) -> int:
        return x * 2

    print(double(21))       // 42
```

Lambda types are compared *structurally*: two differently-named function types
match when their parameters and returns do.

## Type inference

Annotations are optional wherever the type can be determined:

```jarvil
def main():
    let x = 1               // int
    let xs = [1, 2]         // [int]
    let pair = (1, "a")     // (int, str)
```

Annotations are required for empty collections, and for function parameters and
return types — a signature is always explicit.
