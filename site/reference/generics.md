# Generics

A generic parameter is declared in angle brackets and may be bounded by an
interface. The bound is what makes the parameter useful: it is the set of
methods the function is allowed to call.

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

Neither call names its type argument — `S` is inferred from the argument.

Passing a type that does not implement `Shape` is rejected by the bound, and the
diagnostic says which interface was not satisfied.

## Generic types

Structs and enums take parameters too:

```jarvil
type Pair<A, B> struct:
    first: A
    second: B

    def __init__(first: A, second: B):
        self.first = first
        self.second = second
```

## Explicit type arguments

Where inference cannot reach — constructing an enum variant, for instance — the
arguments are written out:

```jarvil
return Result<int, str>::Ok(1)
```

## What it compiles to

Nothing. Generics are **erased**: Python is dynamically typed, so one function
body serves every instantiation and no monomorphisation is needed. `total_area`
above is emitted once, not twice.
