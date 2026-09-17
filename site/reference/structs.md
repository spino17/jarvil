# Structs and interfaces

## Structs

```jarvil
type Point struct:
    x: int
    y: int

    def __init__(x: int, y: int):
        self.x = x
        self.y = y

    def magnitude_squared() -> int:
        return self.x * self.x + self.y * self.y

def main():
    let p = Point(3, 4)
    print(p.magnitude_squared())    // 25
```

`__init__` is the constructor and must initialise every field — the compiler
lists any it misses. Methods take `self` implicitly; it is not written in the
parameter list.

## Interfaces

An interface declares methods a type must provide.

```jarvil
interface Printable:
    def show()

type Tag struct implements Printable:
    label: str

    def __init__(label: str):
        self.label = label

    def show():
        print(self.label)

def main():
    let t = Tag("hello")
    t.show()
```

Declaring `implements` without supplying every method is an error, and the
diagnostic names what is missing.

Interfaces are what make generics useful — see [Generics](/reference/generics).
