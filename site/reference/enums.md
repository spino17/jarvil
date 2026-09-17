# Enums and matching

Enum variants may carry a payload.

```jarvil
type Result<T, E> enum:
    Ok(T)
    Err(E)
```

Construct a variant with the type arguments spelled out:

```jarvil
def square_of(side: int) -> Result<int, str>:
    if 0 > side:
        return Result<int, str>::Err("side must be non-negative")

    return Result<int, str>::Ok(side * side)
```

## Matching

`match` destructures the payload into a binding:

```jarvil
def main():
    match square_of(7):
        case Result::Ok(area):
            print(area)             // 49
        case Result::Err(why):
            print(why)
```

## Exhaustiveness

Every variant must be handled. Omitting one is a compile error, not a runtime
surprise:

```
SemanticError

  x enum variants missing from match-case statement
   :  `-- variants `Err` not handled inside the match-case statement
```

This is the property that makes enums worth using for error handling: adding a
variant later turns every incomplete `match` into a compile error, so nothing is
silently forgotten.

## What it compiles to

Python has no enums with payloads, so a variant becomes a small class carrying a
discriminant and its data, and `match` becomes a chain over the discriminant.

```python
class Result_0_ty:
    def __init__(self, index, data=None):
        self.index = index
        self.data = data
```
