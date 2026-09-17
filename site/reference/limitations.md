# Limitations

A complete list of what will bite you, rather than a polite summary. Each is a
real constraint in the current compiler.

## No modules

A program is a single file. There is no `import`, no cross-file resolution, and
no way to split a project up.

`declare def` parses but is inert — the resolver has no case for it, so it
registers nothing. The standard library is built into the compiler rather than
written in Jarvil for this reason.

This is the biggest gap, and it blocks the most.

## `a < b` does not parse {#less-than}

`<` immediately after an identifier is always read as the start of generic type
arguments, never as less-than.

```jarvil
if n < 0:       // SyntaxError
if 0 > n:       // fine
if n <= 0:      // fine — `<=` is a distinct token
```

Reverse the comparison, or use `<=`. This one is worth knowing before you write
anything substantial.

## No conversions {#no-conversions}

There is no `int("5")`, `str(5)` or `bool(x)`. Those names are atomic *type*
keywords, so a call to one is a syntax error before name resolution ever runs.

Fixing it means either teaching the parser that an atomic type in expression
position is a call, or giving conversions different names — at the cost of no
longer matching Python, which is what lets builtin names pass through
unchanged.

## No methods on literals

```jarvil
"a,b".split(",")        // parse error
[1, 2].append(3)        // parse error
```

An atom can only start with an identifier or `self`. Bind to a name first.

## Comments are stripped

The generated Python keeps the source's vertical spacing but loses the prose —
each comment becomes a newline.

## One error at a time from the CLI

`jarvil build` reports only the first diagnostic. The language server and the
[playground](/playground) report all of them, since they use a different entry
point.

## No tree-sitter grammar

So no syntax highlighting in Zed, Neovim or Helix. VS Code works because it
takes a TextMate grammar, which does exist.

Jarvil is indentation-sensitive, which in tree-sitter means a hand-written
external scanner emitting INDENT/DEDENT tokens.

## Small standard library

No file IO, no networking, no JSON, no datetime, no regular expressions. See
[Standard library](/reference/stdlib) for exactly what exists.
