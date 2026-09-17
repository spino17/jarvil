# Jarvil for Zed

Language server support for `.jv` files — the same server the VS Code extension
uses, so both editors behave identically.

## What works

- **Diagnostics**, republished as you type
- **Go to definition** for variables, functions, types and interfaces
- **Hover** showing a signature or inferred type, plus any doc comment
- File association, `//` comment toggling, and 4-space indentation

## What doesn't

**No syntax highlighting.** Zed highlights exclusively through tree-sitter, and
Jarvil has no grammar yet — unlike VS Code, there is no TextMate fallback to
borrow. `.jv` files will render as plain text with working LSP features on top.

Writing `tree-sitter-jarvil` is the fix, and it would benefit Neovim, Helix and
Zed at once. It is real work: Jarvil is indentation-sensitive, which in
tree-sitter means a hand-written external scanner in C to emit INDENT/DEDENT
tokens, the same approach `tree-sitter-python` takes.

## Install

```bash
# 1. the language server
cargo install --path crates/jarvil-lsp

# 2. build the extension
cd extensions/jarvil-zed
cargo build --target wasm32-wasip1 --release
```

Then in Zed: **Extensions** → **Install Dev Extension** → choose this directory.

Open any `.jv` file. Hover an identifier, or press `F12` on a function call.

## Troubleshooting

Run `zed --foreground` to see extension logs.

If nothing happens, the server almost certainly was not found. The extension
looks for `jarvil-lsp` on `PATH` only — and a GUI application does not always
inherit the `PATH` from your shell profile. Launching Zed from a terminal is the
quickest way to rule that out:

```bash
which jarvil-lsp   # should print a path
zed .
```

## How this relates to the other editor integration

Both extensions are thin clients over `crates/jarvil-lsp`. Neither reimplements
anything: a change to diagnostics, go-to-definition or hover lands in both at
once, and the corpus tests in `crates/jarvil-parser/tests/` cover the logic
behind them.

The asymmetry is only in highlighting — VS Code takes a TextMate grammar, which
exists, and Zed takes tree-sitter, which does not.
