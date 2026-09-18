# Jarvil for Zed

Syntax highlighting and language server support for `.jv` files — the same
server the VS Code extension uses, so both editors behave identically.

## What works

- **Syntax highlighting**, through `tree-sitter-jarvil` at the top of this
  repository
- **Diagnostics**, republished as you type
- **Go to definition** for variables, functions, types and interfaces
- **Hover** showing a signature or inferred type, plus any doc comment
- File association, `//` comment toggling, and 4-space indentation

Highlighting and the language server are independent: Zed builds the grammar
itself from the revision pinned in `extension.toml`, so colours work whether or
not `jarvil-lsp` is installed.

## What doesn't

Only `highlights.scm` is supplied. Zed reads several other query files, and
without them it falls back to defaults:

- no `indents.scm`, so indentation on a new line follows the fixed `tab_size` in
  `config.toml` rather than the block structure
- no `brackets.scm`, so bracket matching is not syntax-aware
- no `outline.scm`, so the outline view and breadcrumbs stay empty

None of these need compiler work — they are queries against the grammar that
already exists.

## Install

```bash
# 1. the language server
cargo install --path crates/jarvil-lsp

# 2. build the extension
cd extensions/jarvil-zed
cargo build --target wasm32-wasip1 --release
```

Then in Zed: **Extensions** → **Install Dev Extension** → choose this directory.

Open any `.jv` file. It should be coloured; hover an identifier, or press `F12`
on a function call.

## Changing the grammar

`extension.toml` pins `tree-sitter-jarvil` by commit, and Zed clones it from
GitHub rather than reading the copy on disk. Editing the grammar locally
therefore changes nothing in Zed until the commit is pushed and `rev` bumped to
match.

To iterate without pushing, point `repository` at the working tree instead:

```toml
[grammars.jarvil]
repository = "file:///absolute/path/to/jarvil"
rev = "<a local commit sha>"
path = "tree-sitter-jarvil"
```

A `file://` URL is still a git clone, so the change has to be committed — but it
need not leave the machine. Restore the GitHub URL before committing the
extension.

The queries in `languages/jarvil/` are a copy of
`tree-sitter-jarvil/queries/`, because Zed loads queries from the extension and
the grammar from the pinned revision. Edit the canonical file and copy it across;
the two drifting apart is the likeliest cause of a construct losing its colour.

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

The asymmetry is only in highlighting, and it is now one of format rather than
coverage: VS Code takes the TextMate grammar in `extensions/jarvil-vscode/`, Zed
takes `tree-sitter-jarvil/`. The two are maintained separately, so a construct
added to the language needs adding to both.
