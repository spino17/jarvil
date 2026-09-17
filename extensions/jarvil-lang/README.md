# Jarvil for VS Code

Syntax highlighting and language-server support for `.jv` files.

## What works

**Highlighting** (no build needed — TextMate grammar only)

Keywords, declarations, atomic types, string and numeric literals, comments,
generics, enum variants, and call sites. Words reserved so the generated Python
stays valid (`import`, `class`, `None`, …) are flagged as errors, because using
one as an identifier really is a syntax error in Jarvil.

**Language server** (needs `jarvil-lsp` built)

- **Diagnostics** — every error the compiler reports, live as you type, with the
  secondary labels attached as related information.
- **Go to definition** — on a variable, function, type, or interface reference.
- **Hover** — signature or inferred type, plus the `//` doc comment above a
  declaration.

## Setup

```bash
# from the repository root
cargo build --release -p jarvil-lsp

cd extensions/jarvil-lang
npm install
npm run compile
```

Then press <kbd>F5</kbd> in VS Code with this folder open to launch an Extension
Development Host, and open any `.jv` file.

The extension finds the server by checking, in order:

1. the `jarvil.server.path` setting
2. `jarvil-lsp` on `PATH`
3. `target/release/jarvil-lsp`, then `target/debug/jarvil-lsp`, in the workspace

Step 3 means a plain `cargo build` in this repository is enough — no
configuration.

## Limitations

Single file. Jarvil has no module system yet, so there is no cross-file
resolution and nothing resolves outside the buffer it is in. The server
re-analyses the whole document on each request rather than reparsing
incrementally; at the size of programs this compiler handles that is fast, and
it means an answer can never be stale with respect to the source.

Highlighting is a regex grammar, so it identifies tokens by shape rather than by
meaning — it cannot tell a type from a variable in every position. Semantic
tokens from the language server would fix that and are the natural next step.

## Troubleshooting

Set `"jarvil.trace.server": "verbose"` and check the *Jarvil Language Server*
output channel to see the JSON-RPC traffic.

If highlighting works but nothing else does, the server is not being found —
check the output channel for a start-up error, and confirm the binary exists at
one of the three locations above.
