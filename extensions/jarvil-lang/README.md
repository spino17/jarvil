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

## Install

```bash
# 1. the language server, into ~/.cargo/bin so it is found from any project
cargo install --path tools/jarvil-lsp

# 2. the extension
cd extensions/jarvil-lang
npm install
npm run install-extension
```

`npm run install-extension` compiles the client, builds a `.vsix`, and installs
it. To only build the package, use `npm run package` and install it yourself:

```bash
code --install-extension jarvil-lang-0.1.0.vsix
```

Reload the window afterwards, then open any `.jv` file.

### Finding the server

The extension checks, in order:

1. the `jarvil.server.path` setting
2. `jarvil-lsp` on `PATH`
3. `target/release/jarvil-lsp`, then `target/debug/jarvil-lsp`, in the workspace

Step 3 means a plain `cargo build` inside this repository needs no
configuration. Outside it, install the server (step 1 above) — but note that
**GUI applications on macOS do not always inherit your shell's `PATH`**, so if
VS Code was launched from the Dock it may not see `~/.cargo/bin`. If the server
will not start, set the path explicitly:

```json
{ "jarvil.server.path": "/Users/you/.cargo/bin/jarvil-lsp" }
```

### Developing the extension

Press <kbd>F5</kbd> with this folder open to launch an Extension Development
Host — no packaging needed, and it picks up changes on reload.

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
