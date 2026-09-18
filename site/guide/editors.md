# Editor setup

Both integrations are thin clients over the same language server, so a change to
diagnostics, go-to-definition or hover lands in both at once.

```bash
cargo install --path crates/jarvil-lsp
```

| Editor | Highlighting | Language server |
|---|---|---|
| VS Code | yes (TextMate) | yes |
| Zed | yes (tree-sitter) | yes |

## VS Code

```bash
cd extensions/jarvil-vscode
npm install
npm run install-extension
```

Reload the window, then open any `.jv` file.

## Zed

```bash
cd extensions/jarvil-zed
cargo build --target wasm32-wasip1 --release
```

Then **Extensions** → **Install Dev Extension** → choose that directory.

Zed highlights exclusively through tree-sitter. It builds the grammar itself
from the revision pinned in `extension.toml`, so highlighting works whether or
not the language server is installed.

## What you get

- **Diagnostics** as you type, all of them rather than just the first
- **Go to definition** on variables, functions, types and interfaces
- **Hover** showing a signature or inferred type, plus the doc comment above a
  declaration

## Troubleshooting

If highlighting works but nothing else does, the server was not found. GUI
applications do not always inherit a login shell's `PATH`; launching the editor
from a terminal is the quickest way to rule that out.

If everything appears **twice**, two extensions are both registering providers.
Check with `ps aux | grep jarvil-lsp` — more than one process means a stale
install is still loaded.
