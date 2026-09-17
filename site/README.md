# The Jarvil website

Documentation and an in-browser playground, built with
[VitePress](https://vitepress.dev).

```bash
cd site
npm install
npm run dev        # builds the wasm, then serves at localhost:5173
```

| Script | Effect |
|---|---|
| `npm run wasm` | build `jarvil-wasm` into `src/wasm/` |
| `npm run dev` | wasm + dev server |
| `npm run build` | wasm + static site into `.vitepress/dist` |
| `npm run test` | offset-conversion tests |

`npm run wasm` needs [`wasm-pack`](https://rustwasm.github.io/wasm-pack/).

## How the playground works

Three pieces, and it is worth being precise about which does what.

**The compiler, as WebAssembly** (~850 KB, `crates/jarvil-wasm`). Provides
diagnostics, hover and go-to-definition to the editor, and compiles to Python.

**No language server.** `jarvil-lsp` cannot compile to wasm — tokio has no
`io-std` there — and should not: LSP is a transport for talking to a *separate
process*, and a browser tab has none. So `src/jarvil-wasm-bridge.js` calls the
analysis functions directly. They are the same functions the real language
server calls, so the browser and your editor cannot disagree about what a
program means.

**Pyodide, for execution** (~10 MB, from a CDN). Compiling Jarvil to wasm yields
Python *source*; running it needs a Python interpreter, which in a browser means
CPython-compiled-to-wasm. It loads lazily on the first ▶ Run, so browsing the
docs never pays for it.

## Deployment

Pushing to `main` builds and publishes to GitHub Pages via
[`.github/workflows/deploy-site.yml`](../.github/workflows/deploy-site.yml).

The job needs a Rust toolchain and `wasm-pack` as well as Node, because the
playground ships the real compiler — `npm run build` compiles the wasm before
building the site.

**One-time setup:** in the repository's *Settings → Pages*, set **Source** to
**GitHub Actions**. Without it the workflow runs and the deploy step fails.

The site is served from a subpath, so `.vitepress/config.mts` sets
`base: "/jarvil/"`. If it ever moves to a custom domain at the root, change that
to `"/"` — otherwise the page loads and every asset 404s.

## Layout

```
index.md              landing page
guide/                motivation, install, editor setup
reference/            language reference
playground.md         hosts the <Playground> component
.vitepress/
  config.mts          nav, sidebar, Shiki grammar registration
  theme/Playground.vue  the editor, wired to the wasm bridge
src/
  jarvil-language.js  CodeMirror syntax mode
  jarvil-wasm-bridge.js  wasm loading + offset conversion
  python-runtime.js   Pyodide loading and execution
test/offsets.test.mjs
```

## Two things that are easy to get wrong

**Offsets.** The compiler speaks byte offsets; CodeMirror speaks UTF-16 indices.
They agree on ASCII and diverge on the first accented character, at which point
every range on that line is silently wrong. Everything crossing the boundary is
converted, and `test/offsets.test.mjs` checks the conversions round-trip —
including mid-surrogate positions, which an emoji creates and which naive
slicing corrupts.

**Static highlighting.** Code blocks in the markdown are highlighted by Shiki
using the *VS Code extension's* TextMate grammar, loaded in `config.mts`. One
grammar, so the docs and the editor cannot drift — and
`crates/jarvil-parser/tests/grammar.rs` guards it against drifting from the
lexer.

The editor itself uses the separate CodeMirror mode in `src/`, because
CodeMirror does not consume TextMate grammars. A tree-sitter grammar would let
both collapse into one.
