import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { defineConfig } from "vitepress";

// Reuse the VS Code extension's TextMate grammar for the static code blocks, so
// the docs and the editor highlight Jarvil identically and there is only one
// grammar to maintain. `crates/jarvil-parser/tests/grammar.rs` guards it
// against drifting from the lexer's keyword list.
const jarvilGrammar = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "../../extensions/jarvil-vscode/syntaxes/jarvil.tmLanguage.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
);

export default defineConfig({
  title: "Jarvil",
  description: "A statically typed language that compiles to Python",
  cleanUrls: true,

  // The compiler ships as a WebAssembly module the playground loads at runtime.
  // Vite must not try to inline or pre-bundle it.
  vite: {
    optimizeDeps: { exclude: ["/src/wasm/jarvil_wasm.js"] },
    server: { fs: { allow: [".."] } },
  },

  markdown: {
    languages: [{ ...jarvilGrammar, name: "jarvil" }],
  },

  themeConfig: {
    nav: [
      { text: "Guide", link: "/guide/why" },
      { text: "Reference", link: "/reference/syntax" },
      { text: "Playground", link: "/playground" },
      { text: "GitHub", link: "https://github.com/spino17/jarvil" },
    ],

    sidebar: {
      "/guide/": [
        {
          text: "Guide",
          items: [
            { text: "Why Jarvil", link: "/guide/why" },
            { text: "Getting started", link: "/guide/getting-started" },
            { text: "Editor setup", link: "/guide/editors" },
          ],
        },
      ],
      "/reference/": [
        {
          text: "Language reference",
          items: [
            { text: "Syntax", link: "/reference/syntax" },
            { text: "Types", link: "/reference/types" },
            { text: "Functions", link: "/reference/functions" },
            { text: "Structs & interfaces", link: "/reference/structs" },
            { text: "Enums & matching", link: "/reference/enums" },
            { text: "Generics", link: "/reference/generics" },
            { text: "Standard library", link: "/reference/stdlib" },
            { text: "Limitations", link: "/reference/limitations" },
          ],
        },
      ],
    },

    socialLinks: [{ icon: "github", link: "https://github.com/spino17/jarvil" }],

    footer: {
      message: "MIT licensed",
      copyright: "Copyright © 2021-2026 Bhavya Bhatt",
    },

    search: { provider: "local" },
  },
});
