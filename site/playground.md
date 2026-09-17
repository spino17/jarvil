---
title: Playground
aside: false
pageClass: playground-page
---

# Playground

The compiler runs as WebAssembly in this tab — diagnostics, hover and
go-to-definition come from the same code the language server uses.

<ClientOnly>
  <Playground height="calc(100vh - 310px)" />
</ClientOnly>

**▶ Run** executes the program with [Pyodide](https://pyodide.org) (~10 MB, first
run only). While typing: `a < b` does not parse — write `b > a`, and bind a
literal to a name before calling a method on it
([why](/reference/limitations)).
