---
layout: home

hero:
  name: Jarvil
  text: Static types, Python output
  tagline: Catch the bugs before you ship, then deploy ordinary .py files that run anywhere Python does.
  actions:
    - theme: brand
      text: Try it in the browser
      link: /playground
    - theme: alt
      text: Why Jarvil
      link: /guide/why
    - theme: alt
      text: Reference
      link: /reference/syntax

features:
  - title: Types the compiler enforces
    details: Not a linter and not gradual. A program that does not type-check produces no output, so the class of TypeError Python raises in production is reported before the program runs.
  - title: Inference that works
    details: Generic functions bounded by interfaces, with the type argument inferred at the call site. Enums carry payloads, and match must be exhaustive.
  - title: Plain Python out
    details: No runtime to install, no bindings, no FFI. The compiler emits readable Python that your deployment target already knows how to run.
---
