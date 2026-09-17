# Front-end corpus tests

Snapshot tests over a corpus of `.jv` files, one directory per pass. Adding
coverage means adding a `.jv` file — no Rust required.

```
corpus/
  parser/ok/        must parse cleanly; snapshot pins the tree shape
  parser/err/       malformed; snapshot pins how panic-mode recovery reshapes the tree
  resolver/err/     name-resolution failures
  type_checker/err/ type errors
```

The transpiler has its own corpus in `crates/jarvil-py/tests/`, which lives
there because running the code generator needs a crate this one does not depend
on. It works exactly the same way.

## Adding a case

1. Drop a `.jv` file into the relevant directory.
2. `cargo test` — the new case fails, because no snapshot exists yet.
3. Read the generated `.snap.new` and check it says what you expect.
4. Accept it: `cargo insta accept` (or rename `.snap.new` → `.snap`).

Step 3 is the one that matters. A snapshot accepted without reading records
whatever the compiler does today, bugs included.

## Why snapshots

The three artifacts worth asserting on — the AST, the diagnostics, the emitted
Python — are all large structured text. Hand-writing assertions against them
would mean a fraction of the coverage for far more effort, and the failure
output would be worse. The trade is that snapshots only catch *changes*; they
say nothing about whether current behaviour is correct. Hence the corpus split:
`err/` files assert that something is reported at all, and the codegen tests
additionally execute the generated Python and assert on real output.

## Conventions

- One behaviour per file, named for what it exercises
  (`binary_precedence.jv`, not `test2.jv`).
- Keep files minimal. A snapshot diff is only readable if the input is.
- Snapshots exclude byte ranges, line numbers and trivia, so they don't churn
  when unrelated lines shift. If you need positions asserted, assert them in a
  hand-written test instead.

## Notes

- Diagnostics are rendered with miette's `NarratableReportHandler` and stripped
  of ANSI, so snapshots are stable across terminals. See `strip_ansi` in
  `common/mod.rs` for why stripping is needed at all.
- The harness reports *every* diagnostic, while `build_code` currently surfaces
  only the first. That is deliberate: these snapshots already reflect the
  multi-error behaviour, so they won't churn when the driver catches up.
