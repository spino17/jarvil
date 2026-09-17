# Transpiler corpus tests

Snapshot tests over `.jv` files that must compile cleanly. Each one is asserted
twice, and the two do different jobs:

- the snapshot pins the **emitted Python**, so a diff shows exactly what changed
  about code generation
- executing that Python and checking its output proves the program still
  **means** the same thing, which a snapshot alone cannot tell you

Adding a case means dropping a `.jv` file into `corpus/codegen/` and accepting
the two snapshots it generates. See
[`../../jarvil-parser/tests/README.md`](../../jarvil-parser/tests/README.md) for
the workflow and conventions, which are shared.

## Notes

- These need `python3` on `PATH`. Without it the execution half is skipped
  rather than failed — a missing interpreter is not a compiler regression.
- `stdlib.rs` lives here rather than with the front end because it asserts on
  analysis *and* on generated code: that every builtin resolves, and that
  builtin names survive code generation unmangled.
