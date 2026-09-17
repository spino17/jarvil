// Guards on the shape of the prelude, as distinct from its behaviour.
//
// The corpus in `corpus/codegen/stdlib_*.jv` checks that the standard library
// *works*; these check that it is even reachable, and that its names still line
// up with Python's.

mod common;

use common::{transpile, type_check_to_diagnostics};

// Names the lexer turns into something other than an identifier.
//
// A builtin named after one of these is dead on arrival: `bool(x)` lexes as
// `<atomic-type>` followed by `(`, which is a syntax error long before name
// resolution runs, so the namespace entry can never be reached. This was a real
// bug -- `bool` was registered, documented, and uncallable.
const NON_IDENTIFIER_NAMES: [&str; 4] = ["int", "float", "str", "bool"];

// Every free function the prelude claims to provide.
const FREE_FUNCTIONS: [&str; 11] = [
    "print", "len", "range", "abs", "min", "max", "sum", "input", "ord", "chr", "round",
];

#[test]
fn no_builtin_is_named_after_a_keyword() {
    for name in FREE_FUNCTIONS {
        assert!(
            !NON_IDENTIFIER_NAMES.contains(&name),
            "`{}` is an atomic type keyword, so `{}(..)` cannot parse -- \
             registering it as a builtin creates an unreachable namespace entry",
            name,
            name
        );
    }
}

// Proves the point above rather than asserting it: each of these really is a
// syntax error, so the exclusion above is protecting against something.
#[test]
fn calling_an_atomic_type_is_a_syntax_error() {
    for name in NON_IDENTIFIER_NAMES {
        let source = format!("def main():\n    let x = 1\n    print({}(x))\n", name);
        let diagnostics = type_check_to_diagnostics(&source);

        assert!(
            diagnostics.contains("SyntaxError"),
            "expected `{}(x)` to be a syntax error, got:\n{}",
            name,
            diagnostics
        );
    }
}

// Each free function must be callable, which is what catches a name that the
// lexer refuses to treat as an identifier.
#[test]
fn every_free_function_is_reachable() {
    for name in FREE_FUNCTIONS {
        // `input` takes nothing; the rest are called with arguments that may be
        // the wrong type. That is fine: this asserts the *name resolves*, not
        // that the call type-checks, so only "not declared" is a failure.
        let source = format!("def main():\n    let a = 1\n    {}(a)\n", name);
        let diagnostics = type_check_to_diagnostics(&source);

        assert!(
            !diagnostics.contains("not declared in any namespace"),
            "builtin `{}` does not resolve:\n{}",
            name,
            diagnostics
        );
        assert!(
            !diagnostics.contains("SyntaxError"),
            "builtin `{}` cannot be parsed as a call:\n{}",
            name,
            diagnostics
        );
    }
}

// The whole design rests on builtin names surviving into the generated Python
// unchanged, since each is a front for the Python function of the same name.
// If mangling ever started applying to them, every one of these would break at
// runtime while still type-checking.
#[test]
fn builtin_names_are_not_mangled_in_generated_python() {
    let source = "\
def main():
    let xs = [3, 1]
    let s = \"hi\"
    print(len(xs))
    print(max(1, 2))
    print(s.upper())
    xs.sort()
";

    let python = transpile(source).expect("expected this program to compile");

    for expected in ["print(", "len(", "max(", ".upper()", ".sort()"] {
        assert!(
            python.contains(expected),
            "`{}` was rewritten by code generation; generated:\n{}",
            expected,
            python
        );
    }

    // and the user's own names *are* mangled, so this is a real distinction
    assert!(
        python.contains("xs_0_var"),
        "expected user identifiers to be mangled; generated:\n{}",
        python
    );
}
