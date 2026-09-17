// Tests for the editor-facing queries.
//
// Offsets are located by searching the source for a marker substring rather
// than hard-coded, so these stay readable and don't break when the fixture is
// edited.

use jarvil_parser::analysis::with_analysis;
use jarvil_parser::queries::{definition_at, hover_at};

// Byte offset of the `n`th (0-based) occurrence of `needle`.
fn offset_of(source: &str, needle: &str, n: usize) -> u32 {
    let mut from = 0;

    for _ in 0..=n {
        from = source[from..]
            .find(needle)
            .map(|index| from + index)
            .unwrap_or_else(|| panic!("`{}` occurrence {} not found", needle, n))
            + if from == 0 { 0 } else { 1 };
    }

    // `from` now points just past the start for repeats; recompute cleanly
    let mut start = 0;
    let mut found = 0;

    while let Some(index) = source[start..].find(needle) {
        let absolute = start + index;

        if found == n {
            return absolute as u32;
        }

        found += 1;
        start = absolute + 1;
    }

    panic!("`{}` occurrence {} not found", needle, n)
}

fn line_of(source: &str, offset: u32) -> usize {
    source[..offset as usize].matches('\n').count() + 1
}

const SAMPLE: &str = "\
// adds two numbers together
def add(x: int, y: int) -> int:
    return x + y

def main():
    let total = add(1, 2)
    print(total)
";

#[test]
fn goto_definition_jumps_from_call_to_declaration() {
    let call_site = offset_of(SAMPLE, "add(1, 2)", 0);

    with_analysis(SAMPLE, |ctx| {
        let definition = definition_at(&ctx, call_site).expect("call site should resolve");

        assert_eq!(
            line_of(SAMPLE, u32::from(definition.target_range.start())),
            2,
            "should jump to the `def add` line"
        );
    });
}

#[test]
fn goto_definition_jumps_from_variable_use_to_its_let() {
    let use_site = offset_of(SAMPLE, "total)", 0);

    with_analysis(SAMPLE, |ctx| {
        let definition = definition_at(&ctx, use_site).expect("variable use should resolve");

        assert_eq!(
            line_of(SAMPLE, u32::from(definition.target_range.start())),
            6,
            "should jump to the `let total` line"
        );
    });
}

#[test]
fn hover_on_a_function_shows_its_signature() {
    let call_site = offset_of(SAMPLE, "add(1, 2)", 0);

    with_analysis(SAMPLE, |ctx| {
        let hover = hover_at(&ctx, call_site).expect("call site should hover");

        assert!(
            hover.contents.contains("def add(int, int) -> int"),
            "unexpected hover contents: {}",
            hover.contents
        );
    });
}

#[test]
fn hover_on_a_variable_shows_its_inferred_type() {
    let use_site = offset_of(SAMPLE, "total)", 0);

    with_analysis(SAMPLE, |ctx| {
        let hover = hover_at(&ctx, use_site).expect("variable use should hover");

        assert!(
            hover.contents.contains("let total: int"),
            "unexpected hover contents: {}",
            hover.contents
        );
    });
}

#[test]
fn hover_on_a_declaration_includes_its_doc_comment() {
    let decl_site = offset_of(SAMPLE, "add(x: int", 0);

    with_analysis(SAMPLE, |ctx| {
        let hover = hover_at(&ctx, decl_site).expect("declaration should hover");

        assert!(
            hover.contents.contains("adds two numbers together"),
            "doc comment missing from hover: {}",
            hover.contents
        );
    });
}

#[test]
fn queries_on_whitespace_return_nothing() {
    // Inside the indentation before `return`. Note this cannot be the newline
    // at the end of the previous line: a cursor sitting immediately after a
    // name is still *on* that name as far as an editor is concerned, and the
    // queries deliberately treat it that way.
    let indentation = offset_of(SAMPLE, "return x + y", 0) - 2;

    with_analysis(SAMPLE, |ctx| {
        assert!(definition_at(&ctx, indentation).is_none());
        assert!(hover_at(&ctx, indentation).is_none());
    });
}

#[test]
fn queries_survive_a_file_that_does_not_compile() {
    // A server queries constantly against half-written code, so the pipeline
    // must stay answerable rather than panicking once errors are present.
    let broken = "def main():\n    let x: int = \"oops\"\n    let y = undefined_thing()\n";

    with_analysis(broken, |ctx| {
        assert!(
            !ctx.diagnostics.is_empty(),
            "expected this program to report errors"
        );

        // should not panic
        let _ = definition_at(&ctx, offset_of(broken, "x: int", 0));
        let _ = hover_at(&ctx, offset_of(broken, "x: int", 0));
    });
}

#[test]
fn diagnostics_carry_ranges_and_plain_text() {
    let broken = "def main():\n    let x: int = \"oops\"\n";

    with_analysis(broken, |ctx| {
        let diagnostic = ctx
            .diagnostics
            .first()
            .expect("expected a type error")
            .clone();

        assert!(!diagnostic.labels.is_empty(), "diagnostic has no labels");

        let range = diagnostic.primary_range();

        assert_eq!(
            line_of(broken, u32::from(range.start())),
            2,
            "diagnostic should point at the `let` line"
        );

        // colour belongs to the renderer; nothing here may carry escape codes
        assert!(
            !diagnostic.message.contains('\u{1b}'),
            "message contains ANSI escapes: {:?}",
            diagnostic.message
        );

        if let Some(help) = &diagnostic.help {
            assert!(
                !help.contains('\u{1b}'),
                "help contains ANSI escapes: {:?}",
                help
            );
        }
    });
}
