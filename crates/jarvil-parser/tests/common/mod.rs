// Each integration test binary compiles this module separately and uses only
// the helpers it needs, so unused ones are expected rather than a smell.
#![allow(dead_code)]

// Shared helpers for the corpus tests.
//
// Each helper takes Jarvil source and returns a deterministic `String` for
// `insta` to snapshot. Two things matter for these to stay reviewable:
//
//   - diagnostics are rendered with miette's `NarratableReportHandler`, not the
//     graphical one the CLI uses. The graphical handler emits ANSI colour and
//     box-drawing that varies with terminal width, which would make snapshots
//     both unreadable and machine-dependent.
//   - nothing here panics on malformed input; a corpus file that crashes the
//     compiler should fail its own test, not take down the run.

use jarvil_parser::ast::dump::dump_ast;
use jarvil_parser::build_ast;
use jarvil_parser::code::JarvilCode;
use jarvil_parser::error::error::JarvilProgramAnalysisErrors;
use jarvil_parser::parser::resolver::JarvilResolver;
use jarvil_parser::parser::type_checker::JarvilTypeChecker;
use miette::NarratableReportHandler;

// Renders every diagnostic logged so far, in discovery order.
fn render_diagnostics(errors: &JarvilProgramAnalysisErrors, source: &str) -> String {
    let reports = errors.reports();

    if reports.is_empty() {
        return "(no diagnostics)\n".to_string();
    }

    let handler = NarratableReportHandler::new();
    let mut out = String::new();

    for (index, report) in reports.into_iter().enumerate() {
        let report = report.with_source_code(source.to_string());

        out.push_str(&format!("--- diagnostic {} ---\n", index + 1));

        let mut rendered = String::new();

        if handler
            .render_report(&mut rendered, report.as_ref())
            .is_err()
        {
            rendered.push_str("<failed to render diagnostic>\n");
        }

        out.push_str(&rendered);
        out.push('\n');
    }

    out
}

// Lexes and parses only. Used by the parser corpus, where the point is the
// shape of the tree (and, for the `err` cases, how recovery reshapes it).
pub fn parse_to_ast(source: &str) -> String {
    let code = JarvilCode::new(source);
    let errors = JarvilProgramAnalysisErrors::default();
    let (ast, code_handler) = build_ast(&code, &errors);

    let dump = match dump_ast(&ast, &code_handler) {
        Ok(dump) => dump,
        Err(err) => format!("<failed to dump ast: {}>\n", err),
    };

    format!(
        "=== ast ===\n{}\n=== diagnostics ===\n{}",
        dump,
        render_diagnostics(&errors, source)
    )
}

// Runs lexing, parsing and name resolution, then reports what was found. Type
// checking is deliberately not run, so that a resolver corpus file fails on
// resolution errors alone rather than on whatever the type checker makes of a
// tree that already failed to resolve.
pub fn resolve_to_diagnostics(source: &str) -> String {
    let code = JarvilCode::new(source);
    let errors = JarvilProgramAnalysisErrors::default();
    let (ast, code_handler) = build_ast(&code, &errors);

    let resolver = JarvilResolver::new(&code_handler, &errors);
    let _ = resolver.resolve_ast(&ast);

    render_diagnostics(&errors, source)
}

// The full analysis pipeline, stopping before code generation.
pub fn type_check_to_diagnostics(source: &str) -> String {
    let code = JarvilCode::new(source);
    let errors = JarvilProgramAnalysisErrors::default();
    let (ast, code_handler) = build_ast(&code, &errors);

    let resolver = JarvilResolver::new(&code_handler, &errors);
    let semantic_db = resolver.resolve_ast(&ast);

    let type_checker = JarvilTypeChecker::new(&code_handler, &errors, semantic_db);
    let _ = type_checker.check_ast(&ast);

    render_diagnostics(&errors, source)
}
