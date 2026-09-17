// The query surface an editor needs, as opposed to `build_code`, which is
// shaped for the CLI: it consumes the semantic database inside code generation
// and surfaces only the first error.
//
// Everything here is single-file. Jarvil has no module system yet, so there is
// no workspace to model and no cross-file resolution to do; each query re-runs
// the whole pipeline over one buffer. At the size of programs this compiler
// handles that is comfortably fast enough, and it keeps the server free of
// caches that could disagree with the source.

use crate::ast::ast::BlockNode;
use crate::code::{JarvilCode, JarvilCodeHandler};
use crate::error::error::JarvilProgramAnalysisErrors;
use crate::lexer::lexer::JarvilLexer;
use crate::parser::parser::JarvilParser;
use crate::parser::resolver::JarvilResolver;
use crate::parser::type_checker::JarvilTypeChecker;
use crate::scope::semantic_db::SemanticStateDatabase;
use text_size::TextRange;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Advice,
}

// A label attached to a diagnostic: a range plus what to say about it.
#[derive(Debug, Clone)]
pub struct DiagnosticLabel {
    pub range: TextRange,
    pub message: Option<String>,
    // miette marks one label as primary; that is the one worth anchoring the
    // diagnostic to when a consumer can only show a single range.
    pub is_primary: bool,
}

// A diagnostic reduced to plain data, so consumers do not have to render
// miette's report types or depend on miette at all.
#[derive(Debug, Clone)]
pub struct JarvilDiagnostic {
    pub message: String,
    pub code: Option<String>,
    pub help: Option<String>,
    pub severity: Severity,
    pub labels: Vec<DiagnosticLabel>,
}

impl JarvilDiagnostic {
    // The range to anchor this diagnostic to: the primary label if there is
    // one, else the first, else an empty range at the start of the file.
    pub fn primary_range(&self) -> TextRange {
        self.labels
            .iter()
            .find(|label| label.is_primary)
            .or_else(|| self.labels.first())
            .map(|label| label.range)
            .unwrap_or_default()
    }
}

// Everything a single query needs, borrowed for the duration of the callback.
//
// These cannot be handed back to the caller: `JarvilCodeHandler` borrows the
// `JarvilCode` that the analysis owns, so returning them would be a
// self-referential struct. Hence the callback shape.
pub struct AnalysisCtx<'ctx> {
    pub ast: &'ctx BlockNode,
    pub semantic_db: &'ctx SemanticStateDatabase,
    pub code_handler: &'ctx JarvilCodeHandler<'ctx>,
    pub diagnostics: Vec<JarvilDiagnostic>,
    // The original text. Queries that work in terms of lines rather than nodes
    // -- picking up doc comments, say -- read it directly instead of
    // reconstructing it from the code handler on every request.
    pub source: &'ctx str,
}

// Converts miette's view of a diagnostic into plain data.
//
// Reading it back through the `Diagnostic` trait rather than matching on the
// ~90 `Diagnostics` variants means new variants are picked up for free, and
// their spans stay in one place: the `#[label]` attributes.
fn to_plain_diagnostic(diagnostic: &crate::error::diagnostics::Diagnostics) -> JarvilDiagnostic {
    let report = diagnostic.report();

    let severity = match report.severity() {
        Some(miette::Severity::Warning) => Severity::Warning,
        Some(miette::Severity::Advice) => Severity::Advice,
        _ => Severity::Error,
    };

    let labels = report
        .labels()
        .map(|labels| {
            labels
                .map(|label| DiagnosticLabel {
                    range: TextRange::at(
                        (label.offset() as u32).into(),
                        (label.len() as u32).into(),
                    ),
                    message: label.label().map(|text| text.to_string()),
                    is_primary: label.primary(),
                })
                .collect()
        })
        .unwrap_or_default();

    JarvilDiagnostic {
        message: report.to_string(),
        code: report.code().map(|code| code.to_string()),
        help: report.help().map(|help| help.to_string()),
        severity,
        labels,
    }
}

// Runs lexing, parsing, name resolution and type checking over `source`, then
// hands the results to `query`.
pub fn with_analysis<R>(source: &str, query: impl FnOnce(AnalysisCtx<'_>) -> R) -> R {
    let code = JarvilCode::new(source);
    let errors = JarvilProgramAnalysisErrors::default();

    let lexer = JarvilLexer::new(&errors);
    let (token_vec, code_lines) = lexer.tokenize(&code);
    let code_handler = JarvilCodeHandler::new(&code, code_lines);

    let parser = JarvilParser::new(&code_handler, &errors);
    let ast = parser.parse(token_vec);

    let resolver = JarvilResolver::new(&code_handler, &errors);
    let semantic_db = resolver.resolve_ast(&ast);

    let type_checker = JarvilTypeChecker::new(&code_handler, &errors, semantic_db);
    let semantic_db = type_checker.check_ast(&ast);

    let diagnostics = errors
        .diagnostics()
        .iter()
        .map(to_plain_diagnostic)
        .collect();

    query(AnalysisCtx {
        ast: &ast,
        semantic_db: &semantic_db,
        code_handler: &code_handler,
        diagnostics,
        source,
    })
}

// Convenience wrapper for the common case of only wanting the diagnostics.
pub fn diagnostics(source: &str) -> Vec<JarvilDiagnostic> {
    with_analysis(source, |ctx| ctx.diagnostics)
}
