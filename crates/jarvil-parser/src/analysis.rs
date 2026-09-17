//! The query surface an editor needs, as opposed to `build_code`, which is
//! shaped for the CLI: it consumes the semantic database inside code generation
//! and surfaces only the first error.
//!
//! Everything here is single-file. Jarvil has no module system yet, so there is
//! no workspace to model and no cross-file resolution to do; each query re-runs
//! the whole pipeline over one buffer. At the size of programs this compiler
//! handles that is comfortably fast enough, and it keeps the server free of
//! caches that could disagree with the source.

use crate::ast::ast::BlockNode;
use crate::code::{JarvilCode, JarvilCodeHandler};
use crate::error::error::JarvilProgramAnalysisErrors;
use crate::lexer::lexer::JarvilLexer;
use crate::parser::parser::JarvilParser;
use crate::parser::resolver::JarvilResolver;
use crate::parser::type_checker::JarvilTypeChecker;
use crate::scope::semantic_db::SemanticStateDatabase;
use text_size::TextRange;

/// How serious a diagnostic is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// Prevents compilation.
    Error,
    /// Worth attention, but compilation continues.
    Warning,
    /// A suggestion.
    Advice,
}

/// A span within a diagnostic, and what to say about it.
#[derive(Debug, Clone)]
pub struct DiagnosticLabel {
    /// The source range this label points at.
    pub range: TextRange,
    /// What to show against the range, if anything.
    pub message: Option<String>,
    /// Whether this is the diagnostic's principal location.
    ///
    /// A consumer that can only show one range should use this one; see
    /// [`JarvilDiagnostic::primary_range`].
    pub is_primary: bool,
}

/// A diagnostic reduced to plain data.
///
/// Deliberately free of miette's types so that consumers -- a language server,
/// say -- need not depend on miette or render its reports. Text here carries no
/// ANSI colour; styling belongs to whatever displays it.
#[derive(Debug, Clone)]
pub struct JarvilDiagnostic {
    /// The headline message.
    pub message: String,
    /// The diagnostic class, such as `TypeCheckError`.
    pub code: Option<String>,
    /// Longer advice on how to fix it.
    pub help: Option<String>,
    /// How serious it is.
    pub severity: Severity,
    /// Every source range involved, primary first if one is marked.
    pub labels: Vec<DiagnosticLabel>,
}

impl JarvilDiagnostic {
    /// The single range best representing this diagnostic.
    ///
    /// The primary label if one is marked, otherwise the first, otherwise an
    /// empty range at the start of the file.
    pub fn primary_range(&self) -> TextRange {
        self.labels
            .iter()
            .find(|label| label.is_primary)
            .or_else(|| self.labels.first())
            .map(|label| label.range)
            .unwrap_or_default()
    }
}

/// A completed analysis, borrowed for the duration of a query.
///
/// Cannot be returned from [`with_analysis`]: [`JarvilCodeHandler`] borrows the
/// source that the analysis owns, so handing these back would require a
/// self-referential struct. Hence the callback.
pub struct AnalysisCtx<'ctx> {
    /// The syntax tree, present even when parsing reported errors.
    pub ast: &'ctx BlockNode,
    /// Resolved names and inferred types.
    pub semantic_db: &'ctx SemanticStateDatabase,
    /// Source access for spans and token text.
    pub code_handler: &'ctx JarvilCodeHandler<'ctx>,
    /// Every diagnostic from every pass, in the order found.
    pub diagnostics: Vec<JarvilDiagnostic>,
    /// The original text.
    ///
    /// Queries phrased in terms of lines rather than nodes -- collecting a doc
    /// comment, say -- read this directly instead of reconstructing it.
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

/// Analyses `source` and hands the result to `query`.
///
/// Runs lexing, parsing, name resolution and type checking, stopping before
/// code generation. Always produces a context: a program that fails to compile
/// still has a tree and a partly-populated semantic database, which is what
/// lets an editor answer questions about code mid-edit.
///
/// ```
/// use jarvil_parser::analysis::with_analysis;
///
/// let names = with_analysis("def main():\n    let x = 1\n", |ctx| {
///     ctx.semantic_db.identifier_in_decl_binding_table_ref().len()
/// });
///
/// assert!(names > 0);
/// ```
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

/// Every diagnostic `source` produces, for callers that need nothing else.
///
/// ```
/// assert!(jarvil_parser::analysis::diagnostics("def main():\n    print(1)\n").is_empty());
/// ```
pub fn diagnostics(source: &str) -> Vec<JarvilDiagnostic> {
    with_analysis(source, |ctx| ctx.diagnostics)
}
