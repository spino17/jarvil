//! Browser bindings for the Jarvil compiler.
//!
//! Everything the playground needs: compile to Python, and answer the same
//! editor questions the language server answers.
//!
//! # Why there is no language server here
//!
//! [`jarvil-lsp`] cannot be compiled to WebAssembly -- it needs tokio's
//! `io-std` for stdin and stdout, which does not exist on wasm -- and it should
//! not be. LSP is a transport for talking to a *separate process*, and a
//! browser tab has none; serialising JSON-RPC to talk to yourself buys nothing.
//!
//! So the browser skips the protocol and calls the analysis directly. The
//! functions below are thin wrappers over `jarvil_parser::analysis` and
//! `jarvil_parser::queries` -- exactly what the real language server calls, so
//! the browser and the editor cannot disagree about what a program means.
//!
//! # Positions
//!
//! Offsets here are **byte offsets**, as the compiler uses internally. A
//! JavaScript caller holding UTF-16 string indices must convert; see
//! `site/playground` for the mapping used there.
//!
//! [`jarvil-lsp`]: https://github.com/spino17/jarvil/tree/main/crates/jarvil-lsp

use jarvil_parser::analysis::{Severity, with_analysis};
use jarvil_parser::queries::{definition_at, hover_at};
use jarvil_py::build_code;
use serde::Serialize;
use wasm_bindgen::prelude::*;

/// A diagnostic, flattened for JavaScript.
///
/// Ranges are byte offsets into the source. `severity` is one of `"error"`,
/// `"warning"` or `"advice"`.
#[derive(Serialize)]
struct JsDiagnostic {
    message: String,
    help: Option<String>,
    code: Option<String>,
    severity: &'static str,
    start: u32,
    end: u32,
    /// Secondary labels: the "expected X, got Y" halves of a type error.
    labels: Vec<JsLabel>,
}

#[derive(Serialize)]
struct JsLabel {
    message: Option<String>,
    start: u32,
    end: u32,
    is_primary: bool,
}

/// The result of compiling: Python if it succeeded, and every diagnostic found.
///
/// `python` is `None` exactly when `diagnostics` contains an error. Both are
/// returned together so the playground can show errors without losing whatever
/// was previously compiled.
#[derive(Serialize)]
struct JsCompileResult {
    python: Option<String>,
    diagnostics: Vec<JsDiagnostic>,
}

#[derive(Serialize)]
struct JsHover {
    contents: String,
    start: u32,
    end: u32,
}

#[derive(Serialize)]
struct JsDefinition {
    target_start: u32,
    target_end: u32,
    origin_start: u32,
    origin_end: u32,
}

fn severity_name(severity: Severity) -> &'static str {
    match severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Advice => "advice",
    }
}

/// Analyses `source` and returns every diagnostic, without generating code.
///
/// This is what the editor calls on each keystroke: it stops before code
/// generation, so it stays cheap and works on a program that does not compile.
#[wasm_bindgen]
pub fn diagnostics(source: &str) -> Result<JsValue, JsValue> {
    let collected = with_analysis(source, |ctx| {
        ctx.diagnostics
            .iter()
            .map(|diagnostic| {
                let range = diagnostic.primary_range();

                JsDiagnostic {
                    message: diagnostic.message.clone(),
                    help: diagnostic.help.clone(),
                    code: diagnostic.code.clone(),
                    severity: severity_name(diagnostic.severity),
                    start: range.start().into(),
                    end: range.end().into(),
                    labels: diagnostic
                        .labels
                        .iter()
                        .map(|label| JsLabel {
                            message: label.message.clone(),
                            start: label.range.start().into(),
                            end: label.range.end().into(),
                            is_primary: label.is_primary,
                        })
                        .collect(),
                }
            })
            .collect::<Vec<_>>()
    });

    to_js(&collected)
}

/// Compiles `source`, returning the Python and any diagnostics.
///
/// Unlike [`diagnostics`] this runs the whole pipeline. Use it for the Run
/// button rather than on every keystroke.
#[wasm_bindgen]
pub fn compile(source: &str) -> Result<JsValue, JsValue> {
    // Diagnostics come from the analysis pass, which reports all of them;
    // `build_code` alone would surface only the first.
    let found = with_analysis(source, |ctx| {
        ctx.diagnostics
            .iter()
            .map(|diagnostic| {
                let range = diagnostic.primary_range();

                JsDiagnostic {
                    message: diagnostic.message.clone(),
                    help: diagnostic.help.clone(),
                    code: diagnostic.code.clone(),
                    severity: severity_name(diagnostic.severity),
                    start: range.start().into(),
                    end: range.end().into(),
                    labels: diagnostic
                        .labels
                        .iter()
                        .map(|label| JsLabel {
                            message: label.message.clone(),
                            start: label.range.start().into(),
                            end: label.range.end().into(),
                            is_primary: label.is_primary,
                        })
                        .collect(),
                }
            })
            .collect::<Vec<_>>()
    });

    let python = if found.iter().any(|d| d.severity == "error") {
        None
    } else {
        let (result, _ast) = build_code(jarvil_parser::code::JarvilCode::new(source));

        result.ok()
    };

    to_js(&JsCompileResult {
        python,
        diagnostics: found,
    })
}

/// Describes the symbol at `offset`, or `null` if there is nothing there.
#[wasm_bindgen]
pub fn hover(source: &str, offset: u32) -> Result<JsValue, JsValue> {
    let found = with_analysis(source, |ctx| {
        hover_at(&ctx, offset).map(|hover| JsHover {
            contents: hover.contents,
            start: hover.range.start().into(),
            end: hover.range.end().into(),
        })
    });

    to_js(&found)
}

/// Locates the declaration of the symbol at `offset`, or `null`.
#[wasm_bindgen]
pub fn definition(source: &str, offset: u32) -> Result<JsValue, JsValue> {
    let found = with_analysis(source, |ctx| {
        definition_at(&ctx, offset).map(|definition| JsDefinition {
            target_start: definition.target_range.start().into(),
            target_end: definition.target_range.end().into(),
            origin_start: definition.origin_range.start().into(),
            origin_end: definition.origin_range.end().into(),
        })
    });

    to_js(&found)
}

/// Installs a panic hook that reports Rust panics to the browser console.
///
/// Without it a panic in the compiler surfaces as an opaque "unreachable"
/// trap. Call once on start-up.
#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
}

fn to_js<T: Serialize>(value: &T) -> Result<JsValue, JsValue> {
    serde_wasm_bindgen::to_value(value).map_err(|err| JsValue::from_str(&err.to_string()))
}
