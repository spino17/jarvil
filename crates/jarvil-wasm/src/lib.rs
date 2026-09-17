//! Browser bindings for the Jarvil compiler.
//!
//! Exposes a single [`compile`] function over `wasm-bindgen`, intended for a
//! web playground: source text in, either generated Python or a rendered error
//! out.
//!
//! Diagnostics come back as a string containing ANSI escape codes, because the
//! expected consumer is a terminal emulator in the page. See [`compile`].

use jarvil_parser::code::JarvilCode;
use jarvil_py::build_code;
use miette::{GraphicalReportHandler, GraphicalTheme};
use owo_colors::Style;
use std::fmt::Write;
use std::str;
use wasm_bindgen::prelude::*;

/// Compiles Jarvil source, for calling from JavaScript.
///
/// # Errors
///
/// Returns the first diagnostic, rendered with ANSI escape codes and Unicode
/// box drawing on the assumption that the page displays it in a terminal
/// emulator. Plain text is not currently available through this entry point.
#[wasm_bindgen]
pub fn compile(code_str: &str) -> Result<String, String> {
    let _ = miette::set_hook(Box::new(|_err| {
        // Explicitly `unicode()` rather than `default()`: the default probes for
        // a terminal, which under wasm is never there, so it would downgrade to
        // an uncoloured ASCII theme. The caller here is a web front-end that
        // renders the escape codes, so ask for the styled theme directly.
        let mut my_theme = GraphicalTheme::unicode();

        my_theme.styles.linum = Style::new().bright_blue();
        my_theme.styles.error = Style::new().red();
        my_theme.styles.warning = Style::new().yellow();
        my_theme.styles.advice = Style::new().yellow();
        my_theme.styles.help = Style::new().white();
        Box::new(GraphicalReportHandler::new_themed(my_theme))
    }));
    console_error_panic_hook::set_once();
    let code = JarvilCode::new(code_str);
    let (py_result, _) = build_code(code);
    let mut buffer = String::new();
    match py_result {
        Ok(py_code) => Ok(py_code),
        Err(err) => match write!(&mut buffer, "{:?}", err) {
            Ok(()) => Err(buffer),
            Err(err) => Err(format!("Failed to write Jarvil error to buffer: {}", err)),
        },
    }
}
