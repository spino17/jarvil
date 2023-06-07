use compiler::{build_code, code::JarvilCode};
use miette::{GraphicalReportHandler, GraphicalTheme};
use owo_colors::Style;
use std::fmt::Write;
use std::str;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn compile(code_str: &str) -> Result<String, String> {
    miette::set_hook(Box::new(|_err| {
        let mut my_theme = GraphicalTheme::default();
        my_theme.styles.linum = Style::new().bright_blue();
        my_theme.styles.error = Style::new().red();
        my_theme.styles.warning = Style::new().yellow();
        my_theme.styles.advice = Style::new().yellow();
        my_theme.styles.help = Style::new().white();
        Box::new(GraphicalReportHandler::new_themed(my_theme))
    }));
    console_error_panic_hook::set_once();
    let code = JarvilCode::new(code_str);
    let py_result = build_code(code, code_str.to_string());
    let mut buffer = String::new();
    match py_result {
        Ok(py_code) => return Ok(py_code),
        Err(err) => match write!(&mut buffer, "{:?}", err) {
            Ok(()) => {
                return Err(buffer);
            }
            Err(err) => {
                return Err(format!(
                    "Failed to write Jarvil error to buffer: {}",
                    err.to_string()
                ))
            }
        },
    }
}
