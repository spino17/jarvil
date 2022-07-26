use crate::utils::common::build_ast;
use crate::context;
use crate::errors::ParseError;

pub fn build(code_vec: Vec<char>) -> Result<(), ParseError> {
    let ast = build_ast(code_vec);
    match context::first_error() {
        Some(err) => {
            return Err(err)
        },
        None => {}
    }
    // TODO - use `ast` for name resolution, type checking and code generation
    Ok(())
}