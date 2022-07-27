use crate::utils::common::build_ast;
use crate::context;
use crate::errors::ParseError;
use crate::code::Code;

pub fn build(code_vec: Vec<char>) -> Result<(), ParseError> {
    let mut code = Code::new(code_vec);
    let ast = build_ast(&mut code);
    match context::first_error() {
        Some(err) => {
            return Err(err)
        },
        None => {}
    }
    // TODO - use `ast` for name resolution, type checking and code generation
    Ok(())
}