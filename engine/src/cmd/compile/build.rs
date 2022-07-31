use crate::code::Code;
use crate::context;
use crate::errors::JarvilError;
use crate::utils::common::build_ast;

pub fn build(code_vec: Vec<char>) -> Result<(), JarvilError> {
    let mut code = Code::new(code_vec);
    let ast = build_ast(&mut code);
    match context::first_error() {
        Some(err) => return Err(err),
        None => {}
    }
    // TODO - use `ast` for name resolution, type checking and code generation
    Ok(())
}
