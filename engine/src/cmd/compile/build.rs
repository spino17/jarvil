use crate::code::Code;
use crate::context;
use crate::errors::JarvilError;
use crate::utils::common::build_ast;

pub fn build(code_vec: Vec<char>) -> Result<(), JarvilError> {
    let mut code = Code::new(code_vec);
    let (ast, errors) = build_ast(&mut code);
    if errors.len() > 0 {
        println!("{:?}", errors);
        return Err(errors[0].clone())
    }
    // TODO - use `ast` for name resolution, type checking and code generation
    Ok(())
}
