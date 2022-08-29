use crate::code::Code;
use crate::error::core::JarvilError;
use crate::parser::resolver::Resolver;
use crate::utils::common::build_ast;

pub fn build(code_vec: Vec<char>) -> Result<(), JarvilError> {
    let mut code = Code::new(code_vec);
    let (ast, mut parser_errors) = build_ast(&mut code);
    let mut resolver = Resolver::new(&code);
    let (scope_table, mut semantic_errors) = resolver.resolve_ast(&ast);
    parser_errors.append(&mut semantic_errors);
    if parser_errors.len() > 0 {
        return Err(parser_errors[0].clone())
    }
    // TODO - use `ast` for name resolution, type checking and code generation
    Ok(())
}
