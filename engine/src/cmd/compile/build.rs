use crate::backend::chunk::Chunk;
use crate::code::Code;
use crate::error::core::JarvilError;
use crate::parser::resolver::Resolver;
use crate::parser::type_checker::TypeChecker;
use crate::utils::common::build_ast;

pub fn build(code_vec: Vec<char>) -> Result<Chunk, JarvilError> {
    // TODO - change this to Result<Chunk, JarvilError>
    let mut code = Code::new(code_vec);
    let (ast, mut errors) = build_ast(&mut code);
    let mut resolver = Resolver::new(&code);
    let (scope_table, mut semantic_errors) = resolver.resolve_ast(&ast);
    ast.set_scope(&scope_table);
    errors.append(&mut semantic_errors);
    let mut type_checker = TypeChecker::new(&code, &scope_table);
    let mut type_errors = type_checker.check_ast(&ast);
    errors.append(&mut type_errors);
    if errors.len() > 0 {
        return Err(errors[0].clone());
    }
    Ok(Chunk::default())
}
