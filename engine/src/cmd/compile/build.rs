use crate::ast::ast::BlockNode;
use crate::code::Code;
use crate::error::diagnostics::Diagnostics;
use crate::lexer::lexer::{CoreLexer, Lexer};
use crate::parser::parser::{PackratParser, Parser};
use crate::parser::resolver::Resolver;
use crate::parser::type_checker::TypeChecker;

pub fn build_ast(code: &mut Code) -> (BlockNode, Vec<Diagnostics>) {
    let core_lexer = CoreLexer::new();
    let (token_vec, mut lexical_errors) = core_lexer.tokenize(code);
    let parser = PackratParser::new(&*code);
    let (ast, mut parse_errors) = parser.parse(token_vec);
    lexical_errors.append(&mut parse_errors);
    (ast, lexical_errors)
}

pub fn build(code_vec: Vec<char>) -> Result<(), Diagnostics> {
    let mut code = Code::new(code_vec);
    let (ast, mut errors) = build_ast(&mut code);
    let resolver = Resolver::new(&code);
    let (scope_table, mut semantic_errors) = resolver.resolve_ast(&ast);
    ast.set_scope(&scope_table);
    errors.append(&mut semantic_errors);
    let type_checker = TypeChecker::new(&code);
    let mut type_errors = type_checker.check_ast(&ast);
    errors.append(&mut type_errors);
    if errors.len() > 0 {
        return Err(errors[0].clone());
    }
    // TODO - return chunk in result and use VM to execute it!
    Ok(())
}
