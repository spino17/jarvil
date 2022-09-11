use crate::ast::ast::BlockNode;
use crate::code::Code;
use crate::error::diagnostics::Diagnostics;
use crate::lexer::lexer::{CoreLexer, Lexer};
use crate::parser::parser::{PackratParser, Parser};

pub fn build_ast(code: &mut Code) -> (BlockNode, Vec<Diagnostics>) {
    let mut core_lexer = CoreLexer::new();
    let (token_vec, mut lexical_errors) = core_lexer.tokenize(code);
    let parser = PackratParser::new(&*code);
    let (ast, mut parse_errors) = parser.parse(token_vec);
    lexical_errors.append(&mut parse_errors);
    (ast, lexical_errors)
}
