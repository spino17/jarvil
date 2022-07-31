use crate::ast::ast::BlockNode;
use crate::code::Code;
use crate::lexer::lexer::{CoreLexer, Lexer};
use crate::parser::parser::{PackratParser, Parser};

pub fn build_ast(code: &mut Code) -> BlockNode {
    let mut core_lexer = CoreLexer::new();
    let token_vec = core_lexer.tokenize(code);
    let parser = PackratParser::new(&*code);
    let ast = parser.parse(token_vec);
    ast
}
