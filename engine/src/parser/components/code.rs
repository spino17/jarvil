use crate::ast::ast::BlockNode;
use crate::parser::parser::PackratParser;
use crate::lexer::token::Token;
use crate::errors::{SyntaxError};

pub fn code(parser: &mut PackratParser, token_vec: Vec<Token>) -> Result<BlockNode, SyntaxError> {
    parser.set_token_vec(token_vec);
    let (response, block_node) = parser.block(vec![], None)?;
    parser.expect("endmarker");
    todo!()
}