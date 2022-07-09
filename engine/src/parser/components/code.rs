use crate::ast::ast::BlockNode;
use crate::parser::parser::PackratParser;
use crate::lexer::token::Token;
use std::rc::Rc;

pub fn code(parser: &mut PackratParser, token_vec: Vec<Token>) -> BlockNode {
    parser.set_token_vec(token_vec);
    let block_node = parser.block(&Rc::new(vec![]), None);
    parser.expect("endmarker", true);
    block_node
}