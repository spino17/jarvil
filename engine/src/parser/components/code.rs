use crate::ast::ast::{BlockNode, BlockKind};
use crate::constants::common::ENDMARKER;
use crate::lexer::token::Token;
use crate::parser::components::statement::core::{
    is_statement_starting_with, STATEMENT_EXPECTED_STARTING_SYMBOLS,
};
use crate::parser::parser::PackratParser;

pub fn code(parser: &mut PackratParser, token_vec: Vec<Token>) -> BlockNode {
    parser.set_token_vec(token_vec);
    let block_node = parser.block(
        |token| is_statement_starting_with(token),
        |parser| parser.stmt(),
        &STATEMENT_EXPECTED_STARTING_SYMBOLS,
        BlockKind::TOP
    );
    parser.expect(ENDMARKER);
    block_node
}
