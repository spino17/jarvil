use crate::ast::ast::BlockNode;
use crate::constants::common::ENDMARKER;
use crate::lexer::token::Token;
use crate::parser::components::statement::core::{
    is_statement_at_global_scope_starting_with, STATEMENT_AT_GLOBAL_SCOPE_STARTING_SYMBOLS,
};
use crate::parser::parser::JarvilParser;

pub fn code(parser: &mut JarvilParser, token_vec: Vec<Token>) -> BlockNode {
    parser.set_token_vec(token_vec);
    let block_node = parser.block(
        |token| is_statement_at_global_scope_starting_with(token),
        |parser| parser.stmt(),
        &STATEMENT_AT_GLOBAL_SCOPE_STARTING_SYMBOLS,
    );
    parser.expect(ENDMARKER);
    block_node
}
