use crate::ast::ast::BlockNode;
use crate::parser::parser::PackratParser;
use crate::lexer::token::Token;
use crate::errors::{SyntaxError, aggregate_errors};

pub fn code(parser: &mut PackratParser, token_vec: Vec<Token>) -> Result<BlockNode, SyntaxError> {
    let mut errors_vec: Vec<SyntaxError> = vec![];
    parser.set_token_vec(token_vec);
    let (response, block_node) = parser.block(vec![], None)?;
    if let Some(err) = response.possible_err {
        errors_vec.push(err);
    }
    match parser.expect("endmarker") {
        Ok(_) => {
            return Ok(block_node);
        },
        Err(err) => errors_vec.push(err)
    }
    Err(aggregate_errors(errors_vec))
}