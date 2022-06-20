use crate::parser::packrat::PackratParser;
use crate::lexer::token::Token;
use crate::errors::{ParseError,aggregate_errors};

pub fn code(parser: &mut PackratParser, token_vec: Vec<Token>) -> Result<(), ParseError> {
    let mut errors_vec: Vec<ParseError> = vec![];
    parser.set_token_vec(token_vec);
    let response = parser.block(None)?;
    if let Some(err) = response.possible_err {
        errors_vec.push(err);
    }
    match parser.expect("endmarker") {
        Ok((_, _)) => {
            return Ok(());
        },
        Err(err) => errors_vec.push(err)
    }
    Err(aggregate_errors(errors_vec))
}