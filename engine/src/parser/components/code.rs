use crate::parser::packrat::PackratParser;
use crate::lexer::token::Token;
use crate::errors::{ParseError,aggregate_errors};

pub fn code(parser: &mut PackratParser, token_vec: Vec<Token>) -> Result<(), ParseError> {
    let mut errors_vec: Vec<ParseError> = vec![];
    parser.set_token_vec(token_vec);
    parser.stmt()?;
    let curr_lookahead = parser.get_lookahead();
    let response = PackratParser::expect_zero_or_more(|| {
        let response = parser.stmt()?;
        Ok(response)
    }, curr_lookahead);
    parser.reset_lookahead(response.lookahead);
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