use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, aggregate_errors};

pub fn r_asssign_alternatives(parser: &mut PackratParser) -> Result<(ParseSuccess, usize, bool), ParseError> {
    let mut errors_vec: Vec<ParseError> = vec![];
    let curr_lookahead = parser.get_lookahead();
    match parser.expr() {
        // rule index - 0
        Ok((response, has_float)) => return Ok((response, 0, has_float)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    match parser.bexpr() {
        // rule index - 1
        Ok(response) => return Ok((response, 1, false)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    match parser.expect("literal") {
        // rule index - 2
        Ok((response, _)) => return Ok((response, 2, false)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    // TODO - add for new id(optparams) for user-defined types
    Err(aggregate_errors(errors_vec))
}