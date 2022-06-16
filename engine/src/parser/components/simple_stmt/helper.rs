use crate::parser::packrat::PackratParser;
use crate::errors::{ParseError, SemanticError, aggregate_errors};

pub fn r_asssign_alternatives(parser: &mut PackratParser) -> Result<(usize, usize, bool), ParseError> {
    let mut errors_vec: Vec<ParseError> = vec![];
    let curr_lookahead = parser.get_lookahead();
    match parser.expr() {
        Ok((lookahead, has_float)) => return Ok((lookahead, 0, has_float)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            // errors_vec.push(err);
            return Err(err)  // TODO - just for testing purpose
        }
    }
    match parser.bexpr() {
        Ok(lookahead) => return Ok((lookahead, 1, false)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    match parser.expect("literal") {
        Ok((lookahead, _)) => return Ok((lookahead, 2, false)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    // TODO - add for new id(optparams) for user-defined types
    Err(aggregate_errors(errors_vec))
}