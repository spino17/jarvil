use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, aggregate_errors};

pub fn simple_stmts(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    let mut errors_vec: Vec<ParseError> = vec![];
    let response = parser.simple_stmt()?;
    if let Some(err) = response.possible_err {
        errors_vec.push(err);
    }
    match parser.expect("\n") {
        Ok((_, _)) => {
            return Ok(ParseSuccess{
                lookahead: parser.get_lookahead(),
                possible_err: None,
            });
        },
        Err(err) => errors_vec.push(err)
    }
    Err(aggregate_errors(errors_vec))
}

pub fn simple_stmt(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    let mut errors_vec: Vec<ParseError> = vec![];
    let curr_lookahead = parser.get_lookahead();
    match parser.decls() {
        Ok(response) => return Ok(response),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    match parser.assign() {
        Ok(response) => return Ok(response),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    match parser.atom() {
        Ok(response) => {
            return Ok(response.0)
        },
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    /*
    match parser.function_call(false) {
        Ok(response) => return Ok(response.0),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }*/
    Err(aggregate_errors(errors_vec))
}