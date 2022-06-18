use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, aggregate_errors};
use crate::lexer::token::CoreToken;

pub fn try_compound_stmt(parser: &mut PackratParser) -> bool {
    match parser.get_curr_core_token() {
        CoreToken::STRUCT => {
            true
        },
        CoreToken::DEF => {
            true
        },
        CoreToken::IF => {
            true
        },
        CoreToken::FOR => {
            true
        },
        CoreToken::WHILE => {
            true
        }
        _ => {
            false
        }
    }
}

pub fn stmt(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    // let mut errors_vec: Vec<ParseError> = vec![];
    // TODO - handle lookahead index to reset it if a production fails
    // Below is a general pattern among many production rule cases where we always have to reset lookahead back to the original
    // value when trying out new production rule after prior one failed
    // let curr_lookahead = parser.get_lookahead();
    if try_compound_stmt(parser) {
        parser.compound_stmt()
    } else {
        parser.simple_stmts()
    }
    /*
    match parser.compound_stmt() {
        Ok(response) => return Ok(response),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    match parser.simple_stmts() {
        Ok(response) => return Ok(response),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    Err(aggregate_errors(errors_vec))
     */
}