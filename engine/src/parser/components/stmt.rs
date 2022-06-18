use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, SyntaxError, aggregate_errors};
use crate::lexer::token::CoreToken;

pub fn try_compound_stmt(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    match parser.get_curr_core_token() {
        CoreToken::STRUCT => {
            match parser.struct_stmt() {
                Ok(response) => return Ok(response),
                Err(err) => {
                    return Err(err);
                }
            }
        },
        CoreToken::DEF => {
            match parser.function_stmt() {
                Ok(response) => return Ok(response),
                Err(err) => {
                    return Err(err);
                }
            }
        }
        _ => {
            Err(ParseError::SYNTAX_ERROR(SyntaxError::new(parser.get_curr_line_number(), 
            parser.get_lookahead(), 
            format!("expected 'struct', 'def', got '{}'",
            PackratParser::parse_for_err_message(parser.get_curr_token_name().to_string())))))
        }
    }
}

pub fn stmt(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    // let mut errors_vec: Vec<ParseError> = vec![];
    // TODO - handle lookahead index to reset it if a production fails
    // Below is a general pattern among many production rule cases where we always have to reset lookahead back to the original
    // value when trying out new production rule after prior one failed
    // let curr_lookahead = parser.get_lookahead();
    match try_compound_stmt(parser) {
        Ok(response) => Ok(response),
        Err(_) => {
            return parser.simple_stmt();
        }
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