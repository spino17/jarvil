use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, SyntaxError};
use crate::lexer::token::CoreToken;

pub fn compound_stmt(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    match parser.get_curr_core_token() {
        CoreToken::STRUCT => {
            match parser.struct_stmt() {
                Ok(response) => return Ok(response),
                Err(err) => {
                    return Err(err);
                }
            }
        },
        _ => {
            Err(ParseError::SYNTAX_ERROR(SyntaxError::new(parser.get_curr_line_number(), 
            parser.get_lookahead(), 
            format!("expected 'struct', got '{}'",
            PackratParser::parse_for_err_message(parser.get_curr_token_name().to_string())))))
        }
    }
}