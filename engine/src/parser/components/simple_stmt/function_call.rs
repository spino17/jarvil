use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, SyntaxError, SemanticError};
use crate::lexer::token::CoreToken;

pub fn function_call(parser: &mut PackratParser, 
    response: ParseSuccess, is_function_call: bool, index: usize) -> Result<ParseSuccess, ParseError> {
    let token_name = parser.get_curr_token_name();
    if is_function_call {
        match parser.get_curr_core_token() {
            CoreToken::NEWLINE => {
                Ok(response)
            },
            _ => {
                let index = parser.get_index();
                return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                    parser.get_code_line(parser.get_curr_line_number(), index),
                    format!(
                    "expected '=' or 'newline', got '{}'",  PackratParser::parse_for_err_message(token_name.to_string()))))
                )
            }
        }
    } else {

        // semantic check - the matched atom should be a function, method or lambda call to be taken as a standalone statement
        let line_number = parser.get_curr_line_number();
        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
            parser.get_code_line(line_number, index),
            String::from("expected a call expression")))
        )
    }
}