use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, aggregate_errors, SyntaxError};
use crate::lexer::token::CoreToken;

pub fn simple_stmt(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    let mut errors_vec: Vec<ParseError> = vec![];
    let response = parser.simple_stmt_alternatives()?;
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

pub fn simple_stmt_alternatives(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    match parser.get_curr_core_token() {
        CoreToken::LET => {
            return parser.decls()
        },
        _ => {
            let index = parser.get_index();
            let (response, data_type, is_assignable, is_function_call) = parser.atom()?;
            let token_name = parser.get_curr_token_name();
            match parser.get_curr_core_token() {
                CoreToken::EQUAL => {
                    return parser.assign(data_type, is_assignable, index)
                },
                CoreToken::NEWLINE => {
                    return parser.function_call(response, is_function_call, index)
                },
                _ => {
                    let index = parser.get_index();
                    let line_number = parser.get_curr_line_number();
                    return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                        parser.get_code_line(line_number, index),
                        format!("expected '=' or 'newline', got '{}'", 
                        PackratParser::parse_for_err_message(token_name.to_string()))))
                    )
                }
            }
        }
    }
    /*
    match parser.decls() {
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
    */
    /*
    match parser.function_call(false) {
        Ok(response) => return Ok(response.0),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }*/
}