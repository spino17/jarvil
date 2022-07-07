use crate::parser::parser::{PackratParser, ParseSuccess};
use crate::lexer::token::CoreToken;
use crate::errors::{ParseError, SyntaxError, SemanticError};
use std::rc::Rc;
use crate::types::core::{Type};
use crate::types::core::TypeCheck;

pub fn params(parser: &mut PackratParser,
    expected_params: &Rc<Vec<(Rc<String>, Type)>>, param_index: usize) -> Result<(ParseSuccess, usize), ParseError> {
    let expected_params_len = expected_params.as_ref().len();
    match parser.get_curr_core_token() {
        CoreToken::RPAREN => {
            if param_index < expected_params.as_ref().len() {
                let index = parser.get_index();
                let line_number = parser.get_curr_line_number();
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                    parser.get_code_line(line_number, index),
                    format!("expected '{}' number of arguments to the function, got '{}'", 
                    expected_params_len, param_index)))
                )
            }
            return Ok((ParseSuccess{
                lookahead: parser.get_lookahead(),
                possible_err: None,
            }, parser.get_curr_line_number()))
        },
        _ => {}
    }
    let (response, (param_data_type, index)) = parser.expr()?;
    let line_number = parser.get_curr_line_number();
    if param_index >= expected_params_len {
        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
            parser.get_code_line(line_number, index),
            format!("expected '{}' number of arguments to the function, got more than that", 
            expected_params_len)))
        )
    }
    let expected_param_data_type = &expected_params.as_ref()[param_index].1;
    if !param_data_type.is_eq(expected_param_data_type) {
        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
            parser.get_code_line(line_number, index),
            format!("expected type '{}' for argument '{}', got '{}'",
            expected_param_data_type, param_index + 1, param_data_type)))
        )
    }
    let token_name = parser.get_curr_token_name();
    match parser.get_curr_core_token() {
        CoreToken::RPAREN => {
            if param_index + 1 < expected_params.as_ref().len() {
                let index = parser.get_index();
                let line_number = parser.get_curr_line_number();
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                    parser.get_code_line(line_number, index),
                    format!("expected '{}' number of arguments to the function, got '{}'", 
                    expected_params_len, param_index + 1)))
                )
            }
            Ok((response, line_number))
        },
        CoreToken::COMMA => {
            parser.expect(",")?;
            let (response, line_number)
            = parser.params(expected_params, param_index + 1)?;
            Ok((response, line_number))
        }
        _ => {
            let index = parser.get_index();
            return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                parser.get_code_line(line_number, index),
                format!("expected ',' or ')', got '{}'", 
                PackratParser::parse_for_err_message(token_name.to_string()))))
            )
        }
    }
}