use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::lexer::token::CoreToken;
use crate::errors::{ParseError, SyntaxError, SemanticError, aggregate_errors};
use std::rc::Rc;
use crate::types::{Type, CoreType, Atomic};
use crate::types::TypeCheck;

pub fn param(parser: &mut PackratParser) -> Result<(ParseSuccess, (Type, usize)), ParseError> {
    let mut errors_vec: Vec<ParseError> = vec![];
    let curr_lookahead = parser.get_lookahead();
    let index = parser.get_index();
    match parser.bexpr() {
        Ok(response) => {
            return Ok((response, (Type(Rc::new(CoreType::ATOMIC(Atomic::BOOL))), index)))
        },
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err)
        }
    }
    match parser.expr() {
        Ok((response, has_float)) => {
            if has_float {
                return Ok((response, (Type(Rc::new(CoreType::ATOMIC(Atomic::FLOAT))), index)))
            } else {
                return Ok((response, (Type(Rc::new(CoreType::ATOMIC(Atomic::INT))), index)))
            }
        },
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err)
        }
    }
    match parser.atom() {
        Ok((response, data_type, _, _)) => {
            if let Some(data_type) = data_type {
                if parser.check_next_token("\n")
                || parser.check_next_token(")")
                || parser.check_next_token(",") {
                    return Ok((response, (data_type, index)))
                } else {
                    let line_number = parser.get_curr_line_number();
                    let index = parser.get_index();
                    let err = ParseError::SYNTAX_ERROR(SyntaxError::new(
                        parser.get_code_line(line_number, index),
                        format!("expected 'newline', ',' or ')', got '{}'",
                        PackratParser::parse_for_err_message(parser.get_next_token_name().to_string())))
                    );
                    parser.reset_lookahead(curr_lookahead);
                    errors_vec.push(err)
                }
            } else {

                // semantic check - error when argument with 'None' datatype is passed
                let line_number = parser.get_curr_line_number();
                let err = ParseError::SEMANTIC_ERROR(SemanticError::new(
                    parser.get_code_line(line_number, index),
                    String::from("'None' value found"))
                );
                parser.reset_lookahead(curr_lookahead);
                errors_vec.push(err)
            }
        },
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err)
        }
    }
    match parser.expect("literal") {
        Ok((response, _)) => return Ok((response, (Type(Rc::new(CoreType::ATOMIC(Atomic::STRING))), index))),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err)
        }
    }
    Err(aggregate_errors(errors_vec))
}

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
    let (response, (param_data_type, index)) = parser.param()?;
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