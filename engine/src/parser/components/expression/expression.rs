use crate::parser::parser::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, SyntaxError, SemanticError, aggregate_errors};
use std::rc::Rc;
use crate::types::core::{Type, CoreType, Atomic};

pub fn expr(parser: &mut PackratParser) -> Result<(ParseSuccess, (Type, usize)), ParseError> {
    let mut errors_vec: Vec<ParseError> = vec![];
    let curr_lookahead = parser.get_lookahead();
    let index = parser.get_index();
    match parser.bool_expr() {
        Ok(response) => {
            return Ok((response, (Type(Rc::new(CoreType::ATOMIC(Atomic::BOOL))), index)))
        },
        Err(err) => {
            println!("bexpr error: {:?}", err);
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err)
        }
    }
    match parser.numeric_expr() {
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
                        format!("expected ',', ')' or 'newline', got '{}'",
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