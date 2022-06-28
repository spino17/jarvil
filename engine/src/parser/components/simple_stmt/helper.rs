use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, SemanticError};
use std::rc::Rc;

pub fn param_decl(parser: &mut PackratParser) -> Result<(ParseSuccess, usize, Rc<String>, Rc<String>), ParseError> {
    let (_, _, data_type, _) = parser.expect_type()?;
    let (response, line_number, token_value) = parser.expect_any_id()?;
    Ok((response, line_number, data_type, token_value))
}

pub fn r_asssign(parser: &mut PackratParser,
    rule_index: usize, line_number: usize) -> Result<ParseSuccess, ParseError> {
    match rule_index {
        0 => {
            match parser.expr() {
                // rule index - 0
                Ok((response, has_float)) => {
                    if has_float {
                        let index = parser.get_index();
                        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                            parser.get_code_line(line_number, index),
                            String::from(
                                "mismatched types\n    identifier declared with type 'int', got assigned with value of type 'float'")))
                            )
                    }
                    return Ok(response)
                },
                Err(err) => {
                    return Err(err)
                }
            }
        },
        1 => {
            match parser.expr() {
                // rule index - 1
                Ok((response, has_float)) => {
                    if !has_float {
                        let index = parser.get_index();
                        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                            parser.get_code_line(line_number, index),
                            String::from(
                                "mismatched types\n    identifier declared with type 'float', got assigned with value of type 'int'")))
                            )
                    }
                    return Ok(response)
                },
                Err(err) => {
                    return Err(err)
                }
            }
        },
        2 => {
            match parser.bexpr() {
                // rule index - 2
                Ok(response) => return Ok(response),
                Err(err) => {
                    return Err(err)
                }
            }
        },
        3 => {
            match parser.expect("literal") {
                // rule index - 3
                Ok((response, _)) => return Ok(response),
                Err(err) => {
                    return Err(err)
                }
            }
        },
        _ => {
            unreachable!("rule index can only be 0, 1, 2 and 3")
        }
    }
}