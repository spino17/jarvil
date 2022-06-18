use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, SemanticError};

pub fn r_asssign(parser: &mut PackratParser,
    rule_index: usize, line_number: usize) -> Result<ParseSuccess, ParseError> {
    match rule_index {
        0 => {
            match parser.expr() {
                // rule index - 0
                Ok((response, has_float)) => {
                    if has_float {
                        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number,
                            parser.get_lookahead(), 
                            String::from(
                                "mismatched types\nidentifier declared with type 'int', got assigned with value of type 'float'")))
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
                        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number,
                            parser.get_lookahead(), 
                            String::from(
                                "mismatched types\nidentifier declared with type 'float', got assigned with value of type 'int'")))
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