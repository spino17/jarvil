use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::lexer::token::CoreToken;
use crate::errors::{ParseError, SyntaxError, SemanticError};

pub fn bfactor_expr_in_parenthesis(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.expect("(")?;
    parser.bexpr()?;
    parser.expect(")")?;
    Ok(ParseSuccess{
        lookahead: parser.get_lookahead(),
        possible_err: None,
    })
}

pub fn bfactor_not(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.expect("not")?;
    parser.bfactor()?;
    Ok(ParseSuccess{
        lookahead: parser.get_lookahead(),
        possible_err: None,
    })
}

pub fn bfactor(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    // let curr_lookahead = parser.get_lookahead();
    let token_value = parser.get_curr_token_value();
    match parser.get_curr_core_token() {
        CoreToken::LPAREN => {
            match bfactor_expr_in_parenthesis(parser) {
                Ok(response) => return Ok(response),
                Err(err) => {
                    // parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        CoreToken::NOT => {
            match bfactor_not(parser) {
                Ok(response) => return Ok(response),
                Err(err) => {
                    // parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        CoreToken::TRUE => {
            match parser.expect("True") {
                Ok((response, _)) => return Ok(response),
                Err(err) => {
                    // parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        CoreToken::FALSE => {
            match parser.expect("False") {
                Ok((response, _)) => return Ok(response),
                Err(err) => {
                    // arser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        CoreToken::IDENTIFIER(_) => {
            match parser.expect_id_and_get_data() {
                Ok((response, line_number, symbol_data)) => {
                    if !symbol_data.is_init() {
                        if let Some(token_value) = token_value {
                            return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(line_number,
                                response.lookahead, format!(
                                    "identifier '{}' is not initialized", token_value.0.clone())))
                                )
                        } else {
                            unreachable!("identifier token must have a value")
                        }
                    }
                    if symbol_data.type_eq("bool") {
                        return Ok(response);
                    } else {
                        // parser.reset_lookahead(curr_lookahead);
                        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                            response.lookahead, format!(
                                "expected an identifier with type 'bool' in an boolean expression, got type '{}'", 
                                symbol_data.get_type())))
                            );
                    }
                },
                Err(err) => {
                    // parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        _ => {
            Err(ParseError::SYNTAX_ERROR(SyntaxError::new(parser.get_curr_line_number(), 
            parser.get_lookahead(), 
            format!("expected '(', 'True', 'False' or an identifier, got '{}'", 
            parser.get_curr_token_name()))))
        }
    }
}

pub fn andtive_alternative(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.expect("and")?;
    let response = parser.bterm()?;
    Ok(response)
}

pub fn andtive(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    // let curr_lookahead = parser.get_lookahead();
    match parser.get_curr_core_token() {
        CoreToken::AND => {
            match andtive_alternative(parser) {
                Ok(response) => return Ok(response),
                Err(err) => {
                    // parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        _ => {
            match parser.expect("empty") {
                Ok((response, _)) => {
                    if parser.check_next_token("\n") 
                    || parser.check_next_token(")") 
                    || parser.check_next_token("or") {
                        return Ok(response)
                    } else {
                        let err = ParseError::SYNTAX_ERROR(SyntaxError::new(
                            parser.get_curr_line_number(), parser.get_lookahead(),
                            format!("expected a ')', 'or' or 'and', got '{}'", parser.get_next_token_name())
                        ));
                        return Err(err);
                    }
                },
                Err(err) => {
                    // parser.reset_lookahead(curr_lookahead);
                    unreachable!("parsing empty string never give error, got {:?}", err)
                }
            }
        }
    }
}

pub fn bterm(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.bfactor()?;
    let response = parser.andtive()?;
    Ok(response)
}

pub fn ortive_alternative(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.expect("or")?;
    let response = parser.bexpr()?;
    Ok(response)
}

pub fn ortive(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    // let curr_lookahead = parser.get_lookahead();
    match parser.get_curr_core_token() {
        CoreToken::OR => {
            match ortive_alternative(parser) {
                Ok(response) => return Ok(response),
                Err(err) => {
                    // parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        _ => {
            match parser.expect("empty") {
                Ok((response, _)) => {
                    if parser.check_next_token("\n") 
                    || parser.check_next_token(")") {
                        return Ok(response)
                    } else {
                        let err = ParseError::SYNTAX_ERROR(SyntaxError::new(
                            parser.get_curr_line_number(), parser.get_lookahead(),
                            format!("expected a ')', 'or' or 'and', got '{}'", parser.get_next_token_name())
                        ));
                        return Err(err);
                    }
                },
                Err(err) => {
                    // parser.reset_lookahead(curr_lookahead);
                    unreachable!("parsing empty string never give error, got {:?}", err)
                }
            }
        }
    }
}

pub fn bexpr(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.bterm()?;
    let response = parser.ortive()?;
    Ok(response)
}