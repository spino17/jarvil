use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::lexer::token::CoreToken;
use crate::errors::{ParseError, SyntaxError, SemanticError};

pub fn factor_expr_in_parenthesis(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    parser.expect("(")?;
    let (_, has_float) = parser.expr()?;
    let (response, _) = parser.expect(")")?;
    return Ok((response, has_float))
}

pub fn factor_plus(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    parser.expect("+")?;
    parser.factor()
}

pub fn factor_minus(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    parser.expect("-")?;
    parser.factor()
}

pub fn factor(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    let token_value = parser.get_curr_token_value();
    match parser.get_curr_core_token() {
        CoreToken::LPAREN => {
            return parser.factor_expr_in_parenthesis();
        },
        CoreToken::INTEGER(_) => {
            match parser.expect("int") {
                Ok((response, _)) => return Ok((response, false)),
                Err(err) => {
                    return Err(err);
                }
            }
        },
        CoreToken::FLOAT(_) => {
            match parser.expect("float") {
                Ok((response, _)) => return Ok((response, true)),
                Err(err) => {
                    return Err(err);
                }
            }
        },
        CoreToken::PLUS => {
            return parser.factor_plus();
        },
        CoreToken::MINUS => {
            return parser.factor_minus();
        },
        CoreToken::IDENTIFIER(_) => {
            /*
            match parser.expect_id() {
                Ok((response, line_number, _, data_type, is_init)) => {
                    if !is_init {
                        if let Some(token_value) = token_value {
                            return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                                line_number,
                                parser.get_code_line(line_number),
                                parser.get_index(), format!(
                                    "identifier '{}' is not initialized", token_value.0.clone())))
                                )
                        } else {
                            unreachable!("identifier token must have a value")
                        }
                    }
                    if data_type.to_string().eq("int") {
                        return Ok((response, false));
                    } else if data_type.to_string().eq("float") {
                        return Ok((response, true));
                    } else {
                        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                            line_number,
                            parser.get_code_line(line_number),
                            parser.get_index(), format!(
                                "expected an identifier with type 'int' or 'float' in an expression, got type '{}'", 
                                data_type)))
                            );
                    }
                },
                Err(err) => {
                    return Err(err);
                }
            }*/
            let (response, data_type) = parser.atom()?;
            if let Some(data_type) = data_type {
                if data_type.as_ref().eq("int") {
                    return Ok((response, false))
                } else if data_type.as_ref().eq("float") {
                    return Ok((response, true))
                } else {
                    let line_number = parser.get_curr_line_number();
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                        line_number,
                        parser.get_code_line(line_number),
                        parser.get_index(), 
                        format!("expected value with type 'int' or 'float' in an expression, got type '{}'", data_type)))
                    );
                }
            } else {
                let line_number = parser.get_curr_line_number();
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                    line_number,
                    parser.get_code_line(line_number),
                    parser.get_index(), 
                    String::from("'None' value found in expression")))
                );
            }
        },
        _ => {
            let line_number = parser.get_curr_line_number();
            Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
            line_number,
            parser.get_code_line(line_number),
            parser.get_index(), 
            format!("expected '(', 'int', 'float', '+', '-' or an identifier, got '{}'", 
            PackratParser::parse_for_err_message(parser.get_curr_token_name().to_string())))))
        }
    }
}

pub fn multitive_star(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    parser.expect("*")?;
    let (response, has_float) = parser.term()?;
    Ok((response, has_float))
}

pub fn multitive_slash(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    parser.expect("/")?;
    let (response, has_float) = parser.term()?;
    Ok((response, has_float))
}

pub fn multitive(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    match parser.get_curr_core_token() {
        CoreToken::STAR => {
            match parser.multitive_star() {
                Ok((response, has_float)) => return Ok((response, has_float)),
                Err(err) => {
                    return Err(err);
                }
            }
        },
        CoreToken::SLASH => {
            match parser.multitive_slash() {
                Ok((response, has_float)) => return Ok((response, has_float)),
                Err(err) => {
                    return Err(err);
                }
            }
        },
        _ => {
            match parser.expect("empty") {
                Ok((response, _)) => {
                    // FOLLOW(multitive)
                    if parser.check_next_token("\n") 
                    || parser.check_next_token(")") 
                    || parser.check_next_token("+") 
                    || parser.check_next_token("-")
                    || parser.check_next_token("==")
                    || parser.check_next_token(">=")
                    || parser.check_next_token(">")
                    || parser.check_next_token("<=")
                    || parser.check_next_token("<")
                    || parser.check_next_token("or") 
                    || parser.check_next_token("and")
                    || parser.check_next_token(",") {
                        return Ok((response, false))
                    } else {
                        let line_number = parser.get_curr_line_number();
                        let err = ParseError::SYNTAX_ERROR(SyntaxError::new(
                            line_number,
                            parser.get_code_line(line_number),
                             parser.get_index(),
                            format!(
                            "expected a ')', '+', '-', '*', '/', '==', '>=', '>', '<=', '<', 'or', 'and', ',' or 'newline', got '{}'", 
                            PackratParser::parse_for_err_message(parser.get_next_token_name().to_string()))
                        ));
                        return Err(err);
                    }
                },
                Err(err) => {
                    unreachable!("parsing empty string never give error, got {:?}", err)
                }
            }
        }
    }
}

pub fn term(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    let (_, is_float_in_factor) = parser.factor()?;
    let (response, is_float_in_multitive) = parser.multitive()?;
    if is_float_in_factor || is_float_in_multitive {
        Ok((response, true))
    } else {
        Ok((response, false))
    }
}

pub fn additive_plus(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    parser.expect("+")?;
    let (response, has_float) = parser.expr()?;
    Ok((response, has_float))
}

pub fn additive_minus(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    parser.expect("-")?;
    let (response, has_float) = parser.expr()?;
    Ok((response, has_float))
}

pub fn additive(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    match parser.get_curr_core_token() {
        CoreToken::PLUS => {
            match parser.additive_plus() {
                Ok((response, has_float)) => return Ok((response, has_float)),
                Err(err) => {
                    return Err(err);
                }
            }
        },
        CoreToken::MINUS => {
            match parser.additive_minus() {
                Ok((response, has_float)) => return Ok((response, has_float)),
                Err(err) => {
                    return Err(err);
                }
            }
        },
        _ => {
            match parser.expect("empty") {
                Ok((response, _)) => {
                    // FOLLOW(additive)
                    if parser.check_next_token("\n") 
                    || parser.check_next_token(")")
                    || parser.check_next_token("==")
                    || parser.check_next_token(">=")
                    || parser.check_next_token(">")
                    || parser.check_next_token("<=")
                    || parser.check_next_token("<")
                    || parser.check_next_token("or") 
                    || parser.check_next_token("and")
                    || parser.check_next_token(",") {
                        return Ok((response, false))
                    } else {
                        let line_number = parser.get_curr_line_number();
                        let err = ParseError::SYNTAX_ERROR(SyntaxError::new(
                            line_number, 
                            parser.get_code_line(line_number),
                            parser.get_index(),
                            format!(
                            "expected a ')', '+', '-', '*', '/' '==', '>=', '>', '<=', '<', 'or', 'and', ',' or 'newline', got '{}'", 
                            PackratParser::parse_for_err_message(parser.get_next_token_name().to_string()))
                        ));
                        return Err(err);
                    }
                },
                Err(err) => {
                    unreachable!("parsing empty string never give error, got {:?}", err)
                }
            }
        }
    }
}

pub fn expr(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    let (_, is_float_in_term) = parser.term()?;
    let (response, is_float_in_additive) = parser.additive()?;
    if is_float_in_term || is_float_in_additive {
        return Ok((response, true))
    } else {
        return Ok((response, false))
    }
}