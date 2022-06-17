use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::lexer::token::CoreToken;
use crate::errors::{ParseError, SyntaxError, SemanticError, aggregate_errors};

pub fn factor_expr_in_parenthesis(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    parser.expect("(")?;
    let (_, has_float) = parser.expr()?;
    parser.expect(")")?;
    return Ok((ParseSuccess{
        lookahead: parser.get_lookahead(),
        possible_err: None,
    }, has_float))
}

pub fn factor(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    let curr_lookahead = parser.get_lookahead();
    let token_value = parser.get_curr_token_value();
    match parser.get_curr_core_token() {
        CoreToken::LPAREN => {
            match factor_expr_in_parenthesis(parser) {
                Ok((response, has_float)) => return Ok((response, has_float)),
                Err(err) => {
                    // parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        CoreToken::INTEGER(_) => {
            match parser.expect("int") {
                Ok((response, _)) => return Ok((response, false)),
                Err(err) => {
                    // parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        CoreToken::FLOAT(_) => {
            match parser.expect("float") {
                Ok((response, _)) => return Ok((response, true)),
                Err(err) => {
                    // parser.reset_lookahead(curr_lookahead);
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
                    if symbol_data.type_eq("int") {
                        return Ok((response, false));
                    } else if symbol_data.type_eq("float") {
                        return Ok((response, true));
                    } else {
                        // parser.reset_lookahead(curr_lookahead);
                        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                            response.lookahead, format!(
                                "expected an identifier with type 'int' or 'float' in an expression, got type '{}'", symbol_data.get_type())))
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
            format!("expected '(', 'int', 'float' or an identifier, got '{}'", parser.get_curr_token_name()))))
        }
    }
}

pub fn star_multitive_alternative(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    parser.expect("*")?;
    let (response, has_float) = parser.term()?;
    Ok((response, has_float))
}

pub fn slash_multitive_alternative(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    parser.expect("/")?;
    let (response, has_float) = parser.term()?;
    Ok((response, has_float))
}

pub fn multitive(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    let curr_lookahead = parser.get_lookahead();
    match parser.get_curr_core_token() {
        CoreToken::STAR => {
            match star_multitive_alternative(parser) {
                Ok((response, has_float)) => return Ok((response, has_float)),
                Err(err) => {
                    // parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        CoreToken::SLASH => {
            match slash_multitive_alternative(parser) {
                Ok((response, has_float)) => return Ok((response, has_float)),
                Err(err) => {
                    // parser.reset_lookahead(curr_lookahead);
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
                    || parser.check_next_token("-") {
                        return Ok((response, false))
                    } else {
                        let err = ParseError::SYNTAX_ERROR(SyntaxError::new(
                            parser.get_curr_line_number(), parser.get_lookahead(),
                            format!("expected a '+', '-', '*' or '/', got '{}'", parser.get_next_token_name())
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

pub fn plus_additive_alternative(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    parser.expect("+")?;
    let (response, has_float) = parser.expr()?;
    Ok((response, has_float))
}

pub fn minus_additive_alternative(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    parser.expect("-")?;
    let (response, has_float) = parser.expr()?;
    Ok((response, has_float))
}

pub fn additive(parser: &mut PackratParser) -> Result<(ParseSuccess, bool), ParseError> {
    let curr_lookahead = parser.get_lookahead();
    match parser.get_curr_core_token() {
        CoreToken::PLUS => {
            match plus_additive_alternative(parser) {
                Ok((response, has_float)) => return Ok((response, has_float)),
                Err(err) => {
                    // parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        CoreToken::MINUS => {
            match minus_additive_alternative(parser) {
                Ok((response, has_float)) => return Ok((response, has_float)),
                Err(err) => {
                    // parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        _ => {
            match parser.expect("empty") {
                Ok((response, _)) => {
                    // FOLLOW(additive)
                    if parser.check_next_token("\n") 
                    || parser.check_next_token(")") {
                        return Ok((response, false))
                    } else {
                        let err = ParseError::SYNTAX_ERROR(SyntaxError::new(
                            parser.get_curr_line_number(), parser.get_lookahead(),
                            format!("expected a '+', '-', '*' or '/', got '{}'", parser.get_next_token_name())
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