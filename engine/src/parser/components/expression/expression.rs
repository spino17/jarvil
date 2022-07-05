use crate::parser::parser::{PackratParser, ParseSuccess};
use crate::lexer::token::CoreToken;
use crate::errors::{ParseError, SyntaxError, SemanticError};
use crate::constants::common::{INT,FLOAT};

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
    match parser.get_curr_core_token() {
        CoreToken::LPAREN => {
            return parser.factor_expr_in_parenthesis();
        },
        CoreToken::INTEGER(_) => {
            match parser.expect(INT) {
                Ok((response, _)) => return Ok((response, false)),
                Err(err) => {
                    return Err(err);
                }
            }
        },
        CoreToken::FLOAT(_) => {
            match parser.expect(FLOAT) {
                Ok((response, _)) => return Ok((response, true)),
                Err(err) => {
                    return Err(err);
                }
            }
        },
        CoreToken::PLUS => {
            return parser.factor_plus();
        },
        CoreToken::DASH => {
            return parser.factor_minus();
        },
        CoreToken::IDENTIFIER(_) => {
            let index = parser.get_index();
            let (response, data_type, _, _) = parser.atom()?;

            // semantic check - type of matched atom should be int or float
            if let Some(data_type) = data_type {
                if data_type.is_atomic(INT) {
                    return Ok((response, false))
                } else if data_type.is_atomic(FLOAT) {
                    return Ok((response, true))
                } else {
                    let line_number = parser.get_curr_line_number();
                    return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                        parser.get_code_line(line_number, index),
                        format!("expected value with type 'int' or 'float' in an expression, got type '{}'", data_type)))
                    );
                }
            } else {
                let line_number = parser.get_curr_line_number();
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                    parser.get_code_line(line_number, index),
                    String::from("'None' value found in expression")))
                );
            }
        },
        _ => {
            let line_number = parser.get_curr_line_number();
            let index = parser.get_index();
            Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
            parser.get_code_line(line_number, index), 
            format!("expected '(', 'int', 'float', '+', '-' or identifier, got '{}'", 
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
                        let index = parser.get_index();
                        let err = ParseError::SYNTAX_ERROR(SyntaxError::new(
                            parser.get_code_line(line_number, index),
                            format!(
                            "expected ')', '+', '-', '*', '/', '==', '>=', '>', '<=', '<', 'or', 'and', ',' or 'newline', got '{}'", 
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
    Ok((response, is_float_in_factor || is_float_in_multitive))
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
        CoreToken::DASH => {
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
                        let index = parser.get_index();
                        let err = ParseError::SYNTAX_ERROR(SyntaxError::new(
                            parser.get_code_line(line_number, index),
                            format!(
                            "expected ')', '+', '-', '*', '/' '==', '>=', '>', '<=', '<', 'or', 'and', ',' or 'newline', got '{}'", 
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
    Ok((response, is_float_in_term || is_float_in_additive))
}