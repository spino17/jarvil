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
    let curr_lookahead = parser.get_lookahead();
    match parser.get_core_token() {
        CoreToken::LPAREN => {
            match bfactor_expr_in_parenthesis(parser) {
                Ok(response) => return Ok(response),
                Err(err) => {
                    parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        CoreToken::NOT => {
            match bfactor_not(parser) {
                Ok(response) => return Ok(response),
                Err(err) => {
                    parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        CoreToken::TRUE => {
            match parser.expect("True") {
                Ok((response, _)) => return Ok(response),
                Err(err) => {
                    parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        CoreToken::FALSE => {
            match parser.expect("False") {
                Ok((response, _)) => return Ok(response),
                Err(err) => {
                    parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        CoreToken::IDENTIFIER(_) => {
            match parser.expect_id_and_get_data() {
                Ok((response, line_number, symbol_data)) => {
                    if symbol_data.type_eq("bool") {
                        return Ok(response);
                    } else {
                        parser.reset_lookahead(curr_lookahead);
                        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                            response.lookahead, format!(
                                "expected an identifier with type 'bool' in an boolean expression, got type '{}'", 
                                symbol_data.get_type())))
                            );
                    }
                },
                Err(err) => {
                    parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        _ => {
            Err(ParseError::SYNTAX_ERROR(SyntaxError::new(parser.get_line_number(), 
            parser.get_lookahead(), 
            format!("expected '(', 'True', 'False' or an identifier, got '{}'", 
            parser.get_token_name()))))
        }
    }
    /*
    let mut errors_vec: Vec<ParseError> = vec![];
    let curr_lookahead = parser.get_lookahead();
    match factor_expr_in_parenthesis(parser) {
        Ok((lookahead, has_float)) => return Ok((lookahead, has_float)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    match parser.expect("int") {
        Ok((lookahead, _)) => return Ok((lookahead, false)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    match parser.expect("float") {
        Ok((lookahead, _)) => return Ok((lookahead, true)),
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    match parser.expect_id_and_get_data() {
        Ok((lookahead, line_number, symbol_data)) => {
            if symbol_data.type_eq("int") {
                return Ok((lookahead, false))
            } else if symbol_data.type_eq("float") {
                return Ok((lookahead, true))
            } else {
                parser.reset_lookahead(curr_lookahead);
                return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(line_number, 
                    lookahead, format!(
                        "expected an identifier with type 'int' or 'float' in an expression, got '{}'", symbol_data.get_type())))
                    )
            }
        },
        Err(err) => {
            parser.reset_lookahead(curr_lookahead);
            errors_vec.push(err);
        }
    }
    Err(aggregate_errors(errors_vec))
     */
    
}

pub fn andtive_alternative(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.expect("and")?;
    parser.bterm()?;
    Ok(ParseSuccess{
        lookahead: parser.get_lookahead(),
        possible_err: None,
    })
}

pub fn andtive(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    let curr_lookahead = parser.get_lookahead();
    match parser.get_core_token() {
        CoreToken::AND => {
            match andtive_alternative(parser) {
                Ok(response) => return Ok(response),
                Err(err) => {
                    parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        _ => {
            match parser.expect("empty") {
                Ok((response, _)) => return Ok(response),
                Err(err) => {
                    parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        }
    }
}

pub fn bterm(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.bfactor()?;
    parser.andtive()?;
    Ok(ParseSuccess{
        lookahead: parser.get_lookahead(),
        possible_err: None,
    })
}

pub fn ortive_alternative(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.expect("or")?;
    parser.bexpr()?;
    Ok(ParseSuccess{
        lookahead: parser.get_lookahead(),
        possible_err: None,
    })
}

pub fn ortive(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    let curr_lookahead = parser.get_lookahead();
    match parser.get_core_token() {
        CoreToken::OR => {
            match ortive_alternative(parser) {
                Ok(response) => return Ok(response),
                Err(err) => {
                    parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        },
        _ => {
            match parser.expect("empty") {
                Ok((response, _)) => return Ok(response),
                Err(err) => {
                    parser.reset_lookahead(curr_lookahead);
                    return Err(err);
                }
            }
        }
    }
}

pub fn bexpr(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.bterm()?;
    parser.ortive()?;
    Ok(ParseSuccess{
        lookahead: parser.get_lookahead(),
        possible_err: None,
    })
}