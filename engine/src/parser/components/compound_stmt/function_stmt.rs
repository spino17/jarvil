use std::rc::Rc;
use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::ParseError;
use crate::lexer::token::CoreToken;
use crate::errors::SyntaxError;

pub fn optparams_factor(parser: &mut PackratParser) -> Result<(ParseSuccess, Vec<(Rc<String>, Rc<String>)>), ParseError> {
    match parser.get_curr_core_token() {
        CoreToken::COMMA => {
            parser.expect(",")?;
            parser.optparams()
        },
        _ => {
            match parser.expect("empty") {
                Ok((response, _)) => {
                    // FOLLOW(optparams)
                    if parser.check_next_token(")") {
                        return Ok((response, vec![]))
                    } else {
                        let err = ParseError::SYNTAX_ERROR(SyntaxError::new(
                            parser.get_curr_line_number(), parser.get_lookahead(),
                            format!(
                            "expected a ')', got '{}'", PackratParser::parse_for_err_message(
                                parser.get_next_token_name().to_string())
                            )
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

pub fn optparams(parser: &mut PackratParser) -> Result<(ParseSuccess, Vec<(Rc<String>, Rc<String>)>), ParseError> {
    let mut params: Vec<(Rc<String>, Rc<String>)> = vec![];
    let (_, _, data_type, token_value) = parser.l_decl()?;
    params.push((data_type.0.clone(), token_value.0.clone()));
    // parser.expect(",")?;
    // let (response, mut remaining_params) = parser.optparams()?;
    let (response, mut remaining_params) = parser.optparams_factor()?;
    params.append(&mut remaining_params);
    Ok((response, params))
}

pub fn function_stmt(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.expect("def")?;
    let (_, _, token_value) = parser.expect_and_get_value("identifier")?;
    parser.expect("(")?;
    let (_, params) = parser.optparams()?;  // take individual l_decl info from this parsing step for semantic analysis
    parser.expect(")")?;
    let curr_lookahead = parser.get_lookahead();
    let (is_matched, (response, return_type), err) = 
    PackratParser::expect_optionally(|| {
        let (_, _) = parser.expect("->")?;
        let (response, _, data_type) = parser.expect_type()?;
        Ok((response, Some(data_type)))
    }, (ParseSuccess{
        lookahead: curr_lookahead,
        possible_err: None,
    }, None));
    parser.reset_lookahead(response.lookahead);
    match parser.expect(":") {
        Ok((_, _)) => {},
        Err(error) => {
            if let Some(possible_err) = err {
                return Err(possible_err)
            } else {
                return Err(error)
            }
        }
    }
    let response = parser.block()?;
    if is_matched {
        if let Some(return_type) = return_type {
            parser.set_function_to_scope(&token_value, params, Some(return_type.0.clone()))
        } else {
            unreachable!("if optional pattern is matched will give some return type")
        }
    } else {
        parser.set_function_to_scope(&token_value, params, None)
    }
    Ok(response)
}