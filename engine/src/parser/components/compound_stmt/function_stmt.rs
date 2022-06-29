use std::rc::Rc;
use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::ParseError;
use crate::lexer::token::{CoreToken};
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
                        let line_number = parser.get_curr_line_number();
                        let index = parser.get_index();
                        let err = ParseError::SYNTAX_ERROR(SyntaxError::new(
                            parser.get_code_line(line_number, index),
                            format!(
                            "expected ',' or ')', got '{}'", PackratParser::parse_for_err_message(
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
    let (_, _, data_type, token_value) = parser.param_decl()?;
    params.push((token_value.clone(), data_type.clone()));
    let (response, mut remaining_params) = parser.optparams_factor()?;
    params.append(&mut remaining_params);
    Ok((response, params))
}

pub fn function_input_output(parser: &mut PackratParser)
-> Result<(ParseSuccess, Vec<(Rc<String>, Rc<String>)>, bool, Option<Rc<String>>, Option<ParseError>), ParseError> {
    // TODO - check for any generic symbols inside '<' '>'
    parser.expect("(")?;
    let mut params = vec![];
    if !parser.check_next_token(")") {
        let (_, opt_params) = parser.optparams()?;
        params = opt_params;
    }
    parser.expect(")")?;
    let curr_lookahead = parser.get_lookahead();
    let (is_matched, (response, return_type), err) = 
    PackratParser::expect_optionally(|| {
        let (_, _) = parser.expect("->")?;
        let (response, _, data_type, _) = parser.expect_type()?;
        Ok((response, Some(data_type.clone())))
    }, (ParseSuccess{
        lookahead: curr_lookahead,
        possible_err: None,
    }, None));
    parser.reset_lookahead(response.lookahead);
    Ok((response, params, is_matched, return_type, err))
}

pub fn function_declaration(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.expect("def")?;
    match parser.get_curr_core_token() {
        CoreToken::IDENTIFIER(_) => {
            let (_, _, token_value) = parser.expect_any_id()?;
            let (_, params, 
                _, return_type, err) = parser.function_input_output()?;
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
            let response = parser.block(Some(&params))?;
            parser.set_function_to_scope(&token_value, &Rc::new(params), &Rc::new(return_type));
            Ok(response)
        },
        _ => {
            let line_number = parser.get_curr_line_number();
            let index = parser.get_index();
            Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
            parser.get_code_line(line_number, index),
            format!("expected '(' or an identifier, got '{}'",
            PackratParser::parse_for_err_message(parser.get_curr_token_name().to_string())))))
        }
    }
}