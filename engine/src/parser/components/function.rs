use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::lexer::token::CoreToken;
use crate::errors::{ParseError, SyntaxError, aggregate_errors};
use std::rc::Rc;

pub fn param(parser: &mut PackratParser) -> Result<(ParseSuccess, Rc<String>), ParseError> {
    let mut errors_vec: Vec<ParseError> = vec![];
    match parser.expr() {
        Ok((response, has_float)) => {
            if has_float {
                return Ok((response, Rc::new(String::from("float"))))
            } else {
                return Ok((response, Rc::new(String::from("int"))))
            }
        },
        Err(err) => errors_vec.push(err)
    }
    match parser.bexpr() {
        Ok(response) => {
            return Ok((response, Rc::new(String::from("bool"))))
        },
        Err(err) => errors_vec.push(err)
    }
    match parser.expect("literal") {
        Ok((response, _)) => return Ok((response, Rc::new(String::from("string")))),
        Err(err) => errors_vec.push(err)
    }
    match parser.atom() {
        Ok((response, data_type)) => {
            if let Some(data_type) = data_type {
                return Ok((response, data_type))
            } else {
                let line_number = parser.get_curr_line_number();
                return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                    line_number, 
                    parser.get_code_line(line_number),
                    parser.get_index(), 
                    String::from("argument with type 'None' found")))
                )
            }
        },
        Err(err) => errors_vec.push(err)
    }
    Err(aggregate_errors(errors_vec))
}

pub fn params(parser: &mut PackratParser) -> Result<(ParseSuccess, usize, Vec<Rc<String>>), ParseError> {
    let mut params_data_type_vec: Vec<Rc<String>> = vec![];
    match parser.get_curr_core_token() {
        CoreToken::RPAREN => {
            // let (response, line_number) = parser.expect(")")?;
            return Ok((ParseSuccess{
                lookahead: parser.get_lookahead(),
                possible_err: None,
            }, parser.get_curr_line_number(), params_data_type_vec))
        },
        _ => {}
    }
    let (response, param_data_type) = parser.param()?;
    let line_number = parser.get_curr_line_number();
    params_data_type_vec.push(param_data_type);
    let token_name = parser.get_curr_token_name();
    match parser.get_curr_core_token() {
        CoreToken::RPAREN => {
            Ok((response, line_number, params_data_type_vec))
        },
        CoreToken::COMMA => {
            parser.expect(",")?;
            let (response, line_number, mut remaining_params_data_type_vec) = parser.params()?;
            params_data_type_vec.append(&mut remaining_params_data_type_vec);
            Ok((response, line_number, params_data_type_vec))
        }
        _ => {
            return Err(ParseError::SYNTAX_ERROR(SyntaxError::new(
                line_number, 
                parser.get_code_line(line_number),
                parser.get_index(), 
                format!("expected ',' or ')', got '{}'", token_name)))
            )
        }
    }
}