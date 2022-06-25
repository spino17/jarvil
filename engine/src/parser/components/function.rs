use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::lexer::token::CoreToken;
use crate::errors::{ParseError, SyntaxError};
use std::rc::Rc;

pub fn param(parser: &mut PackratParser) -> Result<(ParseSuccess, usize, Rc<String>), ParseError> {
    todo!()
}

pub fn params(parser: &mut PackratParser) -> Result<(ParseSuccess, Vec<Rc<String>>), ParseError> {
    let mut params_data_type_vec: Vec<Rc<String>> = vec![];
    let (response, line_number, param_data_type) = parser.param()?;
    params_data_type_vec.push(param_data_type);
    let token_name = parser.get_curr_token_name();
    match parser.get_curr_core_token() {
        CoreToken::RPAREN => {
            Ok((response, params_data_type_vec))
        },
        CoreToken::COMMA => {
            parser.expect(",")?;
            let (response, mut remaining_params_data_type_vec) = parser.params()?;
            params_data_type_vec.append(&mut remaining_params_data_type_vec);
            Ok((response, params_data_type_vec))
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