use std::rc::Rc;
use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::ParseError;
use crate::lexer::token::{CoreToken, TokenValue};
use crate::errors::SyntaxError;

pub fn struct_stmt(parser: &mut PackratParser, name: &Rc<String>) -> Result<ParseSuccess, ParseError> {
    // parser.expect("type")?;
    // let (_, _, token_value) = parser.expect_any_id()?;
    // parser.expect(":")?;
    let (response, fields_map) = parser.struct_block()?;
    parser.set_user_defined_struct_type_to_scope(name, &Rc::new(fields_map));
    Ok(response)
}

pub fn lambda_stmt(parser: &mut PackratParser, name: &Rc<String>) -> Result<ParseSuccess, ParseError> {
    let (_, params, 
        _, return_type, err) = parser.function_input_output()?;
    match parser.expect("\n") {
        Ok((_, _)) => {},
        Err(error) => {
            if let Some(possible_err) = err {
                return Err(possible_err)
            } else {
                return Err(error)
            }
        }
    }
    parser.set_user_defined_lambda_type(name, &Rc::new(params), &Rc::new(return_type));
    Ok(ParseSuccess{
        lookahead: parser.get_lookahead(),
        possible_err: None,
    })
}

pub fn type_decl_stmt(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.expect("type")?;
    let (_, _, token_value) = parser.expect_any_id()?;
    parser.expect(":")?;
    match parser.get_curr_core_token() {
        CoreToken::NEWLINE => {
            return parser.struct_stmt(&token_value.0)
        },
        CoreToken::LPAREN => {
            return parser.lambda_stmt(&token_value.0)
        },
        _ => {
            let line_number = parser.get_curr_line_number();
            let index = parser.get_index();
            let err = ParseError::SYNTAX_ERROR(SyntaxError::new(
                parser.get_code_line(line_number, index),
                format!(
                "expected 'newline' or '(', got '{}'", PackratParser::parse_for_err_message(
                    parser.get_next_token_name().to_string())
                )
            ));
            return Err(err)
        }
    }
}