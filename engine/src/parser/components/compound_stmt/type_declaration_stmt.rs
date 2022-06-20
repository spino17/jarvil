use std::rc::Rc;
use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::ParseError;
use crate::lexer::token::{CoreToken, TokenValue};
use crate::errors::SyntaxError;

pub fn struct_stmt(parser: &mut PackratParser, token_value: &TokenValue) -> Result<ParseSuccess, ParseError> {
    // parser.expect("type")?;
    // let (_, _, token_value) = parser.expect_any_id()?;
    // parser.expect(":")?;
    let (response, fields_map) = parser.struct_block()?;
    parser.set_user_defined_struct_type_to_scope(token_value, &Rc::new(fields_map));
    Ok(response)
}

pub fn lambda_stmt(parser: &mut PackratParser, token_value: &TokenValue) -> Result<ParseSuccess, ParseError> {
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
    parser.set_user_defined_lambda_type(token_value, &Rc::new(params), &Rc::new(return_type));
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
            return parser.struct_stmt(&token_value)
        },
        CoreToken::LPAREN => {
            return parser.lambda_stmt(&token_value)
        },
        _ => {
            let line_number = parser.get_curr_line_number();
            let err = ParseError::SYNTAX_ERROR(SyntaxError::new(
                line_number,
                parser.get_code_line(line_number),
                parser.get_index(),
                format!(
                "expected a 'newline' or '(', got '{}'", PackratParser::parse_for_err_message(
                    parser.get_next_token_name().to_string())
                )
            ));
            return Err(err)
        }
    }
}