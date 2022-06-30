use std::rc::Rc;
use crate::parser::components::code;
use crate::parser::packrat::{PackratParser, ParseSuccess};
use crate::errors::{ParseError, SemanticError};
use crate::lexer::token::{CoreToken};
use crate::errors::SyntaxError;

pub fn struct_stmt(parser: &mut PackratParser, name: &Rc<String>, index: usize) -> Result<ParseSuccess, ParseError> {
    let (response, fields_vec) = parser.struct_block()?;
    if let Some(identifier_category) = parser.set_user_defined_struct_type_to_scope(name, &Rc::new(fields_vec)) {
        let line_number = parser.get_curr_line_number();
        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
            parser.get_code_line(line_number, index),
            format!(
            "'{}' already declared in the current scope as '{}'", name, identifier_category
            )
        )));
    }
    Ok(response)
}

pub fn lambda_stmt(parser: &mut PackratParser, name: &Rc<String>, index: usize) -> Result<ParseSuccess, ParseError> {
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
    if let Some(identifier_category) = parser.set_user_defined_lambda_type(name, &Rc::new(params), &Rc::new(return_type)) {
        let line_number = parser.get_curr_line_number();
        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
            parser.get_code_line(line_number, index),
            format!(
            "'{}' already declared in the current scope as '{}'", name, identifier_category
            )
        )));
    }
    Ok(ParseSuccess{
        lookahead: parser.get_lookahead(),
        possible_err: None,
    })
}

pub fn type_decl_stmt(parser: &mut PackratParser) -> Result<ParseSuccess, ParseError> {
    parser.expect("type")?;
    let index = parser.get_index();
    let (_, _, type_name) = parser.expect_any_id()?;
    parser.expect(":")?;
    match parser.get_curr_core_token() {
        CoreToken::NEWLINE => {
            return parser.struct_stmt(&type_name, index)
        },
        CoreToken::LPAREN => {
            return parser.lambda_stmt(&type_name, index)
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