use crate::parser::packrat::ParseSuccess;
use crate::errors::{ParseError, SyntaxError, SemanticError};
use std::rc::Rc;

pub fn clone_error(err: &ParseError) -> ParseError {
    match err {
        ParseError::SYNTAX_ERROR(syntax_error) => {
            ParseError::SYNTAX_ERROR(SyntaxError{
                code_line: (syntax_error.code_line.0.clone(), syntax_error.code_line.1, syntax_error.code_line.2, syntax_error.code_line.3),
                err_message: syntax_error.err_message.clone(),
            })
        },
        ParseError::SEMANTIC_ERROR(semantic_error) => {
            ParseError::SEMANTIC_ERROR(SemanticError{
                code_line: (semantic_error.code_line.0.clone(), semantic_error.code_line.1, semantic_error.code_line.2, semantic_error.code_line.3),
                err_message: semantic_error.err_message.clone(),
            })
        }
    }
}

pub fn clone_atom_result(result: &Result<(ParseSuccess, Option<Rc<String>>, bool, bool), ParseError>) 
-> Result<(ParseSuccess, Option<Rc<String>>, bool, bool), ParseError> {
    match result {
        Ok(response) => {
            let possible_err = match &response.0.possible_err {
                Some(val) => Some(clone_error(val)),
                None => None,
            };
            let parse_success = ParseSuccess{
                lookahead: response.0.lookahead,
                possible_err,
            };
            let data_type = match &response.1 {
                Some(data_type) => Some(data_type.clone()),
                None => None,
            };
            Ok((parse_success, data_type, response.2, response.3))
        },
        Err(err) => {
            Err(clone_error(err))
        }
    }
}

pub fn clone_expr_result(result: &Result<(ParseSuccess, bool), ParseError>) 
-> Result<(ParseSuccess, bool), ParseError> {
    match result {
        Ok(response) => {
            let possible_err = match &response.0.possible_err {
                Some(val) => Some(clone_error(val)),
                None => None,
            };
            let parse_success = ParseSuccess{
                lookahead: response.0.lookahead,
                possible_err,
            };
            Ok((parse_success, response.1))
        },
        Err(err) => {
            Err(clone_error(err))
        }
    }
}