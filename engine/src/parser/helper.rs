use crate::parser::parser::ParseSuccess;
use crate::errors::{SyntaxError};
use crate::types::core::Type;

pub fn clone_atom_result(result: &Result<(ParseSuccess, Option<Type>, bool, bool), SyntaxError>) 
-> Result<(ParseSuccess, Option<Type>, bool, bool), SyntaxError> {
    match result {
        Ok(response) => {
            let possible_err = match &response.0.possible_err {
                Some(err) => Some(err.clone()),
                None => None,
            };
            let parse_success = ParseSuccess{
                lookahead: response.0.lookahead,
                possible_err,
            };
            let data_type = match &response.1 {
                Some(data_type) => Some(Type(data_type.0.clone())),
                None => None,
            };
            Ok((parse_success, data_type, response.2, response.3))
        },
        Err(err) => {
            Err(err.clone())
        }
    }
}

pub fn clone_expr_result(result: &Result<(ParseSuccess, bool), SyntaxError>) 
-> Result<(ParseSuccess, bool), SyntaxError> {
    match result {
        Ok(response) => {
            let possible_err = match &response.0.possible_err {
                Some(err) => Some(err.clone()),
                None => None,
            };
            let parse_success = ParseSuccess{
                lookahead: response.0.lookahead,
                possible_err,
            };
            Ok((parse_success, response.1))
        },
        Err(err) => {
            Err(err.clone())
        }
    }
}