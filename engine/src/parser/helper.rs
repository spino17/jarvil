use crate::parser::packrat::ParseSuccess;
use crate::errors::ParseError;
use std::rc::Rc;

pub fn clone_error(err: &ParseError) -> ParseError {
    todo!()
}

pub fn clone_atom_result(result: &Result<(ParseSuccess, Option<Rc<String>>, bool), ParseError>) 
-> Result<(ParseSuccess, Option<Rc<String>>, bool), ParseError> {
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
            Ok((parse_success, data_type, response.2))
        },
        Err(err) => {
            Err(clone_error(err))
        }
    }
}