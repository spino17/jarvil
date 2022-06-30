use std::rc::Rc;
use crate::errors::{ParseError, SemanticError};
use crate::parser::packrat::PackratParser;
use crate::types::{Type, TypeCheck};

pub fn function_params_semantic_check(
    parser: &mut PackratParser, curr_params: &Vec<(Type, usize)>, expected_params: &Rc<Vec<(Rc<String>, Type)>>,
    line_number: usize,
) -> Result<(), ParseError>{
    let curr_params_len = curr_params.len();
    let expected_params_len = expected_params.len();
    if curr_params_len != expected_params_len {
        let index = parser.get_index();
        return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
            parser.get_code_line(line_number, index),
            format!("expected '{}' number of arguments to the function, got '{}'", 
            expected_params_len, curr_params_len)))
        )
    }
    for i in 0..curr_params_len {
        let curr_data_type = &curr_params[i].0;
        let expected_data_type = &expected_params[i].1;
        if !curr_data_type.is_eq(expected_data_type) {
            let param_index = curr_params[i].1;
            return Err(ParseError::SEMANTIC_ERROR(SemanticError::new(
                parser.get_code_line(line_number, param_index),
                format!("expected type '{}' for argument '{}', got '{}'",
                expected_data_type, i, curr_data_type)))
            )
        }
    }
    Ok(())
}