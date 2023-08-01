use super::{core::GenericTypeParams, errors::GenericTypeArgsCheckError};
use crate::types::core::Type;

pub fn check_concrete_types_bounded_by(
    generic_type_decls: &Option<GenericTypeParams>,
    concrete_types: &Option<Vec<Type>>,
) -> Result<(), GenericTypeArgsCheckError> {
    match concrete_types {
        Some(concrete_types) => match generic_type_decls {
            Some(generic_type_decls) => {
                return generic_type_decls.check_concrete_types_bounded_by(concrete_types)
            }
            None => return Err(GenericTypeArgsCheckError::GenericTypeArgsNotExpected),
        },
        None => match generic_type_decls {
            Some(_) => return Err(GenericTypeArgsCheckError::GenericTypeArgsExpected),
            None => return Ok(()),
        },
    }
}
