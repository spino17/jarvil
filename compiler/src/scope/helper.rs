use super::{core::GenericTypeParams, errors::GenericTypeArgsCheckError};
use crate::types::core::Type;
use text_size::TextRange;

pub fn check_concrete_types_bounded_by(
    generic_type_decls: &Option<GenericTypeParams>,
    concrete_types: &Option<Vec<Type>>,
    type_ranges: &Option<Vec<TextRange>>,
    is_concrete_types_none_allowed: bool,
) -> Result<(), GenericTypeArgsCheckError> {
    match concrete_types {
        Some(concrete_types) => match generic_type_decls {
            Some(generic_type_decls) => {
                return generic_type_decls.check_concrete_types_bounded_by(
                    concrete_types,
                    match type_ranges {
                        Some(type_ranges) => type_ranges,
                        None => unreachable!(),
                    },
                )
            }
            None => return Err(GenericTypeArgsCheckError::GenericTypeArgsNotExpected),
        },
        None => match generic_type_decls {
            Some(_) => {
                if is_concrete_types_none_allowed {
                    return Ok(());
                } else {
                    return Err(GenericTypeArgsCheckError::GenericTypeArgsExpected);
                }
            }
            None => return Ok(()),
        },
    }
}