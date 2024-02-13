use super::namespace::Namespace;
use super::{concrete::ConcreteTypesTuple, errors::GenericTypeArgsCheckError};
use crate::core::string_interner::Interner;
use crate::scope::symbol::types::generic_type::GenericTypeParams;
use text_size::TextRange;

pub fn check_concrete_types_bounded_by_interfaces(
    generic_type_decls: &Option<GenericTypeParams>,
    concrete_types: &Option<ConcreteTypesTuple>,
    type_ranges: &Option<Vec<TextRange>>,
    is_concrete_types_none_allowed: bool,
    interner: &Interner,
    namespace: &Namespace,
) -> Result<(), GenericTypeArgsCheckError> {
    match concrete_types {
        Some(concrete_types) => match generic_type_decls {
            Some(generic_type_decls) => generic_type_decls.check_concrete_types_bounded_by(
                concrete_types,
                match type_ranges {
                    Some(type_ranges) => type_ranges,
                    None => unreachable!(),
                },
                interner,
                namespace,
            ),
            None => Err(GenericTypeArgsCheckError::GenericTypeArgsNotExpected),
        },
        None => match generic_type_decls {
            Some(_) => {
                if is_concrete_types_none_allowed {
                    Ok(())
                } else {
                    Err(GenericTypeArgsCheckError::GenericTypeArgsExpected)
                }
            }
            None => Ok(()),
        },
    }
}
