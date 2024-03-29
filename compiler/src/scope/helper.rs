use super::namespace::Namespace;
use super::scope::ScopeIndex;
use super::symbol::{
    interfaces::InterfaceBounds,
    types::{
        core::UserDefinedTypeData,
        generic_ty::{GenericTypeData, GenericTypeParams},
    },
};
use super::{concrete::TurbofishTypes, errors::GenericTypeArgsCheckError};
use crate::core::string_interner::Interner;
use crate::scope::symbol::types::generic_ty::GenericTypeDeclarationPlaceCategory;
use crate::types::core::TypeStringifyContext;
use text_size::TextRange;

pub fn check_concrete_types_bounded_by_interfaces(
    generic_ty_decls: Option<&GenericTypeParams>,
    concrete_types: Option<&TurbofishTypes>,
    ty_ranges: Option<&Vec<TextRange>>,
    is_concrete_types_none_allowed: bool,
    context: TypeStringifyContext,
) -> Result<(), GenericTypeArgsCheckError> {
    match concrete_types {
        Some(concrete_types) => match generic_ty_decls {
            Some(generic_ty_decls) => generic_ty_decls.check_concrete_types_bounded_by(
                concrete_types,
                match ty_ranges {
                    Some(ty_ranges) => ty_ranges,
                    None => unreachable!(),
                },
                context,
            ),
            None => Err(GenericTypeArgsCheckError::GenericTypeArgsNotExpected),
        },
        None => match generic_ty_decls {
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

pub fn fill_side_scope_with_generic_types(namespace: &mut Namespace, interner: &Interner) {
    // InStruct generic types
    namespace.types_mut_ref()[ScopeIndex::side()].set(
        interner.intern("T"),
        UserDefinedTypeData::Generic(GenericTypeData::new(
            0,
            GenericTypeDeclarationPlaceCategory::InType,
            InterfaceBounds::default(),
        )),
        TextRange::default(),
        None,
    );
    namespace.types_mut_ref()[ScopeIndex::side()].set(
        interner.intern("U"),
        UserDefinedTypeData::Generic(GenericTypeData::new(
            1,
            GenericTypeDeclarationPlaceCategory::InType,
            InterfaceBounds::default(),
        )),
        TextRange::default(),
        None,
    );

    // InCallable generic types
    namespace.types_mut_ref()[ScopeIndex::side()].set(
        interner.intern("V"),
        UserDefinedTypeData::Generic(GenericTypeData::new(
            0,
            GenericTypeDeclarationPlaceCategory::InCallable,
            InterfaceBounds::default(),
        )),
        TextRange::default(),
        None,
    );
}
