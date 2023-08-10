use super::core::Type;
use crate::{
    parser::type_checker::InferredConcreteTypesEntry,
    scope::{
        core::SymbolData,
        interfaces::InterfaceBounds,
        types::{
            core::UserDefinedTypeData,
            generic_type::{GenericTypeData, GenericTypeDeclarationPlaceCategory},
        },
    },
    types::core::AbstractType,
};
use text_size::TextRange;

pub fn get_unbounded_generic_type_with_declaration_index(index: usize) -> Type {
    Type::new_with_generic(
        "T".to_string(),
        &SymbolData::new(
            UserDefinedTypeData::Generic(GenericTypeData {
                category: GenericTypeDeclarationPlaceCategory::InStruct,
                index,
                interface_bounds: InterfaceBounds::new(vec![]),
            }),
            TextRange::default(),
            true,
        ),
    )
}

pub fn try_infer_types_from_tuple(
    base_types_tuple: &Vec<Type>,
    generics_containing_types_tuple: &Vec<Type>,
    inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
    num_inferred_types: &mut usize,
    generic_ty_decl_place: GenericTypeDeclarationPlaceCategory,
) -> Result<(), ()> {
    if base_types_tuple.len() != generics_containing_types_tuple.len() {
        return Err(());
    }
    for (index, generics_containing_ty) in generics_containing_types_tuple.iter().enumerate() {
        let base_ty = &base_types_tuple[index];
        if generics_containing_ty.has_generics() {
            let _ = base_ty.try_infer_type(
                generics_containing_ty,
                inferred_concrete_types,
                num_inferred_types,
                generic_ty_decl_place,
            )?;
        } else {
            if !base_ty.is_eq(generics_containing_ty) {
                return Err(());
            }
        }
    }
    Ok(())
}
