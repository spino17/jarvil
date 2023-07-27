use super::core::Type;
use crate::scope::{
    core::SymbolData,
    types::{
        core::UserDefinedTypeData,
        generic_type::{GenericTypeData, GenericTypeDeclarationPlaceCategory},
    },
};
use text_size::TextRange;

pub fn get_unbounded_generic_type_with_declaration_index(index: usize) -> Type {
    Type::new_with_generic(
        "T".to_string(),
        &SymbolData::new(
            UserDefinedTypeData::Generic(GenericTypeData {
                category: GenericTypeDeclarationPlaceCategory::InStruct,
                index,
                interface_bounds: vec![],
            }),
            TextRange::default(),
            true,
        ),
    )
}
