use crate::{
    scope::{
        core::SymbolData,
        function::{CallableData, CallableKind},
        types::{
            core::UserDefinedTypeData,
            generic_type::{GenericTypeData, GenericTypeDeclarationPlaceCategory},
        },
    },
    types::core::Type,
};
use std::collections::HashMap;
use text_size::TextRange;

thread_local!(
    pub static ARRAY_BUILTIN_METHODS: &'static HashMap<&'static str, CallableData> =
        Box::leak(Box::new(HashMap::from([
            ("append", CallableData::new(vec![Type::new_with_generic(&SymbolData::new(UserDefinedTypeData::Generic(GenericTypeData {
                category: GenericTypeDeclarationPlaceCategory::InStruct,
                index: 0,
                interface_bounds: vec![]
            }), TextRange::default(), true))], Type::new_with_void(), CallableKind::Method, true, None)),
        ])))
);
