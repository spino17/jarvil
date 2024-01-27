use crate::core::string_interner::Interner;
use crate::types::array::core::Array;
use crate::{
    scope::function::{CallableData, CallableKind},
    types::{core::Type, helper::get_unbounded_generic_type_with_declaration_index},
};
use rustc_hash::FxHashMap;

impl Array {
    pub fn get_builtin_methods(interner: &mut Interner) -> FxHashMap<&'static str, CallableData> {
        let mut methods = FxHashMap::default();

        // array built-in methods
        methods.insert(
            "append",
            CallableData::new(
                vec![get_unbounded_generic_type_with_declaration_index(
                    0, interner,
                )],
                Type::new_with_void(),
                CallableKind::Method,
                Some((vec![0], false)),
                None,
            ),
        );

        return methods;
    }
}
