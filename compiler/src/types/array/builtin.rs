use crate::core::string_interner::Interner;
use crate::types::array::core::Array;
use crate::{
    scope::symbol::function::{CallableData, CallableKind},
    types::{core::Type, helper::unbounded_generic_ty_in_type_with_declaration_index},
};
use rustc_hash::FxHashMap;

// append(x: T), where `T` is the element type of the array
fn append_callable_data(interner: &Interner) -> CallableData {
    CallableData::new(
        vec![unbounded_generic_ty_in_type_with_declaration_index(
            0, interner,
        )],
        Type::new_with_void(),
        CallableKind::Method,
        None,
    )
}

impl Array {
    pub fn builtin_methods(interner: &Interner) -> FxHashMap<&'static str, CallableData> {
        let mut methods = FxHashMap::default();

        // array built-in methods
        methods.insert("append", append_callable_data(interner));

        methods
    }
}
