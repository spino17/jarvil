use super::core::HashMap;
use crate::core::string_interner::Interner;
use crate::{
    scope::function::{CallableData, CallableKind},
    types::{core::Type, helper::get_unbounded_generic_type_with_declaration_index},
};
use rustc_hash::FxHashMap;

impl HashMap {
    pub fn get_builtin_methods(interner: &mut Interner) -> FxHashMap<&'static str, CallableData> {
        let mut methods = FxHashMap::default();

        // hashmap built-in methods

        return methods;
    }
}
