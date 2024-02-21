use super::core::HashMap;
use crate::core::string_interner::Interner;
use crate::scope::symbol::function::CallableData;
use rustc_hash::FxHashMap;

impl HashMap {
    pub fn builtin_methods(_interner: &Interner) -> FxHashMap<&'static str, CallableData> {
        // hashmap built-in methods

        FxHashMap::default()
    }
}
