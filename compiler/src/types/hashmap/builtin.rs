use super::core::HashMap;
use crate::core::string_interner::Interner;
use crate::{
    scope::function::{CallableData},
};
use rustc_hash::FxHashMap;

impl HashMap {
    pub fn get_builtin_methods(_interner: &mut Interner) -> FxHashMap<&'static str, CallableData> {
        

        // hashmap built-in methods

        FxHashMap::default()
    }
}
