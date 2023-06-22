use super::core::ConcreteTypes;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry;

pub struct StructConcreteTypesMap {
    map: FxHashMap<ConcreteTypes, FxHashMap<String, FxHashSet<ConcreteTypes>>>,
}

impl StructConcreteTypesMap {
    fn insert_struct_concrete_types(&mut self, struct_concrete_types: &ConcreteTypes) {
        if !self.map.contains_key(struct_concrete_types) {
            self.map
                .insert(struct_concrete_types.clone(), FxHashMap::default());
        }
    }

    fn insert_method_concrete_types(
        &mut self,
        method_name: &str,
        struct_concrete_types: &ConcreteTypes,
        method_concrete_types: &ConcreteTypes,
    ) {
        match self.map.entry(struct_concrete_types.clone()) {
            Entry::Occupied(mut entry) => {
                let struct_val = entry.get_mut();
                match struct_val.entry(method_name.to_string()) {
                    Entry::Occupied(mut method_entry) => {
                        let method_val = method_entry.get_mut();
                        method_val.insert(method_concrete_types.clone());
                    }
                    Entry::Vacant(method_entry) => {
                        let mut methods_val = FxHashSet::default();
                        methods_val.insert(method_concrete_types.clone());
                        method_entry.insert(methods_val);
                    }
                }
            }
            Entry::Vacant(entry) => {
                let mut methods_val = FxHashSet::default();
                methods_val.insert(method_concrete_types.clone());
                let mut struct_val = FxHashMap::default();
                struct_val.insert(method_name.to_string(), methods_val);
                entry.insert(struct_val);
            }
        }
    }
}

impl Default for StructConcreteTypesMap {
    fn default() -> Self {
        StructConcreteTypesMap {
            map: FxHashMap::default(),
        }
    }
}
