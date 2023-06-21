use std::{
    collections::hash_map::Entry,
    hash::{Hash, Hasher},
};

use crate::types::core::{AbstractType, Type};
use rustc_hash::{FxHashMap, FxHashSet};

pub struct ConcreteTypesMap {
    map: FxHashMap<ConcreteTypes, FxHashMap<String, FxHashSet<ConcreteTypes>>>,
}

impl ConcreteTypesMap {
    fn new() -> Self {
        todo!()
    }

    fn stringify_types(types: &Vec<Type>) -> String {
        let mut s = types[0].stringify();
        let len = types.len();
        for i in 1..len {
            s.push_str("_comma_");
            s.push_str(&types[i].stringify());
        }
        s
    }

    fn insert(
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

#[derive(Debug, Clone)]
pub struct ConcreteTypes {
    types: Vec<Type>,
    hash_str: String,
}

impl ConcreteTypes {
    fn new(types: &Vec<Type>) -> Self {
        ConcreteTypes {
            types: types.clone(), // expensive clone
            hash_str: ConcreteTypesMap::stringify_types(types),
        }
    }
}

impl Hash for ConcreteTypes {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let hash_str = &self.hash_str;
        hash_str.hash(state);
    }
}

impl PartialEq for ConcreteTypes {
    fn eq(&self, other: &Self) -> bool {
        self.hash_str == other.hash_str
    }
}

impl Eq for ConcreteTypes {}
