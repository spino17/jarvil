use std::hash::{Hash, Hasher};

use rustc_hash::FxHashMap;
use crate::types::core::{Type, AbstractType};

pub struct ConcreteTypesMap {
    map: FxHashMap<ConcreteTypes, ()>
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

    fn insert(&mut self, types: &ConcreteTypes) {
        if !self.map.contains_key(types) {
            self.map.insert(types.clone(), ());
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConcreteTypes {
    types: Vec<Type>,
    hash_str: String
}

impl ConcreteTypes {
    fn new(types: &Vec<Type>) -> Self {
        ConcreteTypes {
            types: types.clone(),  // expensive clone
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