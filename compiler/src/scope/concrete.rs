use std::collections::hash_map::Entry;
use super::core::GenericContainingConstructs;
use super::core::GenericTypeParams;
use crate::scope::core::AbstractConcreteTypesHandler;
use crate::scope::core::SymbolData;
use crate::types::core::Type;
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, Copy)]
pub struct ConcreteTypesRegistryKey(usize);

#[derive(Debug)]
pub struct ConcreteSymbolData<T: AbstractConcreteTypesHandler + GenericContainingConstructs> {
    pub symbol_data: SymbolData<T>,
    pub index: Option<ConcreteTypesRegistryKey>, // This will be `None` for symbol data which does not have any generic type params
}

#[derive(Debug)]
pub struct GenericsSpecAndConcreteTypesRegistry<T: AbstractConcreteTypesHandler + Default> {
    pub generics_spec: GenericTypeParams,
    pub concrete_types_registry: T,
}

impl<T: AbstractConcreteTypesHandler + Default> GenericsSpecAndConcreteTypesRegistry<T> {
    fn new(generics_spec: GenericTypeParams) -> Self {
        GenericsSpecAndConcreteTypesRegistry {
            generics_spec,
            concrete_types_registry: T::default(),
        }
    }
}

#[derive(Debug, Default)]
pub struct StructConcreteTypesRegistry(
    Vec<(Vec<Type>, FxHashMap<String, CallableConcreteTypesRegistry>)>,
);

impl StructConcreteTypesRegistry {
    pub fn register_method_concrete_types_for_key(
        &mut self,
        key: ConcreteTypesRegistryKey,
        method_name: String,
        method_concrete_types: &Vec<Type>,
    ) {
        match self.0[key.0].1.entry(method_name.to_string()) {
            Entry::Occupied(mut occupied_entry) => {
                let occupied_entry_mut_ref = occupied_entry.get_mut();
                occupied_entry_mut_ref.register_concrete_types(method_concrete_types);
            }
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(CallableConcreteTypesRegistry::new_with_entries(vec![
                    method_concrete_types.clone(),
                ]));
            }
        }
    }
}

impl AbstractConcreteTypesHandler for StructConcreteTypesRegistry {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        let index = self.0.len();
        self.0.push((concrete_types.clone(), FxHashMap::default()));
        ConcreteTypesRegistryKey(index)
    }
}

#[derive(Debug, Default, Clone)]
pub struct CallableConcreteTypesRegistry(Vec<Vec<Type>>);

impl CallableConcreteTypesRegistry {
    pub fn new_with_entries(entries: Vec<Vec<Type>>) -> Self {
        CallableConcreteTypesRegistry(entries)
    }
}

impl AbstractConcreteTypesHandler for CallableConcreteTypesRegistry {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        let index = self.0.len();
        self.0.push(concrete_types.clone());
        ConcreteTypesRegistryKey(index)
    }
}

/*
#[derive(Debug, Clone)]
pub struct ConcreteTypes {
    types: Vec<Type>,
    hash_str: String,
}

impl ConcreteTypes {
    fn new(types: &Vec<Type>) -> Self {
        ConcreteTypes {
            types: types.clone(), // expensive clone
            hash_str: ConcreteTypes::stringify_types(types),
        }
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
 */
