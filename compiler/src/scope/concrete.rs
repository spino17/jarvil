use super::core::GenericContainingConstructs;
use super::core::GenericTypeParams;
use crate::scope::core::AbstractConcreteTypesHandler;
use crate::scope::core::SymbolData;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;

#[derive(Debug, Clone, Copy)]
pub struct ConcreteTypesRegistryKey(usize);

#[derive(Debug)]
pub struct ConcreteTypesTuple {
    concrete_types: Vec<Type>, // this can contain generic type also
    completely_concrete_types: Option<Vec<Type>>, // it cannot contain generic type -> this is used for code-generation
    generics_containing_indexes: Vec<usize>,      // these indexes needs concretization
}

impl ConcreteTypesTuple {
    fn new(concrete_types: &Vec<Type>, generics_containing_indexes: Vec<usize>) -> Self {
        ConcreteTypesTuple {
            concrete_types: concrete_types.clone(),
            completely_concrete_types: if generics_containing_indexes.len() > 0 {
                None
            } else {
                Some(concrete_types.clone())
            },
            generics_containing_indexes,
        }
    }

    fn is_containing_generics(&self) -> bool {
        if self.generics_containing_indexes.len() > 0 {
            true
        } else {
            false
        }
    }

    pub fn concretizes(&mut self) {
        // either the concrete_types is already concretized or does not have any generic types to concretize!
        if self.completely_concrete_types.is_some() {
            return;
        }
        let mut concretized_vec: Vec<Type> = vec![];
        let concrete_types_len = self.concrete_types.len();
        let mut start_index = 0;
        for &index in &self.generics_containing_indexes {
            for j in start_index..index {
                concretized_vec.push(self.concrete_types[j].clone());
            }
            let mut expanded_vec = self.concrete_types[index].concretize();
            concretized_vec.append(&mut expanded_vec);
            start_index = index + 1;
        }
        if start_index < concrete_types_len {
            for i in start_index..concrete_types_len {
                concretized_vec.push(self.concrete_types[i].clone());
            }
        }
        let len = concretized_vec.len();
        for i in 0..len {
            assert!(!concretized_vec[i].has_generics());
        }
        self.concrete_types = concretized_vec;
    }
}

#[derive(Debug)]
pub struct ConcreteSymbolData<T: AbstractConcreteTypesHandler + GenericContainingConstructs> {
    pub symbol_data: SymbolData<T>,
    pub index: Option<ConcreteTypesRegistryKey>, // This will be `None` for symbol data which does not have any generic type params
}

impl<T: AbstractConcreteTypesHandler + GenericContainingConstructs> Clone
    for ConcreteSymbolData<T>
{
    fn clone(&self) -> Self {
        ConcreteSymbolData {
            symbol_data: self.symbol_data.clone(),
            index: self.index,
        }
    }
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
pub struct StructConcreteTypesRegistry<T: Default>(
    Vec<(
        Vec<Type>,
        T,
        FxHashMap<String, CallableConcreteTypesRegistry>,
    )>,
);

impl<T: Default> StructConcreteTypesRegistry<T> {
    pub fn register_method_concrete_types_for_key(
        &mut self,
        key: ConcreteTypesRegistryKey,
        method_name: String,
        method_concrete_types: &Vec<Type>,
    ) {
        match self.0[key.0].2.entry(method_name.to_string()) {
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

impl<T: Default> AbstractConcreteTypesHandler for StructConcreteTypesRegistry<T> {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        let index = self.0.len();
        self.0
            .push((concrete_types.clone(), T::default(), FxHashMap::default()));
        ConcreteTypesRegistryKey(index)
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        self.0[key.0].0.clone()
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

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        self.0[key.0].clone()
    }
}
