use super::core::GenericContainingConstructs;
use super::core::GenericTypeParams;
use crate::scope::core::AbstractConcreteTypesHandler;
use crate::scope::core::SymbolData;
use crate::types::core::AbstractType;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;

#[derive(Debug, Clone, Copy)]
pub struct ConcreteTypesRegistryKey(usize);

#[derive(Debug)]
pub struct ConcreteTypesTuple {
    pub concrete_types: Vec<Type>, // this can contain generic type also
    pub generics_containing_indexes: Vec<usize>, // these indexes needs concretization
    pub is_concretized: bool,
}

impl ConcreteTypesTuple {
    pub fn new(concrete_types: Vec<Type>, generics_containing_indexes: Vec<usize>) -> Self {
        ConcreteTypesTuple {
            concrete_types: concrete_types.clone(),
            generics_containing_indexes,
            is_concretized: false,
        }
    }

    pub fn is_containing_generics(&self) -> bool {
        if self.generics_containing_indexes.len() > 0 {
            true
        } else {
            false
        }
    }

    pub fn is_concretization_required(&self) -> bool {
        // either there are no generic types or is already concretized
        self.generics_containing_indexes.len() == 0 || self.is_concretized
    }

    fn get_all_concrete_types_combination(&self, index: usize) -> Vec<Vec<Type>> {
        if index == self.generics_containing_indexes.len() - 1 {
            let mut v = vec![];
            for ty in &self.concrete_types[self.generics_containing_indexes[index]].concretize() {
                v.push(vec![ty.clone()])
            }
            return v;
        }
        let first_generic_concrete_type =
            self.concrete_types[self.generics_containing_indexes[index]].concretize();
        let mut remaining_concrete_types_combination =
            self.get_all_concrete_types_combination(index + 1);
        let mut all_concrete_types_combination = vec![];
        for ty in &first_generic_concrete_type {
            for ty_tuple in &mut remaining_concrete_types_combination {
                let mut v = vec![ty.clone()];
                v.append(ty_tuple);
                all_concrete_types_combination.push(v);
            }
        }
        return all_concrete_types_combination;
    }

    pub fn concretize(&mut self) -> Vec<Vec<Type>> {
        if !self.is_concretization_required() {
            unreachable!() // don't call this function which does not require concretization
        }
        let all_concrete_types_combination = self.get_all_concrete_types_combination(0);
        let mut result = vec![];
        let generics_containing_indexes_len = self.generics_containing_indexes.len();
        let concrete_types_len = self.concrete_types.len();
        for ty_combination in all_concrete_types_combination {
            let mut v = vec![];
            let mut start_index = 0;
            for index in 0..generics_containing_indexes_len {
                let critical_index = self.generics_containing_indexes[index];
                let concrete_type = &ty_combination[index];
                for i in start_index..critical_index {
                    v.push(self.concrete_types[i].clone());
                }
                v.push(concrete_type.clone());
                start_index = critical_index + 1;
            }
            if start_index < self.concrete_types.len() {
                for i in start_index..concrete_types_len {
                    v.push(self.concrete_types[i].clone());
                }
            }
            result.push(v);
        }
        self.is_concretized = true;
        return result;
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
        ConcreteTypesTuple,
        T,
        FxHashMap<String, CallableConcreteTypesRegistry>,
    )>,
);

impl<T: Default> StructConcreteTypesRegistry<T> {
    pub fn register_method_concrete_types_for_key(
        &mut self,
        key: ConcreteTypesRegistryKey,
        method_name: String,
        method_concrete_types: Vec<Type>,
        method_containing_generics_indexes: Vec<usize>,
    ) {
        match self.0[key.0].2.entry(method_name.to_string()) {
            Entry::Occupied(mut occupied_entry) => {
                let occupied_entry_mut_ref = occupied_entry.get_mut();
                occupied_entry_mut_ref.register_concrete_types(
                    method_concrete_types,
                    method_containing_generics_indexes,
                );
            }
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(CallableConcreteTypesRegistry::new_with_entries(vec![
                    ConcreteTypesTuple::new(
                        method_concrete_types,
                        method_containing_generics_indexes,
                    ),
                ]));
            }
        }
    }
}

impl<T: Default> AbstractConcreteTypesHandler for StructConcreteTypesRegistry<T> {
    fn register_concrete_types(
        &mut self,
        concrete_types: Vec<Type>,
        generics_containing_indexes: Vec<usize>,
    ) -> ConcreteTypesRegistryKey {
        let index = self.0.len();
        self.0.push((
            ConcreteTypesTuple::new(concrete_types, generics_containing_indexes),
            T::default(),
            FxHashMap::default(),
        ));
        ConcreteTypesRegistryKey(index)
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        self.0[key.0].0.concrete_types.clone()
    }
}

#[derive(Debug, Default)]
pub struct CallableConcreteTypesRegistry(Vec<ConcreteTypesTuple>);

impl CallableConcreteTypesRegistry {
    pub fn new_with_entries(entries: Vec<ConcreteTypesTuple>) -> Self {
        CallableConcreteTypesRegistry(entries)
    }
}

impl AbstractConcreteTypesHandler for CallableConcreteTypesRegistry {
    fn register_concrete_types(
        &mut self,
        concrete_types: Vec<Type>,
        generics_containing_indexes: Vec<usize>,
    ) -> ConcreteTypesRegistryKey {
        let index = self.0.len();
        self.0.push(ConcreteTypesTuple::new(
            concrete_types,
            generics_containing_indexes,
        ));
        ConcreteTypesRegistryKey(index)
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        self.0[key.0].concrete_types.clone()
    }
}
