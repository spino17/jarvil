use super::core::AbstractMethodConcreteTypesHandler;
use super::core::GenericContainingConstructs;
use super::core::GenericTypeParams;
use super::types::core::UserDefinedTypeData;
use crate::scope::core::AbstractConcreteTypesHandler;
use crate::scope::core::SymbolData;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;

#[derive(Debug, Clone, Copy)]
pub struct ConcreteTypesRegistryKey(usize);

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
pub struct StructConcreteTypesRegistry(
    Vec<(Vec<Type>, FxHashMap<String, CallableConcreteTypesRegistry>)>,
);

impl StructConcreteTypesRegistry {}

impl AbstractConcreteTypesHandler for StructConcreteTypesRegistry {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        let index = self.0.len();
        self.0.push((concrete_types.clone(), FxHashMap::default()));
        ConcreteTypesRegistryKey(index)
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        self.0[key.0].0.clone()
    }
}

impl AbstractMethodConcreteTypesHandler for StructConcreteTypesRegistry {
    fn register_method_concrete_types_for_key(
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

#[derive(Debug, Default)]
pub struct InterfaceConcreteTypesRegistry(
    Vec<(
        Vec<Type>,
        Vec<SymbolData<UserDefinedTypeData>>,
        FxHashMap<String, CallableConcreteTypesRegistry>,
    )>,
); // [(interface concrete types, structs implementing the interface, method name to method concrete types mapping)]

impl InterfaceConcreteTypesRegistry {
    pub fn register_struct_for_key(
        &mut self,
        key: ConcreteTypesRegistryKey,
        struct_symbol_data: &SymbolData<UserDefinedTypeData>,
    ) {
        self.0[key.0].1.push(struct_symbol_data.clone());
    }
}

impl AbstractConcreteTypesHandler for InterfaceConcreteTypesRegistry {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        let index = self.0.len();
        self.0
            .push((concrete_types.clone(), Vec::default(), FxHashMap::default()));
        ConcreteTypesRegistryKey(index)
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        self.0[key.0].0.clone()
    }
}

impl AbstractMethodConcreteTypesHandler for InterfaceConcreteTypesRegistry {
    fn register_method_concrete_types_for_key(
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