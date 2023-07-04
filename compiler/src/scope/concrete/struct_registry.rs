use super::callable_registry::CallableConcreteTypesRegistry;
use super::core::ConcreteTypesRegisterHandler;
use super::core::ConcreteTypesRegistryKey;
use super::core::ConcreteTypesTuple;
use crate::scope::core::AbstractConcreteTypesHandler;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;

#[derive(Debug)]
pub struct MethodsConcreteTypesRegistry<T: Default + Clone> {
    pub optional_arg: T,
    pub methods_concrete_types_map: FxHashMap<String, CallableConcreteTypesRegistry>,
    pub are_method_tuples_concretized: bool,
}

impl<T: Default + Clone> MethodsConcreteTypesRegistry<T> {
    pub fn register_method_concrete_types(
        &mut self,
        method_name: String,
        method_concrete_types: Vec<Type>,
        method_generics_containing_indexes: Vec<usize>,
    ) {
        match self
            .methods_concrete_types_map
            .entry(method_name.to_string())
        {
            Entry::Occupied(mut occupied_entry) => {
                let occupied_entry_mut_ref = occupied_entry.get_mut();
                occupied_entry_mut_ref.register_concrete_types(
                    method_concrete_types,
                    method_generics_containing_indexes,
                );
            }
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(CallableConcreteTypesRegistry::new_with_entries(vec![
                    ConcreteTypesTuple::new(
                        method_concrete_types,
                        method_generics_containing_indexes,
                    ),
                ]));
            }
        }
    }
}

impl<T: Default + Clone> Default for MethodsConcreteTypesRegistry<T> {
    fn default() -> Self {
        MethodsConcreteTypesRegistry {
            optional_arg: T::default(),
            methods_concrete_types_map: FxHashMap::default(),
            are_method_tuples_concretized: false,
        }
    }
}

#[derive(Debug, Default)]
pub struct StructConcreteTypesRegistry<T: Default + Clone>(
    Vec<(ConcreteTypesTuple, MethodsConcreteTypesRegistry<T>)>,
);

impl<T: Default + Clone> StructConcreteTypesRegistry<T> {
    pub fn register_method_concrete_types_for_key(
        &mut self,
        key: ConcreteTypesRegistryKey,
        method_name: String,
        method_concrete_types: Vec<Type>,
        method_containing_generics_indexes: Vec<usize>,
    ) {
        /*
        match self.0[key.0].1.methods_concrete_types_map.entry(method_name.to_string()) {
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
         */
        self.0[key.0].1.register_method_concrete_types(
            method_name,
            method_concrete_types,
            method_containing_generics_indexes,
        )
    }

    pub fn register_entry(
        &mut self,
        struct_concrete_types_tuple: ConcreteTypesTuple,
        optional_arg: T,
        methods_concrete_types_map: FxHashMap<String, CallableConcreteTypesRegistry>,
    ) -> ConcreteTypesRegistryKey {
        let index = self.0.len();
        self.0.push((
            struct_concrete_types_tuple,
            MethodsConcreteTypesRegistry {
                optional_arg,
                methods_concrete_types_map,
                are_method_tuples_concretized: false,
            },
        ));
        return ConcreteTypesRegistryKey(index);
    }

    fn concretize(&mut self, key: ConcreteTypesRegistryKey) -> Vec<ConcreteTypesRegistryKey> {
        let index = key.0;
        let are_methods_concretized = self.0[index].1.are_method_tuples_concretized;
        if !are_methods_concretized {
            for (_, method_concrete_types_tuple) in &mut self.0[index].1.methods_concrete_types_map
            {
                method_concrete_types_tuple.concretize_all_entries();
            }
            self.0[index].1.are_method_tuples_concretized = true;
        }
        self.concretize_core(key)
    }
}

impl<T: Default + Clone> AbstractConcreteTypesHandler for StructConcreteTypesRegistry<T> {
    fn register_concrete_types(
        &mut self,
        concrete_types: Vec<Type>,
        generics_containing_indexes: Vec<usize>,
    ) -> ConcreteTypesRegistryKey {
        let index = self.0.len();
        self.0.push((
            ConcreteTypesTuple::new(concrete_types, generics_containing_indexes),
            MethodsConcreteTypesRegistry {
                optional_arg: T::default(),
                methods_concrete_types_map: FxHashMap::default(),
                are_method_tuples_concretized: false,
            },
        ));
        ConcreteTypesRegistryKey(index)
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        self.0[key.0].0.get_concrete_types()
    }
}

impl<T: Default + Clone> ConcreteTypesRegisterHandler for StructConcreteTypesRegistry<T> {
    fn get_tuple_mut_ref_at_index(&mut self, index: usize) -> &mut ConcreteTypesTuple {
        &mut self.0[index].0
    }

    fn register_new_expanded_concrete_types_tuple(
        &mut self,
        tuple: Vec<Type>,
        index: usize,
    ) -> ConcreteTypesRegistryKey {
        let methods_concrete_types_map = self.0[index].1.methods_concrete_types_map.clone();
        let optional_arg = self.0[index].1.optional_arg.clone();
        let are_method_tuples_concretized = self.0[index].1.are_method_tuples_concretized;
        let key_index = self.0.len();
        self.0.push((
            ConcreteTypesTuple::new(tuple, vec![]),
            MethodsConcreteTypesRegistry {
                optional_arg,
                methods_concrete_types_map,
                are_method_tuples_concretized,
            },
        ));
        return ConcreteTypesRegistryKey(key_index);
    }
}
