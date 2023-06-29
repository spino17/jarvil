use rustc_hash::FxHashMap;
use std::collections::hash_map::Entry;
use text_size::TextRange;

use crate::{
    scope::{
        concrete::{
            CallableConcreteTypesRegistry, ConcreteTypesRegistryKey, ConcreteTypesTuple,
            GenericsSpecAndConcreteTypesRegistry, StructConcreteTypesRegistry,
        },
        core::{AbstractConcreteTypesHandler, GenericContainingConstructs, GenericTypeParams},
        function::FunctionPrototype,
    },
    types::core::Type,
};

use super::core::AbstractConcretizer;

#[derive(Debug)]
pub struct MethodData {
    pub prototype: FunctionPrototype,
    pub generics: Option<GenericTypeParams>, // the concrete types registry for methods is handled by the struct registry
}

impl MethodData {
    pub fn new(params: Vec<Type>, return_type: Type, generics: Option<GenericTypeParams>) -> Self {
        MethodData {
            prototype: FunctionPrototype {
                params,
                return_type,
            },
            generics,
        }
    }

    pub fn set_data(
        &mut self,
        params: Vec<Type>,
        return_type: Type,
        generics: Option<GenericTypeParams>,
    ) {
        self.prototype.params = params;
        self.prototype.return_type = return_type;
        self.generics = generics;
    }
}

impl GenericContainingConstructs for MethodData {
    fn has_generics(&self) -> bool {
        self.generics.is_some()
    }
}

impl Default for MethodData {
    fn default() -> Self {
        MethodData {
            prototype: FunctionPrototype::default(),
            generics: None,
        }
    }
}

#[derive(Debug, Default)]
pub struct StructTypeData {
    pub fields: FxHashMap<String, (Type, TextRange)>,
    pub constructor: MethodData,
    pub methods: FxHashMap<String, (MethodData, TextRange)>,
    pub class_methods: FxHashMap<String, (MethodData, TextRange)>,
    pub generics: StructTypeGenerics<()>,
}

impl StructTypeData {
    pub fn set_meta_data(
        &mut self,
        fields: FxHashMap<String, (Type, TextRange)>,
        constructor: Option<(MethodData, TextRange)>,
        methods: FxHashMap<String, (MethodData, TextRange)>,
        class_methods: FxHashMap<String, (MethodData, TextRange)>,
        generics_spec: Option<GenericTypeParams>,
    ) {
        self.fields = fields;
        self.methods = methods;
        self.class_methods = class_methods;
        if let Some((constructor_meta_data, _)) = constructor {
            self.constructor = constructor_meta_data;
        }
        self.generics = StructTypeGenerics::new(generics_spec)
    }

    pub fn try_field(&self, field_name: &str) -> Option<(Type, TextRange)> {
        match self.fields.get(field_name) {
            Some(type_obj) => return Some((type_obj.0.clone(), type_obj.1)),
            None => None,
        }
    }

    pub fn try_method(&self, method_name: &str) -> Option<(&MethodData, TextRange)> {
        match self.methods.get(method_name) {
            Some(func_data) => Some((&func_data.0, func_data.1)),
            None => None,
        }
    }

    pub fn register_method_concrete_types(
        &mut self,
        key: Option<ConcreteTypesRegistryKey>,
        method_name: String,
        method_concrete_types: Vec<Type>,
        method_generics_containing_indexes: Vec<usize>,
    ) {
        self.generics.register_method_concrete_types(
            key,
            method_name,
            method_concrete_types,
            method_generics_containing_indexes,
        )
    }
}

impl AbstractConcreteTypesHandler for StructTypeData {
    fn register_concrete_types(
        &mut self,
        concrete_types: Vec<Type>,
        generics_containing_indexes: Vec<usize>,
    ) -> ConcreteTypesRegistryKey {
        self.generics
            .register_concrete_types(concrete_types, generics_containing_indexes)
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        self.generics.get_concrete_types_at_key(key)
    }
}

impl GenericContainingConstructs for StructTypeData {
    fn has_generics(&self) -> bool {
        self.generics.has_generics()
    }
}

impl AbstractConcretizer for StructTypeData {
    fn concretize(&mut self) -> Vec<ConcreteTypesRegistryKey> {
        todo!()
    }
}

#[derive(Debug)]
pub enum StructTypeGenerics<U: Default> {
    HasGenerics(GenericsSpecAndConcreteTypesRegistry<StructConcreteTypesRegistry<U>>),
    NoGenerics((U, FxHashMap<String, CallableConcreteTypesRegistry>)),
}

impl<U: Default> StructTypeGenerics<U> {
    pub fn new(generics_spec: Option<GenericTypeParams>) -> Self {
        match generics_spec {
            Some(generics_spec) => {
                StructTypeGenerics::HasGenerics(GenericsSpecAndConcreteTypesRegistry {
                    generics_spec,
                    concrete_types_registry: StructConcreteTypesRegistry::default(),
                })
            }
            None => StructTypeGenerics::NoGenerics((U::default(), FxHashMap::default())),
        }
    }

    pub fn register_concrete_types(
        &mut self,
        concrete_types: Vec<Type>,
        generics_containing_indexes: Vec<usize>,
    ) -> ConcreteTypesRegistryKey {
        match self {
            StructTypeGenerics::HasGenerics(generics) => {
                return generics
                    .concrete_types_registry
                    .register_concrete_types(concrete_types, generics_containing_indexes)
            }
            StructTypeGenerics::NoGenerics(_) => unreachable!(),
        }
    }

    pub fn register_implementing_struct(&mut self, key: Option<ConcreteTypesRegistryKey>) {
        todo!()
    }

    pub fn register_method_concrete_types(
        &mut self,
        key: Option<ConcreteTypesRegistryKey>,
        method_name: String,
        method_concrete_types: Vec<Type>,
        method_generics_containing_indexes: Vec<usize>,
    ) {
        match key {
            Some(key) => match self {
                StructTypeGenerics::HasGenerics(generics_spec) => generics_spec
                    .concrete_types_registry
                    .register_method_concrete_types_for_key(
                        key,
                        method_name,
                        method_concrete_types,
                        method_generics_containing_indexes,
                    ),
                StructTypeGenerics::NoGenerics(_) => unreachable!(),
            },
            None => match self {
                StructTypeGenerics::HasGenerics(_) => unreachable!(),
                StructTypeGenerics::NoGenerics((_, generics_spec)) => {
                    match generics_spec.entry(method_name.to_string()) {
                        Entry::Occupied(mut occupied_entry) => {
                            let occupied_entry_ref = occupied_entry.get_mut();
                            occupied_entry_ref.register_concrete_types(
                                method_concrete_types,
                                method_generics_containing_indexes,
                            );
                        }
                        Entry::Vacant(vacant_entry) => {
                            vacant_entry.insert(CallableConcreteTypesRegistry::new_with_entries(
                                vec![ConcreteTypesTuple::new(
                                    method_concrete_types,
                                    method_generics_containing_indexes,
                                )],
                            ));
                        }
                    }
                }
            },
        }
    }

    pub fn has_generics(&self) -> bool {
        match self {
            StructTypeGenerics::HasGenerics(_) => true,
            StructTypeGenerics::NoGenerics(_) => false,
        }
    }

    pub fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        match self {
            StructTypeGenerics::HasGenerics(generics_spec) => generics_spec
                .concrete_types_registry
                .get_concrete_types_at_key(key),
            StructTypeGenerics::NoGenerics(_) => unreachable!(),
        }
    }
}

impl<U: Default> Default for StructTypeGenerics<U> {
    fn default() -> Self {
        StructTypeGenerics::NoGenerics((U::default(), FxHashMap::default()))
    }
}
