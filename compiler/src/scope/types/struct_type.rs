use std::collections::hash_map::Entry;

use rustc_hash::FxHashMap;
use text_size::TextRange;

use crate::{
    scope::{
        concrete::{
            CallableConcreteTypesRegistry, ConcreteTypesRegistryKey,
            GenericsSpecAndConcreteTypesRegistry, StructConcreteTypesRegistry,
        },
        core::{AbstractConcreteTypesHandler, GenericContainingConstructs, GenericTypeParams},
        function::{FunctionData, FunctionPrototype},
    },
    types::core::Type,
};

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
    pub generics: StructTypeGenerics,
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
        self.generics = match generics_spec {
            Some(generics_spec) => {
                StructTypeGenerics::HasGenerics(GenericsSpecAndConcreteTypesRegistry {
                    generics_spec,
                    concrete_types_registry: StructConcreteTypesRegistry::default(),
                })
            }
            None => StructTypeGenerics::NoGenerics(FxHashMap::default()),
        }
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
        method_concrete_types: &Vec<Type>,
    ) {
        match key {
            Some(key) => match &mut self.generics {
                StructTypeGenerics::HasGenerics(generics_spec) => generics_spec
                    .concrete_types_registry
                    .register_method_concrete_types_for_key(
                        key,
                        method_name,
                        method_concrete_types,
                    ),
                StructTypeGenerics::NoGenerics(_) => unreachable!(),
            },
            None => match &mut self.generics {
                StructTypeGenerics::HasGenerics(_) => unreachable!(),
                StructTypeGenerics::NoGenerics(generics_spec) => {
                    match generics_spec.entry(method_name.to_string()) {
                        Entry::Occupied(mut occupied_entry) => {
                            let occupied_entry_ref = occupied_entry.get_mut();
                            occupied_entry_ref.register_concrete_types(method_concrete_types);
                        }
                        Entry::Vacant(vacant_entry) => {
                            vacant_entry.insert(CallableConcreteTypesRegistry::new_with_entries(
                                vec![method_concrete_types.clone()],
                            ));
                        }
                    }
                }
            },
        }
    }
}

impl AbstractConcreteTypesHandler for StructTypeData {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        match &mut self.generics {
            StructTypeGenerics::HasGenerics(generics) => {
                return generics
                    .concrete_types_registry
                    .register_concrete_types(concrete_types)
            }
            StructTypeGenerics::NoGenerics(_) => unreachable!(),
        }
    }
}

impl GenericContainingConstructs for StructTypeData {
    fn has_generics(&self) -> bool {
        match self.generics {
            StructTypeGenerics::HasGenerics(_) => true,
            StructTypeGenerics::NoGenerics(_) => false,
        }
    }
}

#[derive(Debug)]
pub enum StructTypeGenerics {
    HasGenerics(GenericsSpecAndConcreteTypesRegistry<StructConcreteTypesRegistry>),
    NoGenerics(FxHashMap<String, CallableConcreteTypesRegistry>),
}

impl Default for StructTypeGenerics {
    fn default() -> Self {
        StructTypeGenerics::NoGenerics(FxHashMap::default())
    }
}