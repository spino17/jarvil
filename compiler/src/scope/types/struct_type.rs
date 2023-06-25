use rustc_hash::FxHashMap;
use text_size::TextRange;

use crate::{
    scope::{
        concrete::{
            ConcreteTypesRegistryKey, GenericsSpecAndConcreteTypesRegistry,
            StructConcreteTypesRegistry,
        },
        core::{AbstractConcreteTypesHandler, GenericContainingConstructs, GenericTypeParams},
        function::{FunctionData, FunctionPrototype},
    },
    types::core::Type,
};

#[derive(Debug)]
pub struct MethodData {
    pub prototype: FunctionPrototype,
    pub generics: Option<GenericTypeParams>, // the concrete types registry is handled by the struct
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
    pub constructor: FunctionData,
    pub methods: FxHashMap<String, (FunctionData, TextRange)>,
    pub class_methods: FxHashMap<String, (FunctionData, TextRange)>,
    pub generics: Option<GenericsSpecAndConcreteTypesRegistry<StructConcreteTypesRegistry>>,
}

impl StructTypeData {
    pub fn set_meta_data(
        &mut self,
        fields: FxHashMap<String, (Type, TextRange)>,
        constructor: Option<(FunctionData, TextRange)>,
        methods: FxHashMap<String, (FunctionData, TextRange)>,
        class_methods: FxHashMap<String, (FunctionData, TextRange)>,
        generics_spec: Option<GenericTypeParams>,
    ) {
        self.fields = fields;
        self.methods = methods;
        self.class_methods = class_methods;
        if let Some((constructor_meta_data, _)) = constructor {
            self.constructor = constructor_meta_data;
        }
        self.generics = match generics_spec {
            Some(generics_spec) => Some(GenericsSpecAndConcreteTypesRegistry {
                generics_spec,
                concrete_types_registry: StructConcreteTypesRegistry::default(),
            }),
            None => None,
        }
    }

    pub fn try_field(&self, field_name: &str) -> Option<(Type, TextRange)> {
        match self.fields.get(field_name) {
            Some(type_obj) => return Some((type_obj.0.clone(), type_obj.1)),
            None => None,
        }
    }

    pub fn try_method(&self, method_name: &str) -> Option<(&FunctionData, TextRange)> {
        match self.methods.get(method_name) {
            Some(func_data) => Some((&func_data.0, func_data.1)),
            None => None,
        }
    }

    pub fn register_method_concrete_types_for_key(
        &mut self,
        key: &ConcreteTypesRegistryKey,
        method_name: String,
        method_concrete_types: &Vec<Type>,
    ) {
        //self.concrete_types_registry
        //    .register_method_concrete_types_for_key(key, method_name, method_concrete_types)
        todo!()
    }
}

impl AbstractConcreteTypesHandler for StructTypeData {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        match &mut self.generics {
            Some(generics) => {
                return generics
                    .concrete_types_registry
                    .register_concrete_types(concrete_types)
            }
            None => unreachable!(),
        }
    }
}

impl GenericContainingConstructs for StructTypeData {
    fn has_generics(&self) -> bool {
        self.generics.is_some()
    }
}
