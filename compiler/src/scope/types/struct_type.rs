use crate::scope::concrete::struct_registry::StructConcreteTypesRegistry;
use crate::scope::function::FunctionData;
use crate::{
    scope::{
        concrete::core::ConcreteTypesRegistryKey,
        core::{AbstractConcreteTypesHandler, GenericTypeParams},
    },
    types::core::Type,
};
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Debug)]
pub enum GenericContainingLevel {
    HasTypeAndMethodGenerics,
    HasOnlyMethodGenerics,
    NoGenerics,
}

impl Default for GenericContainingLevel {
    fn default() -> Self {
        GenericContainingLevel::NoGenerics
    }
}

#[derive(Debug, Default)]
pub struct StructTypeData {
    pub fields: FxHashMap<String, (Type, TextRange)>,
    pub constructor: FunctionData,
    pub methods: FxHashMap<String, (FunctionData, TextRange)>,
    pub class_methods: FxHashMap<String, (FunctionData, TextRange)>,
    pub generics: StructConcreteTypesRegistry,
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
        self.generics = StructConcreteTypesRegistry::new(generics_spec)
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
}

impl AbstractConcreteTypesHandler for StructTypeData {
    fn register_concrete_types(&mut self, concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey {
        self.generics.register_concrete_types(concrete_types)
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        self.generics.get_concrete_types_at_key(key)
    }

    fn has_generics(&self) -> bool {
        self.generics.has_generics()
    }
}
