use crate::scope::concrete::struct_registry::StructTypeGenerics;
use crate::{
    scope::{
        concrete::core::ConcreteTypesRegistryKey,
        core::{AbstractConcreteTypesHandler, GenericContainingConstructs, GenericTypeParams},
        function::FunctionPrototype,
    },
    types::core::Type,
};
use rustc_hash::FxHashMap;
use text_size::TextRange;

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
    pub constructor: MethodData,
    pub methods: FxHashMap<String, (MethodData, TextRange)>,
    pub class_methods: FxHashMap<String, (MethodData, TextRange)>,
    pub generics: StructTypeGenerics<()>,
    pub generics_containing_level: GenericContainingLevel,
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

    fn register_method_concrete_types(
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

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        self.generics.get_concrete_types_at_key(key)
    }
}

impl GenericContainingConstructs for StructTypeData {
    fn has_generics(&self) -> bool {
        self.generics.has_generics()
    }
}
