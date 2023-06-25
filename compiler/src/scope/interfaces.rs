use super::{
    concrete::{ConcreteTypesRegistryKey, StructConcreteTypesRegistry},
    core::{AbstractConcreteTypesHandler, GenericTypeParams},
    function::FunctionData,
};
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Debug, Default)]
pub struct InterfaceData {
    pub fields: FxHashMap<String, (Type, TextRange)>,
    pub methods: FxHashMap<String, (FunctionData, TextRange)>,
    pub concrete_types_registry: StructConcreteTypesRegistry,
    pub generics: Option<GenericTypeParams>,
}

impl InterfaceData {
    fn set_meta_data(
        &mut self,
        fields: FxHashMap<String, (Type, TextRange)>,
        methods: FxHashMap<String, (FunctionData, TextRange)>,
    ) {
        self.fields = fields;
        self.methods = methods;
    }

    pub fn register_method_concrete_types_for_key(
        &mut self,
        key: &ConcreteTypesRegistryKey,
        method_name: String,
        method_concrete_types: &Vec<Type>,
    ) {
        self.concrete_types_registry
            .register_method_concrete_types_for_key(key, method_name, method_concrete_types)
    }
}

impl AbstractConcreteTypesHandler for InterfaceData {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        self.concrete_types_registry
            .register_concrete_types(concrete_types)
    }

    fn has_generics(&self) -> bool {
        todo!()
    }
}
