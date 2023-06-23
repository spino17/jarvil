use super::{
    concrete::ConcreteTypesRegistryKey, core::AbstractConcreteTypesHandler, function::FunctionData,
};
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Debug, Default)]
pub struct InterfaceData {
    pub fields: FxHashMap<String, (Type, TextRange)>,
    pub methods: FxHashMap<String, (FunctionData, TextRange)>,
    // pub concrete_types_registry: Vec<Vec<Type>>,
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
}

impl AbstractConcreteTypesHandler for InterfaceData {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        todo!()
    }
}
