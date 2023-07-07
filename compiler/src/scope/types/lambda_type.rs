use crate::{
    scope::{
        concrete::core::ConcreteTypesRegistryKey,
        core::{AbstractConcreteTypesHandler, GenericTypeParams},
        function::{FunctionData, FunctionKind},
    },
    types::core::Type,
};

#[derive(Debug)]
pub struct LambdaTypeData {
    pub meta_data: FunctionData,
}

impl LambdaTypeData {
    pub fn new(
        param_types: Vec<Type>,
        return_type: Type,
        generics_spec: Option<GenericTypeParams>,
    ) -> Self {
        LambdaTypeData {
            meta_data: FunctionData::new(param_types, return_type, generics_spec)
        }
    }
}

impl AbstractConcreteTypesHandler for LambdaTypeData {
    fn register_concrete_types(&mut self, concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey {
        self.meta_data.register_concrete_types(concrete_types)
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        self.meta_data.get_concrete_types_at_key(key)
    }

    fn has_generics(&self) -> bool {
        self.meta_data.has_generics()
    }
}
