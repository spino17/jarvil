use crate::{
    scope::{
        concrete::{
            core::ConcreteTypesRegistryKey,
            registry::{ConcreteTypesRegistryCore, GenericsSpecAndConcreteTypesRegistry},
        },
        core::{AbstractConcreteTypesHandler, GenericTypeParams},
        function::CallablePrototypeData,
    },
    types::core::Type,
};

#[derive(Debug)]
pub struct LambdaTypeData {
    pub meta_data: CallablePrototypeData,
    pub generics: Option<GenericsSpecAndConcreteTypesRegistry>,
}

impl LambdaTypeData {
    pub fn new(
        param_types: Vec<Type>,
        return_type: Type,
        generics_spec: Option<GenericTypeParams>,
    ) -> Self {
        LambdaTypeData {
            meta_data: CallablePrototypeData::new(param_types, return_type),
            generics: match generics_spec {
                Some(generics_spec) => Some(GenericsSpecAndConcreteTypesRegistry {
                    generics_spec,
                    concrete_types_registry: ConcreteTypesRegistryCore::default(),
                }),
                None => None,
            },
        }
    }
}

impl AbstractConcreteTypesHandler for LambdaTypeData {
    fn register_concrete_types(&mut self, concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey {
        match &mut self.generics {
            Some(generics) => {
                return generics
                    .concrete_types_registry
                    .register_concrete_types(concrete_types)
            }
            None => unreachable!(),
        }
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        match &self.generics {
            Some(generics_spec) => generics_spec
                .concrete_types_registry
                .get_concrete_types_at_key(key),
            None => unreachable!(),
        }
    }

    fn has_generics(&self) -> bool {
        self.generics.is_some()
    }
}
