use crate::types::core::Type;
use std::vec;

use super::{
    concrete::{
        CallableConcreteTypesRegistry, ConcreteTypesRegistryKey,
        GenericsSpecAndConcreteTypesRegistry,
    },
    core::{AbstractConcreteTypesHandler, GenericContainingConstructs, GenericTypeParams},
    interfaces::InterfaceData,
};

#[derive(Debug)]
pub struct FunctionData {
    pub params: Vec<Type>,
    pub return_type: Type,
    pub generics: Option<GenericsSpecAndConcreteTypesRegistry<CallableConcreteTypesRegistry>>,
}

impl FunctionData {
    pub fn new(
        params: Vec<Type>,
        return_type: Type,
        generics_spec: Option<GenericTypeParams>,
    ) -> Self {
        FunctionData {
            params,
            return_type,
            generics: match generics_spec {
                Some(generic_spec) => Some(GenericsSpecAndConcreteTypesRegistry {
                    generics_spec: generic_spec,
                    concrete_types_registry: CallableConcreteTypesRegistry::default(),
                }),
                None => None,
            },
        }
    }

    pub fn set_data(
        &mut self,
        params: Vec<Type>,
        return_type: Type,
        generics_spec: Option<GenericTypeParams>,
    ) {
        self.params = params;
        self.return_type = return_type;
        self.generics = match generics_spec {
            Some(generic_spec) => Some(GenericsSpecAndConcreteTypesRegistry {
                generics_spec: generic_spec,
                concrete_types_registry: CallableConcreteTypesRegistry::default(),
            }),
            None => None,
        }
    }
}

impl AbstractConcreteTypesHandler for FunctionData {
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

impl GenericContainingConstructs for FunctionData {
    fn has_generics(&self) -> bool {
        self.generics.is_some()
    }
}

impl Default for FunctionData {
    fn default() -> Self {
        FunctionData {
            params: vec![],
            return_type: Type::new_with_unset(),
            generics: None,
        }
    }
}
