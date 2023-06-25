use crate::types::core::Type;
use std::vec;

use super::{
    concrete::{CallableConcreteTypesRegistry, ConcreteTypesRegistryKey},
    core::{AbstractConcreteTypesHandler, GenericTypeParams},
    interfaces::InterfaceData,
};

#[derive(Debug)]
pub struct FunctionData {
    pub params: Vec<Type>,
    pub return_type: Type,
    pub concrete_types_registry: CallableConcreteTypesRegistry,
    pub generics: Option<GenericTypeParams>,
}

impl FunctionData {
    pub fn new(params: Vec<Type>, return_type: Type, generics: Option<GenericTypeParams>) -> Self {
        FunctionData {
            params,
            return_type,
            generics,
            concrete_types_registry: CallableConcreteTypesRegistry::default(),
        }
    }

    pub fn set_data(&mut self, params: Vec<Type>, return_type: Type) {
        self.params = params;
        self.return_type = return_type;
    }
}

impl AbstractConcreteTypesHandler for FunctionData {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        self.concrete_types_registry
            .register_concrete_types(concrete_types)
    }

    fn has_generics(&self) -> bool {
        todo!()
    }
}

impl Default for FunctionData {
    fn default() -> Self {
        FunctionData {
            params: vec![],
            return_type: Type::new_with_unset(),
            generics: None,
            concrete_types_registry: CallableConcreteTypesRegistry::default(),
        }
    }
}
