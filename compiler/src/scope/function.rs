use crate::types::core::Type;
use std::vec;

use super::{
    concrete::{
        CallableConcreteTypesRegistry, ConcreteTypesRegistryKey,
        GenericsSpecAndConcreteTypesRegistry,
    },
    core::{AbstractConcreteTypesHandler, GenericContainingConstructs, GenericTypeParams},
};

#[derive(Debug)]
pub struct FunctionPrototype {
    pub params: Vec<Type>,
    pub return_type: Type,
}

impl Default for FunctionPrototype {
    fn default() -> Self {
        FunctionPrototype {
            params: vec![],
            return_type: Type::new_with_unset(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionData {
    pub prototype: FunctionPrototype,
    pub generics: Option<GenericsSpecAndConcreteTypesRegistry<CallableConcreteTypesRegistry>>,
}

impl FunctionData {
    pub fn new(
        params: Vec<Type>,
        return_type: Type,
        generics_spec: Option<GenericTypeParams>,
    ) -> Self {
        FunctionData {
            prototype: FunctionPrototype {
                params,
                return_type,
            },
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
        self.prototype.params = params;
        self.prototype.return_type = return_type;
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
    fn register_concrete_types(
        &mut self,
        concrete_types: Vec<Type>,
        generics_containing_indexes: Vec<usize>,
    ) -> ConcreteTypesRegistryKey {
        match &mut self.generics {
            Some(generics) => {
                return generics
                    .concrete_types_registry
                    .register_concrete_types(concrete_types, generics_containing_indexes)
            }
            None => unreachable!(),
        }
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        match &self.generics {
            Some(generics) => generics
                .concrete_types_registry
                .get_concrete_types_at_key(key),
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
            prototype: FunctionPrototype::default(),
            generics: None,
        }
    }
}
