use super::{
    concrete::{core::ConcreteTypesRegistryKey, registry::{ConcreteTypesRegistryCore, GenericsSpecAndConcreteTypesRegistry}},
    core::{AbstractConcreteTypesHandler, GenericTypeParams},
};
use crate::types::core::AbstractType;
use crate::types::core::Type;
use std::vec;

#[derive(Debug)]
pub enum FunctionKind {
    Function,
    Method,
    Lambda
}

#[derive(Debug)]
pub struct FunctionPrototype {
    pub params: Vec<Type>,
    pub return_type: Type,
}

impl FunctionPrototype {
    pub fn new(params: Vec<Type>, return_type: Type) -> FunctionPrototype {
        FunctionPrototype {
            params,
            return_type,
            
        }
    }

    pub fn is_eq(&self, other: &FunctionPrototype) -> bool {
        let (self_param_types, self_return_type) =
            (&self.params, &self.return_type);
        let (other_param_types, other_return_type) =
            (&other.params, &other.return_type);
        let self_params_len = self_param_types.len();
        let other_params_len = other_param_types.len();
        if self_params_len != other_params_len {
            return false;
        }
        if !self_return_type.is_eq(&other_return_type) {
            return false;
        }
        for index in 0..self_params_len {
            if !self_param_types[index].is_eq(&other_param_types[index]) {
                return false;
            }
        }
        return true;
    }
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
    pub generics: Option<GenericsSpecAndConcreteTypesRegistry>,
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
                Some(generics_spec) => Some(GenericsSpecAndConcreteTypesRegistry {
                    generics_spec,
                    concrete_types_registry: ConcreteTypesRegistryCore::default(),
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
            Some(generics_spec) => Some(GenericsSpecAndConcreteTypesRegistry {
                generics_spec,
                concrete_types_registry: ConcreteTypesRegistryCore::default(),
            }),
            None => None,
        };
    }
}

impl AbstractConcreteTypesHandler for FunctionData {
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

impl Default for FunctionData {
    fn default() -> Self {
        FunctionData {
            prototype: FunctionPrototype::default(),
            generics: None,
        }
    }
}

