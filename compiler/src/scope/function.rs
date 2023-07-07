use super::{
    concrete::core::ConcreteTypesRegistryKey,
    core::{AbstractConcreteTypesHandler, GenericTypeParams},
};
use crate::types::core::AbstractType;
use crate::types::core::Type;
use std::vec;

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
    pub generics: Option<GenericTypeParams>,
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
            generics: generics_spec,
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
        self.generics = generics_spec;
    }

    pub fn is_eq(&self, other: &FunctionData) -> bool {
        let (self_param_types, self_return_type) =
            (&self.prototype.params, &self.prototype.return_type);
        let (other_param_types, other_return_type) =
            (&other.prototype.params, &other.prototype.return_type);
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
        // TODO - check interface bounds also here!
        return true;
    }
}

impl AbstractConcreteTypesHandler for FunctionData {
    fn register_concrete_types(&mut self, _concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey {
        unreachable!()
    }

    fn get_concrete_types_at_key(&self, _key: ConcreteTypesRegistryKey) -> Vec<Type> {
        unreachable!()
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
