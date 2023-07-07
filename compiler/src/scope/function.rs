use super::{
    concrete::core::ConcreteTypesRegistryKey,
    core::{AbstractConcreteTypesHandler, GenericTypeParams},
};
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
    pub generics: Option<GenericTypeParams>, // the concrete types registry for methods is handled by the struct registry
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
}

impl AbstractConcreteTypesHandler for FunctionData {
    fn register_concrete_types(&mut self, _concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey {
        unreachable!()
    }

    fn get_concrete_types_at_key(&self, _key: ConcreteTypesRegistryKey) -> Vec<Type> {
        unreachable!()
    }

    fn has_generics(&self) -> bool {
        unreachable!()
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
