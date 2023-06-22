use crate::types::core::Type;
use std::vec;

use super::core::AbstractConcreteTypesHandler;

#[derive(Debug)]
pub struct FunctionData {
    pub params: Vec<Type>,
    pub return_type: Type,
}

impl FunctionData {
    pub fn new(params: Vec<Type>, return_type: Type) -> Self {
        FunctionData {
            params,
            return_type,
        }
    }

    pub fn set_data(&mut self, params: Vec<Type>, return_type: Type) {
        self.params = params;
        self.return_type = return_type;
    }
}

impl AbstractConcreteTypesHandler for FunctionData {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> usize {
        todo!()
    }
}

impl Default for FunctionData {
    fn default() -> Self {
        FunctionData {
            params: vec![],
            return_type: Type::new_with_unset(),
        }
    }
}
