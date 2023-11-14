use super::core::AbstractConcreteTypesHandler;
use crate::types::core::Type;

#[derive(Debug)]
pub struct VariableData {
    pub data_type: Type,
    pub is_init: bool,
}

impl VariableData {
    pub fn new(variable_type: &Type, is_init: bool) -> Self {
        VariableData {
            data_type: variable_type.clone(),
            is_init,
        }
    }

    pub fn set_data_type(&mut self, data_type: &Type) {
        self.data_type = data_type.clone();
    }

    pub fn set_is_init(&mut self, is_init: bool) {
        self.is_init = is_init
    }

    pub fn set_data_type_from_optional_annotation(&mut self, ty: Type) {
        self.is_init = true;
        self.data_type = ty;
    }
}

impl AbstractConcreteTypesHandler for VariableData {
    fn is_initialized(&self) -> bool {
        self.is_init
    }
}

impl Default for VariableData {
    fn default() -> Self {
        VariableData {
            data_type: Type::new_with_unset(),
            is_init: false,
        }
    }
}
