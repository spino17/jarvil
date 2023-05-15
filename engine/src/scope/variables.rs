use crate::types::core::Type;

#[derive(Debug, Clone)]
pub struct VariableData {
    pub data_type: Type,
    pub is_init: bool,
}

impl VariableData {
    pub fn new(variable_type: &Type) -> Self {
        VariableData {
            data_type: variable_type.clone(),
            is_init: true,
        }
    }

    pub fn set_data_type(&mut self, data_type: &Type) {
        self.data_type = data_type.clone();
    }
}

impl Default for VariableData {
    fn default() -> Self {
        VariableData {
            data_type: Type::new_with_unknown(),
            is_init: true,
        }
    }
}
