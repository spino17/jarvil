use crate::types::core::Type;

#[derive(Debug, Clone)]
pub struct VariableData {
    pub data_type: Type,
    pub is_init: bool,
    pub is_captured: bool,
    pub stack_index: usize,
}

impl VariableData {
    pub fn new(stack_index: usize) -> Self {
        VariableData {
            data_type: Type::new_with_unknown(),
            is_captured: false,
            is_init: true,
            stack_index,
        }
    }

    pub fn set_data_type(&mut self, data_type: &Type) {
        self.data_type = data_type.clone();
    }

    pub fn set_is_captured(&mut self) {
        self.is_captured = true;
    }

    pub fn stack_index(&self) -> usize {
        self.stack_index
    }
}
