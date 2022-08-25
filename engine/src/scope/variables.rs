use crate::types::core::Type;

#[derive(Debug, Clone)]
pub struct VariableData {
    pub data_type: Option<Type>,
    pub is_init: bool,
}
