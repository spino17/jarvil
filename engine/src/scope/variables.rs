use crate::types::core::Type;

#[derive(Debug, Clone)]
pub struct CoreVariableData {
    pub data_type: Type,
    pub is_init: bool,
}

#[derive(Debug, Clone)]
pub struct VariableData(pub Option<CoreVariableData>);
