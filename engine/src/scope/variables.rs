use crate::types::core::Type;

#[derive(Debug, Clone)]
pub struct VariableData {
    pub data_type: Type,
    pub is_init: bool,
}
impl Default for VariableData {
    fn default() -> Self {
        VariableData{
            data_type: Type::new_with_unknown(),
            is_init: true,
        }
    }
}
