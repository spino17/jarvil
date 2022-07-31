use crate::types::core::Type;

#[derive(Debug, Clone)]
pub struct IdentifierData {
    pub data_type: Type,
    pub is_init: bool,
}
