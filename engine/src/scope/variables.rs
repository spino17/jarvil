use crate::types::core::Type;

#[derive(Debug, Clone)]
pub struct VariableData { // TODO - make it into enum to accomodate Lambdas also
    pub data_type: Type,
    pub is_init: bool,
}
