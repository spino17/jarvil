use super::{concrete::core::ConcreteTypesRegistryKey, core::AbstractConcreteTypesHandler};
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

    pub fn is_initialized(&self) -> bool {
        self.is_init
    }
}

impl AbstractConcreteTypesHandler for VariableData {
    fn register_concrete_types(
        &mut self,
        _concrete_types: Vec<Type>,
        _has_generics: bool,
    ) -> ConcreteTypesRegistryKey {
        unreachable!()
    }

    fn is_generics_present_in_tuple_at_index(&self, _index: ConcreteTypesRegistryKey) -> bool {
        unreachable!()
    }

    fn has_generics(&self) -> bool {
        false
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
