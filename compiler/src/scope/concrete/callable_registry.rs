use super::core::ConcreteTypesRegistryKey;
use super::core::ConcreteTypesTuple;
use crate::types::core::Type;

#[derive(Debug, Default, Clone)]
pub struct CallableConcreteTypesRegistry(Vec<ConcreteTypesTuple>);

impl CallableConcreteTypesRegistry {
    pub fn new_with_entries(entries: Vec<ConcreteTypesTuple>) -> Self {
        CallableConcreteTypesRegistry(entries)
    }

    pub fn register_concrete_types(
        &mut self,
        concrete_types: Vec<Type>,
    ) -> ConcreteTypesRegistryKey {
        let index = self.0.len();
        self.0.push(ConcreteTypesTuple::new(concrete_types));
        ConcreteTypesRegistryKey(index)
    }

    pub fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        self.0[key.0].get_concrete_types()
    }
}
