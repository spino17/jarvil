use super::core::ConcreteTypesRegistryKey;
use super::core::ConcreteTypesTuple;
use crate::scope::core::AbstractConcreteTypesHandler;
use crate::scope::core::GenericTypeParams;
use crate::types::core::Type;

#[derive(Debug)]
pub struct GenericsSpecAndConcreteTypesRegistry {
    pub generics_spec: GenericTypeParams,
    pub concrete_types_registry: ConcreteTypesRegistryCore,
}

impl GenericsSpecAndConcreteTypesRegistry {
    fn new(generics_spec: GenericTypeParams) -> Self {
        GenericsSpecAndConcreteTypesRegistry {
            generics_spec,
            concrete_types_registry: ConcreteTypesRegistryCore::default(),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ConcreteTypesRegistryCore(Vec<ConcreteTypesTuple>);

impl ConcreteTypesRegistryCore {
    pub fn new_with_entries(entries: Vec<ConcreteTypesTuple>) -> Self {
        ConcreteTypesRegistryCore(entries)
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
