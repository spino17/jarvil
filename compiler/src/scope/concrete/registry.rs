use super::core::ConcreteTypesRegistryKey;
use super::core::ConcreteTypesTuple;
use crate::scope::core::GenericTypeParams;
use crate::types::core::Type;

#[derive(Debug)]
pub struct GenericsSpecAndConcreteTypesRegistry {
    pub generics_spec: Option<GenericTypeParams>,
    pub concrete_types_registry: ConcreteTypesRegistryCore,
}

impl GenericsSpecAndConcreteTypesRegistry {
    pub fn new(generics_spec: Option<GenericTypeParams>) -> Self {
        GenericsSpecAndConcreteTypesRegistry {
            generics_spec,
            concrete_types_registry: ConcreteTypesRegistryCore::default(),
        }
    }
}

impl Default for GenericsSpecAndConcreteTypesRegistry {
    fn default() -> Self {
        GenericsSpecAndConcreteTypesRegistry::new(None)
    }
}

#[derive(Debug, Default, Clone)]
pub struct ConcreteTypesRegistryCore(pub Vec<ConcreteTypesTuple>);

impl ConcreteTypesRegistryCore {
    pub fn new_with_entries(entries: Vec<ConcreteTypesTuple>) -> Self {
        ConcreteTypesRegistryCore(entries)
    }

    pub fn register_concrete_types(
        &mut self,
        concrete_types: Vec<Type>,
        has_generics: bool,
    ) -> ConcreteTypesRegistryKey {
        let index = self.0.len();
        self.0
            .push(ConcreteTypesTuple::new(concrete_types, has_generics));
        ConcreteTypesRegistryKey(index)
    }

    pub fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> &ConcreteTypesTuple {
        self.0[key.0].get_concrete_types()
    }
}
