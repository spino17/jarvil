use super::core::ConcreteTypesRegisterHandler;
use super::core::ConcreteTypesRegistryKey;
use super::core::ConcreteTypesTuple;
use crate::scope::core::AbstractConcreteTypesHandler;
use crate::types::core::Type;

#[derive(Debug, Default, Clone)]
pub struct CallableConcreteTypesRegistry(Vec<ConcreteTypesTuple>);

impl CallableConcreteTypesRegistry {
    pub fn new_with_entries(entries: Vec<ConcreteTypesTuple>) -> Self {
        CallableConcreteTypesRegistry(entries)
    }

    pub fn concretize_all_entries(&mut self) {
        todo!()
    }
}

impl AbstractConcreteTypesHandler for CallableConcreteTypesRegistry {
    fn register_concrete_types(
        &mut self,
        concrete_types: Vec<Type>,
        generics_containing_indexes: Vec<usize>,
    ) -> ConcreteTypesRegistryKey {
        let index = self.0.len();
        self.0.push(ConcreteTypesTuple::new(
            concrete_types,
            generics_containing_indexes,
        ));
        ConcreteTypesRegistryKey(index)
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        self.0[key.0].get_concrete_types()
    }
}

impl ConcreteTypesRegisterHandler for CallableConcreteTypesRegistry {
    fn get_tuple_mut_ref_at_index(&mut self, index: usize) -> &mut ConcreteTypesTuple {
        &mut self.0[index]
    }

    fn register_new_expanded_concrete_types_tuple(
        &mut self,
        tuple: Vec<Type>,
        _index: usize,
    ) -> ConcreteTypesRegistryKey {
        self.register_concrete_types(tuple, vec![])
    }
}
