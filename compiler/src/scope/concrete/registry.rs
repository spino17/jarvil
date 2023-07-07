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

#[derive(Debug)]
pub enum ConcreteTypesRegistryForStructLikeConstructs {
    HasGenerics(GenericsSpecAndConcreteTypesRegistry),
    NoGenerics,
}

impl ConcreteTypesRegistryForStructLikeConstructs {
    pub fn new(generics_spec: Option<GenericTypeParams>) -> Self {
        match generics_spec {
            Some(generics_spec) => ConcreteTypesRegistryForStructLikeConstructs::HasGenerics(
                GenericsSpecAndConcreteTypesRegistry {
                    generics_spec,
                    concrete_types_registry: ConcreteTypesRegistryCore::default(),
                },
            ),
            None => ConcreteTypesRegistryForStructLikeConstructs::NoGenerics,
        }
    }
}

impl AbstractConcreteTypesHandler for ConcreteTypesRegistryForStructLikeConstructs {
    fn register_concrete_types(&mut self, concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey {
        match self {
            ConcreteTypesRegistryForStructLikeConstructs::HasGenerics(generics) => {
                return generics
                    .concrete_types_registry
                    .register_concrete_types(concrete_types)
            }
            ConcreteTypesRegistryForStructLikeConstructs::NoGenerics => unreachable!(),
        }
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        match self {
            ConcreteTypesRegistryForStructLikeConstructs::HasGenerics(generics_spec) => {
                generics_spec
                    .concrete_types_registry
                    .get_concrete_types_at_key(key)
            }
            ConcreteTypesRegistryForStructLikeConstructs::NoGenerics => unreachable!(),
        }
    }

    fn has_generics(&self) -> bool {
        match self {
            ConcreteTypesRegistryForStructLikeConstructs::HasGenerics(_) => true,
            ConcreteTypesRegistryForStructLikeConstructs::NoGenerics => false,
        }
    }
}

impl Default for ConcreteTypesRegistryForStructLikeConstructs {
    fn default() -> Self {
        ConcreteTypesRegistryForStructLikeConstructs::NoGenerics
    }
}
