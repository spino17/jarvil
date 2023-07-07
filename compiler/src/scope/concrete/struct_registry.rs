use super::callable_registry::CallableConcreteTypesRegistry;
use super::core::ConcreteTypesRegistryKey;
use super::core::GenericsSpecAndConcreteTypesRegistry;
use crate::scope::core::AbstractConcreteTypesHandler;
use crate::scope::core::GenericTypeParams;
use crate::types::core::Type;

#[derive(Debug)]
pub enum StructConcreteTypesRegistry {
    HasGenerics(GenericsSpecAndConcreteTypesRegistry),
    NoGenerics,
}

impl StructConcreteTypesRegistry {
    pub fn new(generics_spec: Option<GenericTypeParams>) -> Self {
        match generics_spec {
            Some(generics_spec) => {
                StructConcreteTypesRegistry::HasGenerics(GenericsSpecAndConcreteTypesRegistry {
                    generics_spec,
                    concrete_types_registry: CallableConcreteTypesRegistry::default(),
                })
            }
            None => StructConcreteTypesRegistry::NoGenerics,
        }
    }
}

impl AbstractConcreteTypesHandler for StructConcreteTypesRegistry {
    fn register_concrete_types(&mut self, concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey {
        match self {
            StructConcreteTypesRegistry::HasGenerics(generics) => {
                return generics
                    .concrete_types_registry
                    .register_concrete_types(concrete_types)
            }
            StructConcreteTypesRegistry::NoGenerics => unreachable!(),
        }
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        match self {
            StructConcreteTypesRegistry::HasGenerics(generics_spec) => generics_spec
                .concrete_types_registry
                .get_concrete_types_at_key(key),
            StructConcreteTypesRegistry::NoGenerics => unreachable!(),
        }
    }

    fn has_generics(&self) -> bool {
        match self {
            StructConcreteTypesRegistry::HasGenerics(_) => true,
            StructConcreteTypesRegistry::NoGenerics => false,
        }
    }
}

impl Default for StructConcreteTypesRegistry {
    fn default() -> Self {
        StructConcreteTypesRegistry::NoGenerics
    }
}
