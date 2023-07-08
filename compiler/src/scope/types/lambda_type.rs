use crate::{
    scope::{
        concrete::{
            core::ConcreteTypesRegistryKey,
            registry::{ConcreteTypesRegistryCore, GenericsSpecAndConcreteTypesRegistry},
        },
        core::{AbstractConcreteTypesHandler, GenericTypeParams},
        function::{CallablePrototypeData, PrototypeConcretizationResult},
    },
    types::core::Type,
};

#[derive(Debug)]
pub struct LambdaTypeData {
    pub prototype: CallablePrototypeData,
    pub generics: Option<GenericsSpecAndConcreteTypesRegistry>,
}

impl LambdaTypeData {
    pub fn new(
        param_types: Vec<Type>,
        return_type: Type,
        generics_spec: Option<GenericTypeParams>,
    ) -> Self {
        LambdaTypeData {
            prototype: CallablePrototypeData::new(param_types, return_type),
            generics: match generics_spec {
                Some(generics_spec) => Some(GenericsSpecAndConcreteTypesRegistry {
                    generics_spec,
                    concrete_types_registry: ConcreteTypesRegistryCore::default(),
                }),
                None => None,
            },
        }
    }

    pub fn get_concrete_prototype(
        &self,
        key: Option<ConcreteTypesRegistryKey>,
    ) -> PrototypeConcretizationResult {
        match key {
            Some(key) => {
                let index = key.0;
                match &self.generics {
                    Some(generics) => {
                        let concrete_types = &generics.concrete_types_registry.0[index];
                        return self
                            .prototype
                            .concretize_prototype(concrete_types.get_concrete_types());
                    }
                    None => unreachable!(),
                }
            }
            None => return PrototypeConcretizationResult::UnConcretized(&self.prototype),
        }
    }
}

impl AbstractConcreteTypesHandler for LambdaTypeData {
    fn register_concrete_types(&mut self, concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey {
        match &mut self.generics {
            Some(generics) => {
                return generics
                    .concrete_types_registry
                    .register_concrete_types(concrete_types)
            }
            None => unreachable!(),
        }
    }

    fn has_generics(&self) -> bool {
        self.generics.is_some()
    }
}
