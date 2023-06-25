use crate::{
    scope::{
        concrete::{
            CallableConcreteTypesRegistry, ConcreteTypesRegistryKey,
            GenericsSpecAndConcreteTypesRegistry,
        },
        core::{AbstractConcreteTypesHandler, GenericContainingConstructs, GenericTypeParams},
        function::{FunctionData, FunctionPrototype},
    },
    types::core::Type,
};

#[derive(Debug, Default)]
pub struct LambdaTypeData {
    pub meta_data: FunctionData,
}

impl LambdaTypeData {
    pub fn new(
        param_types: Vec<Type>,
        return_type: Type,
        generics_spec: Option<GenericTypeParams>,
    ) -> Self {
        LambdaTypeData {
            meta_data: FunctionData {
                prototype: FunctionPrototype {
                    params: param_types,
                    return_type,
                },
                generics: match generics_spec {
                    Some(generic_spec) => Some(GenericsSpecAndConcreteTypesRegistry {
                        generics_spec: generic_spec,
                        concrete_types_registry: CallableConcreteTypesRegistry::default(),
                    }),
                    None => None,
                },
            },
        }
    }
}

impl AbstractConcreteTypesHandler for LambdaTypeData {
    fn register_concrete_types(&mut self, concrete_types: &Vec<Type>) -> ConcreteTypesRegistryKey {
        self.meta_data.register_concrete_types(concrete_types)
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        self.meta_data.get_concrete_types_at_key(key)
    }
}

impl GenericContainingConstructs for LambdaTypeData {
    fn has_generics(&self) -> bool {
        self.meta_data.has_generics()
    }
}
