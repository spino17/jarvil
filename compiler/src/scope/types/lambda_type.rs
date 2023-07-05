use crate::{
    scope::{
        concrete::{
            callable_registry::CallableConcreteTypesRegistry,
            core::{ConcreteTypesRegistryKey, GenericsSpecAndConcreteTypesRegistry},
        },
        core::{AbstractConcreteTypesHandler, GenericTypeParams},
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
    fn register_concrete_types(
        &mut self,
        concrete_types: Vec<Type>,
        generics_containing_indexes: Vec<usize>,
    ) -> ConcreteTypesRegistryKey {
        self.meta_data
            .register_concrete_types(concrete_types, generics_containing_indexes)
    }

    fn register_method_concrete_types(
        &mut self,
        _key: Option<ConcreteTypesRegistryKey>,
        _ethod_name: String,
        _method_concrete_types: Vec<Type>,
        _method_generics_containing_indexes: Vec<usize>,
    ) {
        unreachable!()
    }

    fn get_concrete_types_at_key(&self, key: ConcreteTypesRegistryKey) -> Vec<Type> {
        self.meta_data.get_concrete_types_at_key(key)
    }

    fn has_generics(&self) -> bool {
        self.meta_data.has_generics()
    }
}
