use crate::{
    scope::{
        concrete::core::{ConcreteTypesRegistryKey, ConcreteTypesTuple},
        core::{AbstractConcreteTypesHandler, GenericTypeParams},
        function::{CallableData, CallableKind, PrototypeConcretizationResult},
    },
    types::core::Type,
};

#[derive(Debug)]
pub struct LambdaTypeData {
    pub meta_data: CallableData,
}

impl LambdaTypeData {
    pub fn new(
        param_types: Vec<Type>,
        return_type: Type,
        is_concretization_required: bool,
        generics_spec: Option<GenericTypeParams>,
    ) -> Self {
        LambdaTypeData {
            meta_data: CallableData::new(
                param_types,
                return_type,
                CallableKind::LambdaType,
                is_concretization_required,
                generics_spec,
            ),
        }
    }

    pub fn get_concrete_types(&self, key: ConcreteTypesRegistryKey) -> &ConcreteTypesTuple {
        return self.meta_data.get_concrete_types(key);
    }

    pub fn get_concrete_prototype(
        &self,
        key: Option<ConcreteTypesRegistryKey>,
    ) -> PrototypeConcretizationResult {
        match key {
            Some(key) => {
                let concrete_types = self.meta_data.get_concrete_types(key);
                return self
                    .meta_data
                    .prototype
                    .concretize_prototype(&concrete_types.0);
            }
            None => return PrototypeConcretizationResult::UnConcretized(&self.meta_data.prototype),
        }
    }
}

impl AbstractConcreteTypesHandler for LambdaTypeData {
    fn register_concrete_types(&mut self, concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey {
        return self.meta_data.register_concrete_types(concrete_types);
    }

    fn has_generics(&self) -> bool {
        self.meta_data.has_generics()
    }
}
