use crate::{
    scope::{
        concrete::core::{ConcreteTypesRegistryKey, ConcreteTypesTuple},
        core::{AbstractConcreteTypesHandler, AbstractSymbolMetaData, GenericTypeParams},
        function::{CallableData, CallableKind, PrototypeConcretizationResult},
    },
    types::core::Type,
};

#[derive(Debug)]
pub struct LambdaTypeData {
    meta_data: CallableData,
}

impl LambdaTypeData {
    pub fn new(
        param_types: Vec<Type>,
        return_type: Type,
        is_concretization_required: Option<(Vec<usize>, bool)>,
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

    pub fn get_concrete_prototype(
        &self,
        global_concrete_types: Option<&Vec<Type>>,
    ) -> PrototypeConcretizationResult {
        match global_concrete_types {
            Some(concrete_types) => {
                return self
                    .meta_data
                    .prototype
                    .concretize_prototype(None, Some(concrete_types));
            }
            None => return PrototypeConcretizationResult::UnConcretized(&self.meta_data.prototype),
        }
    }

    pub fn get_generic_type_decls(&self) -> &Option<GenericTypeParams> {
        &self.meta_data.generics.generics_spec
    }
}

impl AbstractConcreteTypesHandler for LambdaTypeData {
    fn register_concrete_types(&mut self, concrete_types: Vec<Type>) -> ConcreteTypesRegistryKey {
        return self.meta_data.register_concrete_types(concrete_types);
    }

    fn is_initialized(&self) -> bool {
        unreachable!()
    }
}

impl AbstractSymbolMetaData for LambdaTypeData {
    fn get_concrete_types(&self, key: ConcreteTypesRegistryKey) -> &ConcreteTypesTuple {
        return self.meta_data.get_concrete_types(key);
    }
}
