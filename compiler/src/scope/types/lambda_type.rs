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
        key: Option<ConcreteTypesRegistryKey>,
    ) -> PrototypeConcretizationResult {
        match key {
            Some(key) => {
                let concrete_types = self.meta_data.get_concrete_types(key);
                return self
                    .meta_data
                    .prototype
                    .concretize_prototype(&vec![], &concrete_types.0);
            }
            None => return PrototypeConcretizationResult::UnConcretized(&self.meta_data.prototype),
        }
    }

    pub fn get_generic_type_decls(&self) -> &Option<GenericTypeParams> {
        &self.meta_data.generics.generics_spec
    }
}

impl AbstractConcreteTypesHandler for LambdaTypeData {
    fn register_concrete_types(
        &mut self,
        concrete_types: Vec<Type>,
        has_generics: bool,
    ) -> ConcreteTypesRegistryKey {
        return self
            .meta_data
            .register_concrete_types(concrete_types, has_generics);
    }

    fn is_generics_present_in_tuple_at_index(&self, index: ConcreteTypesRegistryKey) -> bool {
        self.meta_data
            .generics
            .concrete_types_registry
            .get_concrete_types_at_key(index)
            .1
    }

    fn has_generics(&self) -> bool {
        self.meta_data.has_generics()
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
