use crate::{
    scope::{
        concrete::core::ConcreteTypesRegistryKey,
        core::{AbstractConcreteTypesHandler, GenericTypeParams, SymbolData},
        function::{CallableData, CallableKind, PrototypeConcretizationResult},
        handler::SymbolDataRegistryTable,
    },
    types::core::Type,
};

use super::core::UserDefinedTypeData;

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
        symbol_data: &SymbolData<UserDefinedTypeData>,
        registry: &mut SymbolDataRegistryTable<UserDefinedTypeData>,
    ) -> PrototypeConcretizationResult {
        match key {
            Some(key) => {
                let concrete_types = registry.get_concrete_types(symbol_data, key);
                return self.meta_data.prototype.concretize_prototype(
                    None,
                    Some(&concrete_types.0),
                    registry,
                );
            }
            None => return PrototypeConcretizationResult::UnConcretized(&self.meta_data.prototype),
        }
    }

    pub fn get_generic_type_decls(&self) -> &Option<GenericTypeParams> {
        &self.meta_data.generics.generics_spec
    }
}

impl AbstractConcreteTypesHandler for LambdaTypeData {
    fn is_initialized(&self) -> bool {
        unreachable!()
    }
}
