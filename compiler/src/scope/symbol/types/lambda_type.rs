use crate::scope::namespace::Namespace;
use crate::scope::symbol::types::generic_type::GenericTypeParams;
use crate::scope::traits::IsInitialized;
use crate::{
    core::common::RefOrOwned,
    scope::{
        concrete::ConcreteTypesTuple,
        symbol::function::{CallableData, CallableKind, CallablePrototypeData},
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
        global_concrete_types: Option<&ConcreteTypesTuple>,
        namespace: &Namespace,
    ) -> RefOrOwned<CallablePrototypeData> {
        match global_concrete_types {
            Some(concrete_types) => {
                return self.meta_data.prototype.concretize_prototype(
                    None,
                    Some(concrete_types),
                    namespace,
                );
            }
            None => return RefOrOwned::Ref(&self.meta_data.prototype),
        }
    }

    pub fn get_generic_type_decls(&self) -> &Option<GenericTypeParams> {
        &self.meta_data.generics
    }
}

impl IsInitialized for LambdaTypeData {
    fn is_initialized(&self) -> bool {
        unreachable!()
    }
}