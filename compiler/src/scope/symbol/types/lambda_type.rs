use crate::scope::concrete::TypeGenericsInstantiationContext;
use crate::scope::namespace::Namespace;
use crate::scope::symbol::types::generic_type::GenericTypeParams;
use crate::scope::traits::IsInitialized;
use crate::{
    core::common::RefOrOwned,
    scope::symbol::function::{CallableData, CallableKind, CallablePrototypeData},
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

    pub fn prototype(
        &self,
        namespace: &Namespace,
        context: &TypeGenericsInstantiationContext,
    ) -> RefOrOwned<CallablePrototypeData> {
        self.meta_data
            .concretized_prototype(namespace, &context.into_method_context())
    }

    pub fn generic_type_decls(&self) -> Option<&GenericTypeParams> {
        self.meta_data.generics()
    }
}

impl IsInitialized for LambdaTypeData {
    fn is_initialized(&self) -> bool {
        unreachable!()
    }
}
