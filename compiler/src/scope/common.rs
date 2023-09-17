use super::{
    concrete::{
        core::{ConcreteTypesRegistryKey, ConcretizationContext},
        registry::ConcreteTypesRegistryCore,
    },
    core::AbstractSymbolMetaData,
    function::{CallableData, PartialConcreteCallableDataRef},
};
use crate::types::core::AbstractType;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Debug, Default)]
pub struct FieldsMap {
    fields: FxHashMap<String, (Type, TextRange)>,
}

impl FieldsMap {
    pub fn new(fields: FxHashMap<String, (Type, TextRange)>) -> Self {
        FieldsMap { fields }
    }

    pub fn try_field(
        &self,
        field_name: &str,
        context: &ConcretizationContext,
    ) -> Option<(Type, TextRange)> {
        match self.fields.get(field_name) {
            Some((ty, range)) => {
                if ty.is_concretization_required() {
                    return Some((ty.concretize(context), *range));
                } else {
                    return Some((ty.clone(), *range));
                }
            }
            None => None,
        }
    }
}

#[derive(Debug, Default)]
pub struct MethodsMap {
    methods: FxHashMap<String, (CallableData, TextRange)>,
}

impl MethodsMap {
    pub fn new(methods: FxHashMap<String, (CallableData, TextRange)>) -> Self {
        MethodsMap { methods }
    }

    pub fn get_methods_ref(&self) -> &FxHashMap<String, (CallableData, TextRange)> {
        &self.methods
    }

    pub fn try_method<'a>(
        &'a self,
        method_name: &str,
        global_concrete_types: Option<&'a Vec<Type>>,
    ) -> Option<(PartialConcreteCallableDataRef<'a>, TextRange)> {
        match self.methods.get(method_name) {
            Some((callable_data, range)) => {
                return Some((
                    PartialConcreteCallableDataRef::get_from_registry_key(
                        callable_data,
                        global_concrete_types,
                    ),
                    *range,
                ))
            }
            None => None,
        }
    }
}
