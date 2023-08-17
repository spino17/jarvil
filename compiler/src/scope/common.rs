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

    pub fn try_field<T: AbstractSymbolMetaData>(
        &self,
        field_name: &str,
        key: Option<ConcreteTypesRegistryKey>,
        registry_manager: &T,
    ) -> Option<(Type, TextRange)> {
        match self.fields.get(field_name) {
            Some((ty, range)) => {
                if ty.has_generics() {
                    match key {
                        Some(key) => {
                            let concrete_types = registry_manager.get_concrete_types(key);
                            return Some((
                                ty.concretize(&ConcretizationContext::new(
                                    Some(&concrete_types.0),
                                    None,
                                )),
                                *range,
                            ));
                        }
                        None => unreachable!(),
                    }
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

    pub fn try_method<'a>(
        &'a self,
        method_name: &str,
        key: Option<ConcreteTypesRegistryKey>,
        registry: &'a ConcreteTypesRegistryCore,
    ) -> Option<(PartialConcreteCallableDataRef<'a>, TextRange)> {
        match self.methods.get(method_name) {
            Some((callable_data, range)) => {
                return Some((
                    PartialConcreteCallableDataRef::get_from_registry_key(
                        callable_data,
                        registry,
                        key,
                    ),
                    *range,
                ))
            }
            None => None,
        }
    }
}
