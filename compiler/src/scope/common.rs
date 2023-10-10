use std::marker::PhantomData;

use super::{
    concrete::{ConcreteTypesTuple, ConcretizationContext},
    function::{CallableData, PartialConcreteCallableDataRef},
    interfaces::InterfaceData,
    types::core::UserDefinedTypeData,
    variables::VariableData,
};
use crate::types::core::AbstractType;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use text_size::TextRange;

#[derive(Debug)]
pub struct UniqueKeyGenerator<T> {
    state: usize,
    phanton: PhantomData<T>,
}

impl<T> UniqueKeyGenerator<T> {
    pub fn generate_unique_id(&mut self) -> usize {
        let id = self.state;
        self.state += 1;
        id
    }
}

impl<T> Default for UniqueKeyGenerator<T> {
    fn default() -> Self {
        UniqueKeyGenerator {
            state: 0,
            phanton: PhantomData,
        }
    }
}

#[derive(Debug, Default)]
pub struct GlobalUniqueKeyGenerator {
    variables: UniqueKeyGenerator<VariableData>,
    types: UniqueKeyGenerator<UserDefinedTypeData>,
    functions: UniqueKeyGenerator<CallableData>,
    interfaces: UniqueKeyGenerator<InterfaceData>,
}

impl GlobalUniqueKeyGenerator {
    pub fn generate_unique_id_for_variable(&mut self) -> usize {
        self.variables.generate_unique_id()
    }

    pub fn generate_unique_id_for_function(&mut self) -> usize {
        self.functions.generate_unique_id()
    }

    pub fn generate_unique_id_for_type(&mut self) -> usize {
        self.types.generate_unique_id()
    }

    pub fn generate_unique_id_for_interface(&mut self) -> usize {
        self.interfaces.generate_unique_id()
    }
}

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
                    Some((ty.concretize(context), *range))
                } else {
                    Some((ty.clone(), *range))
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
        global_concrete_types: Option<&'a ConcreteTypesTuple>,
    ) -> Option<(PartialConcreteCallableDataRef<'a>, TextRange)> {
        match self.methods.get(method_name) {
            Some((callable_data, range)) => {
                return Some((
                    PartialConcreteCallableDataRef::new(
                        callable_data,
                        match global_concrete_types {
                            Some(concrete_types) => Some(concrete_types),
                            None => None,
                        },
                    ),
                    *range,
                ))
            }
            None => None,
        }
    }
}
