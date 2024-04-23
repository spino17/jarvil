use super::core::IdentDeclId;
use super::function::{CallableData, PartialConcreteCallableDataRef};
use super::interfaces::InterfaceData;
use super::variables::VariableData;
use crate::scope::concrete::TypeGenericsInstantiationContext;
use crate::scope::namespace::Namespace;
use crate::scope::symbol::types::core::UserDefinedTypeData;
use crate::types::core::Type;
use crate::{core::string_interner::StrId, types::traits::TypeLike};
use rustc_hash::FxHashMap;
use std::marker::PhantomData;
use text_size::TextRange;

#[derive(Debug, Default)]
pub struct FieldsMap {
    fields: FxHashMap<StrId, (Type, TextRange)>,
}

impl FieldsMap {
    pub fn new(fields: FxHashMap<StrId, (Type, TextRange)>) -> Self {
        FieldsMap { fields }
    }

    pub fn core_ref(&self) -> &FxHashMap<StrId, (Type, TextRange)> {
        &self.fields
    }

    pub fn try_field<'a>(
        &'a self,
        field_name: &StrId,
        namespace: &Namespace,
        context: TypeGenericsInstantiationContext,
    ) -> Option<(Type, TextRange)> {
        let Some((ty, range)) = self.fields.get(field_name) else {
            return None;
        };
        Some((ty.concretize(context, namespace), *range))
    }
}

#[derive(Debug, Default)]
pub struct MethodsMap {
    methods: FxHashMap<StrId, (CallableData, TextRange)>,
}

impl MethodsMap {
    pub fn new(methods: FxHashMap<StrId, (CallableData, TextRange)>) -> Self {
        MethodsMap { methods }
    }

    pub fn core_ref(&self) -> &FxHashMap<StrId, (CallableData, TextRange)> {
        &self.methods
    }

    pub fn try_method<'a>(
        &'a self,
        method_name: &StrId,
        context: TypeGenericsInstantiationContext<'a>,
    ) -> Option<(PartialConcreteCallableDataRef<'a>, TextRange)> {
        let Some((callable_data, range)) = self.methods.get(method_name) else {
            return None;
        };
        Some((
            PartialConcreteCallableDataRef::new(callable_data, context),
            *range,
        ))
    }

    pub fn has_method(&self, method_name: &StrId) -> bool {
        self.methods.get(method_name).is_some()
    }
}

#[derive(Debug)]
pub struct UniqueKeyGenerator<T> {
    state: usize,
    phanton: PhantomData<T>,
}

impl<T> UniqueKeyGenerator<T> {
    pub fn generate_unique_id(&mut self) -> IdentDeclId<T> {
        let id = self.state;
        self.state += 1;
        IdentDeclId::new(id)
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
    funcs: UniqueKeyGenerator<CallableData>,
    interfaces: UniqueKeyGenerator<InterfaceData>,
}

impl GlobalUniqueKeyGenerator {
    pub fn generate_unique_id_for_variable(&mut self) -> IdentDeclId<VariableData> {
        self.variables.generate_unique_id()
    }

    pub fn generate_unique_id_for_func(&mut self) -> IdentDeclId<CallableData> {
        self.funcs.generate_unique_id()
    }

    pub fn generate_unique_id_for_ty(&mut self) -> IdentDeclId<UserDefinedTypeData> {
        self.types.generate_unique_id()
    }

    pub fn generate_unique_id_for_interface(&mut self) -> IdentDeclId<InterfaceData> {
        self.interfaces.generate_unique_id()
    }
}
