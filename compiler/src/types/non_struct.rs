use super::traits::CollectionType;
use crate::core::string_interner::Interner;
use crate::scope::namespace::Namespace;
use crate::types::array::core::Array;
use crate::types::hashmap::core::HashMap;
use crate::{
    core::common::RefOrOwned,
    scope::symbol::function::{CallableData, CallablePrototypeData},
};
use rustc_hash::FxHashMap;
use std::marker::PhantomData;

pub struct CoreNonStructMethodsHandler<T: CollectionType> {
    methods: FxHashMap<&'static str, CallableData>,
    phanton: PhantomData<T>,
}

impl<T: CollectionType> CoreNonStructMethodsHandler<T> {
    fn try_method(
        &self,
        ty: &T,
        method_name: &str,
        namespace: &Namespace,
    ) -> Option<CallablePrototypeData> {
        let Some(callable_data) = self.methods.get(method_name) else {
            return None;
        };
        let concrete_types = ty.concrete_types();
        match callable_data.concretized_prototype(Some(&concrete_types), None, namespace) {
            RefOrOwned::Ref(_) => unreachable!(),
            RefOrOwned::Owned(prototype) => Some(prototype),
        }
    }
}

pub struct NonStructMethodsHandler {
    array_methods: CoreNonStructMethodsHandler<Array>,
    hashmap_methods: CoreNonStructMethodsHandler<HashMap>,
}

impl NonStructMethodsHandler {
    pub fn new(interner: &Interner) -> Self {
        NonStructMethodsHandler {
            array_methods: CoreNonStructMethodsHandler {
                methods: Array::builtin_methods(interner),
                phanton: PhantomData,
            },
            hashmap_methods: CoreNonStructMethodsHandler {
                methods: HashMap::builtin_methods(interner),
                phanton: PhantomData,
            },
        }
    }

    pub fn try_method_for_array(
        &self,
        ty: &Array,
        method_name: &str,
        namespace: &Namespace,
    ) -> Option<CallablePrototypeData> {
        self.array_methods.try_method(ty, method_name, namespace)
    }

    pub fn try_method_for_hashmap(
        &self,
        ty: &HashMap,
        method_name: &str,
        namespace: &Namespace,
    ) -> Option<CallablePrototypeData> {
        self.hashmap_methods.try_method(ty, method_name, namespace)
    }
}
