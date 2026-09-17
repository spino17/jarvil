//! Built-in method dispatch for types that are not user-defined.
//!
//! `[T]`, `{K: V}` and `str` have methods, but no declaration site to hang them
//! on, so their prototypes live here. Collection methods are generic over the
//! receiver's element types and are concretized per call; `str` methods are not
//! generic and are stored as finished prototypes.
//!
//! See [`crate::builtin`] for why these mirror Python's method names exactly.

use super::traits::CollectionType;
use crate::core::string_interner::Interner;
use crate::scope::concrete::TypeGenericsInstantiationContext;
use crate::scope::namespace::Namespace;
use crate::types::array::core::Array;
use crate::types::atomic::builtin::str_builtin_methods;
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
        let callable_data = self.methods.get(method_name)?;

        let concrete_types = ty.concrete_types();
        let context = TypeGenericsInstantiationContext::new(Some(&concrete_types));

        match callable_data.concretized_prototype(namespace, context.into_method_context()) {
            RefOrOwned::Ref(_) => unreachable!(),
            RefOrOwned::Owned(prototype) => Some(prototype),
        }
    }
}

pub struct NonStructMethodsHandler {
    array_methods: CoreNonStructMethodsHandler<Array>,
    hashmap_methods: CoreNonStructMethodsHandler<HashMap>,
    // `str` is not a collection type: it has no element type, so its methods
    // need no concretization and are stored as finished prototypes rather than
    // as `CallableData` awaiting generic substitution.
    str_methods: FxHashMap<&'static str, CallablePrototypeData>,
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
            str_methods: str_builtin_methods(),
        }
    }

    pub fn try_method_for_str(&self, method_name: &str) -> Option<CallablePrototypeData> {
        self.str_methods.get(method_name).cloned()
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
