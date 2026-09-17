// Methods on `{K: V}`, each mapping to a Python `dict` method of the same name.
//
// `K` and `V` are the type-level generics at declaration indices 0 and 1; the
// non-struct method handler concretizes them against the receiver before the
// call is checked.

use super::core::HashMap;
use crate::core::string_interner::Interner;
use crate::scope::symbol::function::{CallableData, CallableKind};
use crate::types::core::Type;
use crate::types::helper::unbounded_generic_ty_in_ty_with_decl_index;
use rustc_hash::FxHashMap;

fn method(params: Vec<Type>, return_ty: Type) -> CallableData {
    CallableData::new(params, return_ty, CallableKind::Method, None)
}

impl HashMap {
    pub fn builtin_methods(interner: &Interner) -> FxHashMap<&'static str, CallableData> {
        let mut methods = FxHashMap::default();

        let key = || unbounded_generic_ty_in_ty_with_decl_index(0, interner);
        let value = || unbounded_generic_ty_in_ty_with_decl_index(1, interner);
        let void = Type::new_with_void;

        // pop(key: K) -> V
        //
        // Raises when the key is absent, as `d[key]` already does.
        methods.insert("pop", method(vec![key()], value()));

        // update(other: {K: V})
        methods.insert(
            "update",
            method(vec![Type::new_with_hashmap(key(), value())], void()),
        );

        // clear()
        methods.insert("clear", method(vec![], void()));

        methods
    }
}

// Deliberately absent:
//
// `keys`, `values`, `items` -- Python returns *views*, not lists. Typing them
//     as `[K]` would type-check `d.keys()[0]` and then fail at runtime, which
//     is precisely the class of bug this language exists to prevent. Iterating
//     a hashmap directly (`for k in d`) already yields its keys, so nothing is
//     actually out of reach. Adding them honestly needs either an iterator type
//     or code generation that wraps the call in `list(..)`.
//
// `get` -- returns `None` for a missing key, and there is no way to say that in
//     a return type yet. `pop` raises instead, which is at least consistent
//     with indexing.
