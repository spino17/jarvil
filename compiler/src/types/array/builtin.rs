// Methods on `[T]`, each mapping to a Python `list` method of the same name.
//
// `T` is written as the type-level generic at declaration index 0; the
// non-struct method handler concretizes it against the receiver's element type
// before the call is checked.

use crate::core::string_interner::Interner;
use crate::types::array::core::Array;
use crate::{
    constants::common::INT,
    scope::symbol::function::{CallableData, CallableKind},
    types::{core::Type, helper::unbounded_generic_ty_in_ty_with_decl_index},
};
use rustc_hash::FxHashMap;

fn method(params: Vec<Type>, return_ty: Type) -> CallableData {
    CallableData::new(params, return_ty, CallableKind::Method, None)
}

impl Array {
    pub fn builtin_methods(interner: &Interner) -> FxHashMap<&'static str, CallableData> {
        let mut methods = FxHashMap::default();

        let element = || unbounded_generic_ty_in_ty_with_decl_index(0, interner);
        let int = || Type::new_with_atomic(INT);
        let void = Type::new_with_void;

        // append(x: T)
        methods.insert("append", method(vec![element()], void()));

        // extend(other: [T])
        methods.insert(
            "extend",
            method(vec![Type::new_with_array(element())], void()),
        );

        // insert(index: int, x: T)
        methods.insert("insert", method(vec![int(), element()], void()));

        // pop() -> T
        //
        // Raises on an empty list, exactly as indexing past the end already
        // does. Jarvil cannot express fallibility yet, and excluding `pop`
        // while permitting `a[0]` would be an inconsistent line to draw.
        methods.insert("pop", method(vec![], element()));

        // count(x: T) -> int
        methods.insert("count", method(vec![element()], int()));

        // reverse()
        methods.insert("reverse", method(vec![], void()));

        // sort()
        methods.insert("sort", method(vec![], void()));

        // clear()
        methods.insert("clear", method(vec![], void()));

        methods
    }
}

// Deliberately absent:
//
// `remove`, `index` -- raise `ValueError` when the element is not present.
//                      Unlike `pop`, that failure depends on the *contents*
//                      rather than the size, which is a sharper edge than the
//                      language can currently warn about.
// `copy`            -- would need to return `[T]` by value; correct, but adds
//                      surface without adding capability while `extend` exists.
