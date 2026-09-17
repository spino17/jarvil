//! Methods on `str`.
//!
//! Unlike `[T]` and `{K: V}` these involve no generics, so there is no
//! concretization step and the prototypes can be built once.
//!
//! Every method here maps to a Python `str` method of the same name, arity and
//! return type, because code generation emits the method name verbatim. Methods
//! whose Python counterpart returns something Jarvil's type system cannot
//! honestly describe are left out rather than mistyped -- see the note at the
//! bottom.

use crate::constants::common::{BOOL, INT, STRING};
use crate::scope::symbol::function::CallablePrototypeData;
use crate::types::core::Type;
use rustc_hash::FxHashMap;

fn method(params: Vec<Type>, return_ty: Type) -> CallablePrototypeData {
    CallablePrototypeData::new(params, return_ty)
}

pub fn str_builtin_methods() -> FxHashMap<&'static str, CallablePrototypeData> {
    let mut methods = FxHashMap::default();

    let string = || Type::new_with_atomic(STRING);
    let int = || Type::new_with_atomic(INT);
    let boolean = || Type::new_with_atomic(BOOL);
    let str_array = || Type::new_with_array(Type::new_with_atomic(STRING));

    // case
    methods.insert("upper", method(vec![], string()));
    methods.insert("lower", method(vec![], string()));
    methods.insert("capitalize", method(vec![], string()));
    methods.insert("title", method(vec![], string()));

    // trimming
    methods.insert("strip", method(vec![], string()));
    methods.insert("lstrip", method(vec![], string()));
    methods.insert("rstrip", method(vec![], string()));

    // searching and testing
    methods.insert("startswith", method(vec![string()], boolean()));
    methods.insert("endswith", method(vec![string()], boolean()));
    // `find` returns -1 when absent, matching Python; `index` raises, so it is
    // left out until there is a way to express that
    methods.insert("find", method(vec![string()], int()));
    methods.insert("count", method(vec![string()], int()));
    methods.insert("isdigit", method(vec![], boolean()));
    methods.insert("isalpha", method(vec![], boolean()));
    methods.insert("isspace", method(vec![], boolean()));
    methods.insert("islower", method(vec![], boolean()));
    methods.insert("isupper", method(vec![], boolean()));

    // transformation
    methods.insert("replace", method(vec![string(), string()], string()));
    methods.insert("split", method(vec![string()], str_array()));
    methods.insert("join", method(vec![str_array()], string()));

    methods
}

// Deliberately absent:
//
// `index`, `rindex`   -- raise `ValueError` when the substring is missing, and
//                        Jarvil has no way to say a method can fail
// `format`, `encode`  -- variadic or return types with no Jarvil equivalent
// `splitlines`        -- fine in principle, but `split` covers the need and
//                        every added surface is another thing to keep honest
