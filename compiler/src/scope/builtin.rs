use super::function::{CallableData, CallableKind, CallablePrototypeData};
use crate::{constants::common::INT, types::core::Type};
use rustc_hash::FxHashMap;

// print<T>(obj: T)
fn print_callable_data() -> CallableData {
    CallableData {
        prototype: CallablePrototypeData {
            params: vec![Type::new_with_any()], // TODO - replace it with unbounded generic type `T`
            return_type: Type::new_with_void(),
            is_concretization_required: None,
        },
        kind: CallableKind::Function,
        generics: Option::default(),
    }
}

// range(start: int, end: int) -> [int]
fn range_callable_data() -> CallableData {
    CallableData {
        prototype: CallablePrototypeData {
            params: vec![Type::new_with_atomic(INT), Type::new_with_atomic(INT)],
            return_type: Type::new_with_array(Type::new_with_atomic(INT)),
            is_concretization_required: None,
        },
        kind: CallableKind::Function,
        generics: Option::default(),
    }
}

pub fn get_builtin_functions() -> FxHashMap<&'static str, CallableData> {
    let mut functions = FxHashMap::default();

    // all built-in functions
    functions.insert("print", print_callable_data());
    functions.insert("range", range_callable_data());

    functions
}
