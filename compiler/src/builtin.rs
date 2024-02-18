use crate::{
    constants::common::INT,
    scope::symbol::function::{CallableData, CallableKind},
    types::core::Type,
};
use rustc_hash::FxHashMap;

// print<T>(obj: T)
fn print_callable_data() -> CallableData {
    CallableData::new(
        vec![Type::new_with_any()],
        Type::new_with_void(),
        CallableKind::Function,
        None,
        None,
    )
}

// range(start: int, end: int) -> [int]
fn range_callable_data() -> CallableData {
    CallableData::new(
        vec![Type::new_with_atomic(INT), Type::new_with_atomic(INT)],
        Type::new_with_array(Type::new_with_atomic(INT)),
        CallableKind::Function,
        None,
        None,
    )
}

pub fn get_builtin_functions() -> FxHashMap<&'static str, CallableData> {
    let mut functions = FxHashMap::default();

    // all built-in functions
    functions.insert("print", print_callable_data());
    functions.insert("range", range_callable_data());

    functions
}
