use super::function::{CallableData, CallableKind, CallablePrototypeData};
use crate::{constants::common::INT, types::core::Type};
use std::str::Chars;

// print(_obj: <any>)
pub fn print_meta_data() -> CallableData {
    CallableData {
        prototype: CallablePrototypeData {
            params: vec![Type::new_with_any()],
            return_type: Type::new_with_void(),
            is_concretization_required: None,
        },
        kind: CallableKind::Function,
        generics: Option::default(),
    }
}

// range(_start: int, _end: int) -> [int]
pub fn range_meta_data() -> CallableData {
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
