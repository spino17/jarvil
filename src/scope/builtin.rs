use super::function::FunctionData;
use crate::{constants::common::INT, types::core::Type};
use std::{rc::Rc, str::Chars};

// print(_obj: <any>)
pub fn print_meta_data() -> FunctionData {
    FunctionData {
        params: vec![Type::new_with_any()],
        return_type: Type::new_with_void(),
    }
}

// range(_start: int, _end: int) -> [int]
pub fn range_meta_data() -> FunctionData {
    FunctionData {
        params: vec![Type::new_with_atomic(INT), Type::new_with_atomic(INT)],
        return_type: Type::new_with_array(&Type::new_with_atomic(INT)),
    }
}

fn check_in_builtin_func_trie(remaining_str: &str, value: Chars) -> bool {
    let value: String = value.collect();
    if value.len() == remaining_str.len() && value.eq(remaining_str) {
        true
    } else {
        false
    }
}

pub fn is_name_in_builtin_func(name: &str) -> bool {
    let mut iter = name.chars();
    match iter.next() {
        Some(c) => match c {
            'p' => check_in_builtin_func_trie("rint", iter),
            'r' => check_in_builtin_func_trie("ange", iter),
            _ => return false,
        },
        None => unreachable!(),
    }
}
