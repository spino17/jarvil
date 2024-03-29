use crate::{
    constants::common::INT,
    core::string_interner::Interner,
    scope::symbol::{
        function::{CallableData, CallableKind},
        interfaces::InterfaceBounds,
        types::generic_ty::GenericTypeParams,
    },
    types::{core::Type, helper::unbounded_generic_ty_in_func_with_declaration_index},
};
use rustc_hash::FxHashMap;
use text_size::TextRange;

// print<V>(obj: V)
fn print_callable_data(interner: &Interner) -> CallableData {
    CallableData::new(
        vec![unbounded_generic_ty_in_func_with_declaration_index(
            0, interner,
        )],
        Type::new_with_void(),
        CallableKind::Function,
        Some(GenericTypeParams::new(vec![(
            interner.intern("V"),
            InterfaceBounds::default(),
            TextRange::default(),
        )])),
    )
}

// range(start: int, end: int) -> [int]
fn range_callable_data() -> CallableData {
    CallableData::new(
        vec![Type::new_with_atomic(INT), Type::new_with_atomic(INT)],
        Type::new_with_array(Type::new_with_atomic(INT)),
        CallableKind::Function,
        None,
    )
}

pub fn builtin_functions(interner: &Interner) -> FxHashMap<&'static str, CallableData> {
    let mut functions = FxHashMap::default();

    // all built-in functions
    functions.insert("print", print_callable_data(interner));
    functions.insert("range", range_callable_data());

    functions
}
