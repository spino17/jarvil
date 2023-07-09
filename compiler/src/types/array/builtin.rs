use crate::{
    scope::function::{CallableData, CallableKind},
    types::{core::Type, helper::get_unbounded_generic_type_with_declaration_index},
};
use std::collections::HashMap;

thread_local!(
    pub static ARRAY_BUILTIN_METHODS: &'static HashMap<&'static str, CallableData> =
        Box::leak(Box::new(HashMap::from([
            ("append", CallableData::new(vec![get_unbounded_generic_type_with_declaration_index(0)], Type::new_with_void(), CallableKind::Method, true, None)),
        ])))
);
