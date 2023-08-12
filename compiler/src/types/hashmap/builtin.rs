use crate::{
    scope::function::{CallableData, CallableKind},
    types::helper::get_unbounded_generic_type_with_declaration_index,
};
use std::collections::HashMap;

thread_local!(
    pub static HASHMAP_BUILTIN_METHODS: &'static HashMap<&'static str, CallableData> =
        Box::leak(Box::new(HashMap::from([// TODO - the below type should be bounded by an interface `Hash`
            // ("get", CallableData::new(vec![get_unbounded_generic_type_with_declaration_index(0)], get_unbounded_generic_type_with_declaration_index(1), CallableKind::Method, Some((vec![0], true)), None)),
        ])))
);
