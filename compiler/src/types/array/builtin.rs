use crate::{
    scope::function::{CallableData, CallableKind},
    types::{core::Type, helper::get_unbounded_generic_type_with_declaration_index},
};
use std::{collections::HashMap, rc::Rc};

thread_local!(
    pub static ARRAY_BUILTIN_METHODS: Rc<HashMap<&'static str, CallableData>> =
       Rc::new(HashMap::from([
            // ("append", CallableData::new(vec![get_unbounded_generic_type_with_declaration_index(0)], Type::new_with_void(), CallableKind::Method, Some((vec![0], false)), None)),
        ]))
);
