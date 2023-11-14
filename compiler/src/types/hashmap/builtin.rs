use crate::scope::function::CallableData;
use std::{collections::HashMap, rc::Rc};

thread_local!(
    pub static HASHMAP_BUILTIN_METHODS: Rc<HashMap<&'static str, CallableData>> =
        Rc::new(HashMap::from([// TODO - the below type should be bounded by an interface `Hash`
            // ("get", CallableData::new(vec![get_unbounded_generic_type_with_declaration_index(0)], get_unbounded_generic_type_with_declaration_index(1), CallableKind::Method, Some((vec![0], true)), None)),
        ]))
);
