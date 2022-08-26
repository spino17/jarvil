macro_rules! set_to_parent_scope {
    ($t: ident, $u: ident) => {
        let parent_scope = match &$u.$t.0.as_ref().borrow().parent_scope {
            Some(parent_scope) => parent_scope.clone(),
            None => unreachable!("attempt to close namespace should not be done at global level"),
        };
        $u.$t = parent_scope;
    };
}