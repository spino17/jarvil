macro_rules! default_node_impl {
    ($t: ident) => {
        impl Node for $t {
            fn set_parent(&self, parent_node: WeakASTNode) {
                self.core_ref_mut().parent = Some(parent_node);
            }
        }
    };
}

macro_rules! core_node_access {
    ($t: ident) => {
        pub fn core_ref(&self) -> Ref<$t> {
            self.0.as_ref().borrow()
        }

        pub fn core_ref_mut(&self) -> RefMut<$t> {
            self.0.as_ref().borrow_mut()
        }
    };
}

macro_rules! default_errornous_node_impl {
    ($t: ident, $u: ident, $v: ident) => {
        impl ErrornousNode for $t {
            fn new_with_missing_tokens(
                expected_symbols: &Rc<Vec<&'static str>>,
                received_token: &Token,
                lookahead: usize,
            ) -> Self {
                $t(Rc::new(RefCell::new($u {
                    kind: $v::MISSING_TOKENS(MissingTokenNode::new(
                        expected_symbols,
                        received_token,
                        lookahead,
                    )),
                    parent: None,
                })))
            }
        }
    };
}

macro_rules! set_parent {
    ($t: ident, $u: ident, $v: ident) => {
        $t.set_parent(WeakASTNode::$u(Rc::downgrade(&$v)));
    };
}

macro_rules! set_parents {
    (($($t: ident),*), $u: ident, $v: ident) => {
        $(
            $t.set_parent(WeakASTNode::$u(Rc::downgrade(&$v)));
        )*
    };
}

macro_rules! set_parents_optional {
    (($($t: ident),*), $u: ident, $v: ident) => {
        $(
            match $t {
                Some($t) => {
                    set_parent!($t, $u, $v);
                }
                None => {}
            }
        )*
    };
}

macro_rules! extract_from_option {
    ($t: ident) => {
        match $t {
            Some(val) => val.clone(),
            None => None,
        }
    };
}

macro_rules! impl_weak_node {
    ($t: ident, $v: ident) => {
        #[derive(Debug, Clone)]
        pub struct $t(Weak<RefCell<$v>>);
    };
}

macro_rules! weak_ast_nodes {
    ($(($t: ident, $v: ident)),*) => {
        #[derive(Debug, Clone)]
        pub enum WeakASTNode {
            $(
                $t($v),
            )*
        }
    };
}