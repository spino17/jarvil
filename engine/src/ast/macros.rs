macro_rules! default_errornous_node_impl {
    ($t: ident, $u: ident) => {
        impl ErrornousNode for $t {
            fn new_with_missing_tokens(
                expected_symbols: &Rc<Vec<&'static str>>,
                received_token: &Token,
            ) -> Self {
                $t(Rc::new($u::MISSING_TOKENS(MissingTokenNode::new(
                    expected_symbols,
                    received_token,
                ))))
            }
        }
    };
}

macro_rules! impl_weak_node {
    ($(($t: ident, $v: ident)),*) => {
        $(
            #[derive(Debug, Clone)]
            pub struct $t(Weak<$v>);
        )*
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

macro_rules! impl_ast_node {
    ($(($t: ident, $u: ident, $v: ident)),*) => {
        $(
            pub fn $v(x: &$u) -> Self {
                ASTNode::$t(x.clone())
            }
        )*
    };
}

macro_rules! extract_from_option {
    ($t: ident) => {
        match $t {
            Some(val) => Some(val.clone()),
            None => None,
        }
    };
}

macro_rules! impl_range {
    ($t: expr, $u: expr) => {
        TextRange::new(
            TextSize::from($t.range().start()),
            TextSize::from($u.range().end()),
        )
    };
}

macro_rules! impl_node {
    ($t: expr, $u: expr) => {
        fn range(&self) -> TextRange {
            impl_range!($t, $u)
        }
        fn start_line_number(&self) -> usize {
            $t.start_line_number()
        }
    };
}

macro_rules! impl_enum_variant {
    ($v: expr, $s: ident, $(($t: ident, $u: ident)),*) => {
        match $v {
            $(
                $t::$u(x) => {
                    x.$s()
                }
            )*,
        }
    };
}

macro_rules! impl_node_variant_for_range {
    ($v: expr, $(($t: ident, $u: ident)),*) => {
        match $v {
            $(
                $t::$u(x) => {
                    impl_range!(x, x)
                }
            )*,
        }
    };
}

macro_rules! impl_core_ref {
    ($t: ident) => {
        pub fn core_ref(&self) -> &$t {
            self.0.as_ref()
        }
    };
}

macro_rules! impl_node_walk {
    ($t: ident, $u: ident, $v: ident) => {
        fn $t(&mut self, x: &$u) {
            self.walk(&ASTNode::$v(x));
        }
    };
}

macro_rules! impl_manual_walk_block {
    ($t: expr) => {
        let code_block = $t.0.as_ref().borrow();
        for stmt in &code_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
    };
}
