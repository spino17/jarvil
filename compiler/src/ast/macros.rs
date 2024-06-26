macro_rules! default_errornous_node_impl {
    ($t: ident, $u: ident) => {
        impl ErrornousNode for $t {
            fn new_with_missing_tokens(
                expected_symbols: Vec<&'static str>,
                received_token: Token,
            ) -> Self {
                $t(Rc::new($u::MissingTokens(MissingTokenNode::new(
                    expected_symbols,
                    received_token,
                ))))
            }
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
