macro_rules! impl_symbol_check {
    ($t: ident) => {
        fn $t(&self) -> bool {
            match self {
                CoreToken::$t => true,
                _ => false,
            }
        }
    };
}

macro_rules! impl_symbols_check {
    ($($t: ident),*) => {
        $(
            impl_symbol_check!($t);
        )*
    };
}

macro_rules! impl_symbols_is_eq {
    ($($t: ident),*) => {
        pub fn is_eq(&self, symbol: &str) -> bool {
            match symbol {
                $(
                    $t => self.$t(),
                )*
                "\n" => self.NEWLINE(),
                _ => unreachable!("token `{}` missing from matching arm", symbol),
            }
        }
    };
}

macro_rules! impl_token_to_string {
    ($($t: ident),*) => {
        pub fn to_string(&self) -> &'static str {
            let symbol_str = match self {
                $(
                    CoreToken::$t => $t,
                )*
                CoreToken::LEXICAL_ERROR(_) => LEXICAL_ERROR,
            };
            symbol_str
        }
    };
}