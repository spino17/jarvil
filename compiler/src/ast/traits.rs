use crate::lexer::token::Token;
use text_size::TextRange;

pub trait Node {
    fn range(&self) -> TextRange;
    fn start_line_number(&self) -> usize;
}

pub trait ErrornousNode {
    fn new_with_missing_tokens(expected_symbols: Vec<&'static str>, received_token: Token) -> Self;
}
