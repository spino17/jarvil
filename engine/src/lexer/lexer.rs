use crate::lexer::token::Token;

pub trait Lexer {
    fn scan(code: String) -> Iterator<Item=Token>;
}