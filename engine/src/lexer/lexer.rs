use crate::lexer::token::Token;

// TODO - while lexical phase, when traversing through the string, keep account for \n so as to track the line number
// on which a specific token lies.

pub trait Lexer {
    fn scan(code: String) -> Iterator<Item=Token>;
}