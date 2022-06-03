use crate::lexer::token::{CoreToken, TokenValue};
use crate::context;
use std::rc::Rc;

pub const KEYWORDS: [&'static str; 12] = [
    "for",
    "if",
    "elif",
    "else",
    "while",
    "struct",
    "and",
    "not",
    "or",
    "is",
    "true",
    "false",
];

pub const TYPES: [&'static str; 4] = [
    "int",
    "float",
    "string",
    "bool",
];

pub const LETTERS: [char; 53] = [
    'a',
    'b',
    'c',
    'd',
    'e',
    'f',
    'g',
    'h',
    'i',
    'j',
    'k',
    'l',
    'm',
    'n',
    'o',
    'p',
    'q',
    'r',
    's',
    't',
    'u',
    'v',
    'w',
    'x',
    'y',
    'z',
    'A',
    'B',
    'C',
    'D',
    'E',
    'F',
    'G',
    'H',
    'I',
    'J',
    'K',
    'L',
    'M',
    'N',
    'O',
    'P',
    'Q',
    'R',
    'S',
    'T',
    'U',
    'V',
    'W',
    'X',
    'Y',
    'Z',
    '_',
];

pub const DIGITS: [char; 10] = [
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
];

// everytime there is an addition in keyword, add an else if also here!
pub fn get_token_for_identifier(value: String) -> CoreToken {
    if context::is_keyword(&value) {
        if value.eq("for") {
            CoreToken::FOR
        } else if value.eq("if") {
            CoreToken::IF
        } else if value.eq("elif") {
            CoreToken::ELIF
        } else if value.eq("else") {
            CoreToken::ELSE
        } else if value.eq("while") {
            CoreToken::WHILE
        } else if value.eq("struct") {
            CoreToken::STRUCT
        } else if value.eq("and") {
            CoreToken::AND
        } else if value.eq("not") {
            CoreToken::NOT
        } else if value.eq("or") {
            CoreToken::OR
        } else if value.eq("is") {
            CoreToken::IS
        } else if value.eq("true") {
            CoreToken::TRUE
        } else if value.eq("false") {
            CoreToken::FALSE
        }
        else {
            unreachable!("keyword missing in the matching arms")
        }
    } else if context::is_type(&value) {
        CoreToken::TYPE(TokenValue(Rc::new(value)))
    } else {
        CoreToken::IDENTIFIER(TokenValue(Rc::new(value)))
    }
}