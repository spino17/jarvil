use crate::lexer::token::{CoreToken, TokenValue};
use crate::context;
use std::rc::Rc;

const FOR:          &'static str = "for";
const WHILE:        &'static str = "while";
const CONTINUE:     &'static str = "continue";
const BREAK:        &'static str = "break";
const IF:           &'static str = "if";
const ELIF:         &'static str = "elif";
const ELSE:         &'static str = "else";
const STRUCT:       &'static str = "struct";
const DEF:          &'static str = "def";
const AND:          &'static str = "and";
const NOT:          &'static str = "not";
const OR:           &'static str = "or";
const IS:           &'static str = "is";
const IN:           &'static str = "in";
const TRUE:         &'static str = "True";
const FALSE:        &'static str = "False";

pub const KEYWORDS: [&'static str; 16] = [
    FOR,
    WHILE,
    CONTINUE,
    BREAK,
    IF,
    ELIF,
    ELSE,
    STRUCT,
    DEF,
    AND,
    NOT,
    OR,
    IS,
    IN,
    TRUE,
    FALSE,
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

// everytime there is an addition in keyword, add here also!
pub fn get_token_for_identifier(value: String) -> CoreToken {
    if context::is_keyword(&value) {
        if value.eq(FOR) {
            CoreToken::FOR
        } else if value.eq(WHILE) {
            CoreToken::WHILE
        } else if value.eq(CONTINUE) {
            CoreToken::CONTINUE
        } else if value.eq(BREAK) {
            CoreToken::BREAK
        } else if value.eq(IF) {
            CoreToken::IF
        } else if value.eq(ELIF) {
            CoreToken::ELIF
        } else if value.eq(ELSE) {
            CoreToken::ELSE
        }  else if value.eq(STRUCT) {
            CoreToken::STRUCT
        } else if value.eq(DEF) {
            CoreToken::DEF
        } else if value.eq(AND) {
            CoreToken::AND
        } else if value.eq(NOT) {
            CoreToken::NOT
        } else if value.eq(OR) {
            CoreToken::OR
        } else if value.eq(IS) {
            CoreToken::IS
        } else if value.eq(IN) {
            CoreToken::IN
        } else if value.eq(TRUE) {
            CoreToken::TRUE
        } else if value.eq(FALSE) {
            CoreToken::FALSE
        } else {
            unreachable!("keyword missing in the matching arms")
        }
    } else if context::is_type(&value) {
        CoreToken::TYPE(TokenValue(Rc::new(value)))
    } else {
        CoreToken::IDENTIFIER(TokenValue(Rc::new(value)))
    }
}