use crate::lexer::token::{CoreToken, TokenValue};
use crate::context;
use std::rc::Rc;

const FOR:                  &'static str = "for";
const WHILE:                &'static str = "while";
const CONTINUE:             &'static str = "continue";
const BREAK:                &'static str = "break";
const IF:                   &'static str = "if";
const ELIF:                 &'static str = "elif";
const ELSE:                 &'static str = "else";
const TYPE_KEYWORD:         &'static str = "type";
const INTERFACE_KEYWORD:    &'static str = "interface";
const DEF:                  &'static str = "def";
const LET:                  &'static str = "let";
const SELF:                 &'static str = "self";
const IMPL:                 &'static str = "impl";
const AND:                  &'static str = "and";
const NOT:                  &'static str = "not";
const OR:                   &'static str = "or";
const IS:                   &'static str = "is";
const IN:                   &'static str = "in";
const NEW:                  &'static str = "new";
const TRUE:                 &'static str = "True";
const FALSE:                &'static str = "False";
const RETURN:               &'static str = "return";
pub const INT:              &'static str = "int";
pub const FLOAT:            &'static str = "float";
pub const STRING:           &'static str = "string";
pub const BOOL:             &'static str = "bool";

pub const KEYWORDS: [&'static str; 22] = [
    FOR,
    WHILE,
    CONTINUE,
    BREAK,
    IF,
    ELIF,
    ELSE,
    TYPE_KEYWORD,
    INTERFACE_KEYWORD,
    DEF,
    LET,
    SELF,
    IMPL,
    AND,
    NOT,
    OR,
    IS,
    IN,
    NEW,
    TRUE,
    FALSE,
    RETURN,
];

pub const TYPES: [&'static str; 4] = [
    INT,
    FLOAT,
    STRING,
    BOOL,
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
pub fn get_token_for_identifier(value: String) -> (CoreToken, String) {
    // TODO - try to keep in the same map
    if context::is_keyword(&value) {
        if value.eq(FOR)                        {
            (CoreToken::FOR,                String::from(FOR))
        } else if value.eq(WHILE)               {
            (CoreToken::WHILE,              String::from(WHILE))
        } else if value.eq(CONTINUE)            {
            (CoreToken::CONTINUE,           String::from(CONTINUE))
        } else if value.eq(BREAK)               {
            (CoreToken::BREAK,              String::from(BREAK))
        } else if value.eq(IF)                  {
            (CoreToken::IF,                 String::from(IF))
        } else if value.eq(ELIF)                {
            (CoreToken::ELIF,               String::from(ELIF))
        } else if value.eq(ELSE)                {
            (CoreToken::ELSE,               String::from(ELSE))
        }  else if value.eq(TYPE_KEYWORD)       {
            (CoreToken::TYPE_KEYWORD,       String::from(TYPE_KEYWORD))
        } else if value.eq(INTERFACE_KEYWORD)   {
            (CoreToken::INTERFACE_KEYWORD,  String::from(INTERFACE_KEYWORD))
        } else if value.eq(DEF)                 {
            (CoreToken::DEF,                String::from(DEF))
        } else if value.eq(LET)                 {
            (CoreToken::LET,                String::from(LET))
        } else if value.eq(SELF)                 {
            (CoreToken::SELF,               String::from(SELF))
        } else if value.eq(IMPL)                 {
            (CoreToken::IMPL,               String::from(IMPL))
        } else if value.eq(AND)                 {
            (CoreToken::AND,                String::from(AND))
        } else if value.eq(NOT)                 {
            (CoreToken::NOT,                String::from(NOT))
        } else if value.eq(OR)                  {
            (CoreToken::OR,                 String::from(OR))
        } else if value.eq(IS)                  {
            (CoreToken::IS,                 String::from(IS))
        } else if value.eq(IN)                  {
            (CoreToken::IN,                 String::from(IN))
        } else if value.eq(NEW)                 {
            (CoreToken::NEW,                String::from(NEW))
        } else if value.eq(TRUE)                {
            (CoreToken::TRUE,               String::from(TRUE))
        } else if value.eq(FALSE)               {
            (CoreToken::FALSE,              String::from(FALSE))
        } else if value.eq(RETURN)              {
            (CoreToken::RETURN,             String::from(RETURN))
        } else {
            unreachable!("keyword missing in the matching arms")
        }
    } else if context::is_type(&value) {
        (CoreToken::TYPE(TokenValue(Rc::new(value))), String::from("type"))  // TODO - add value also in name
    } else {
        (CoreToken::IDENTIFIER(TokenValue(Rc::new(value))), String::from("identifier"))  // TODO - add value also in name
    }
}