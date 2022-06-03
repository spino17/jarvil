use crate::lexer::token::CoreToken;

pub const KEYWORDS: [&'static str; 10] = [
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
];

pub const TYPES: [&'static str; 3] = [
    "Int",
    "String",
    "Bool",
];

pub const LETTERS: [char; 27] = [
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