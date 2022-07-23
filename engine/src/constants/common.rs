use crate::lexer::token::{CoreToken};
use crate::context;

pub const PLUS:                     &'static str = "+";                         
pub const DASH:                     &'static str = "-";
pub const RIGHT_ARROW:              &'static str = "->";
pub const STAR:                     &'static str = "*";
pub const DOUBLE_STAR:              &'static str = "**";
pub const SLASH:                    &'static str = "/";
pub const LPAREN:                   &'static str = "(";
pub const RPAREN:                   &'static str = ")";
pub const LBRACE:                   &'static str = "{";
pub const RBRACE:                   &'static str = "}";
pub const LSQUARE:                  &'static str = "[";
pub const RSQUARE:                  &'static str = "]";
pub const SEMICOLON:                &'static str = ";";
pub const COLON:                    &'static str = ":";
pub const DOUBLE_COLON:             &'static str = "::";
pub const COMMA:                    &'static str = ",";
pub const DOT:                      &'static str = ".";
pub const NEWLINE:                  &'static str = "newline";
pub const EQUAL:                    &'static str = "=";
pub const DOUBLE_EQUAL:             &'static str = "==";
pub const LBRACKET:                 &'static str = "<";
pub const RBRACKET:                 &'static str = ">";
pub const LESS_EQUAL:               &'static str = "<=";
pub const GREATER_EQUAL:            &'static str = ">=";
pub const NOT_EQUAL:                &'static str = "!=";
pub const FOR:                      &'static str = "for";
pub const WHILE:                    &'static str = "while";
pub const CONTINUE:                 &'static str = "continue";
pub const BREAK:                    &'static str = "break";
pub const IF:                       &'static str = "if";
pub const ELIF:                     &'static str = "elif";
pub const ELSE:                     &'static str = "else";
pub const TYPE_KEYWORD:             &'static str = "type";
pub const INTERFACE_KEYWORD:        &'static str = "interface";
pub const DEF:                      &'static str = "def";
pub const LET:                      &'static str = "let";
pub const SELF:                     &'static str = "self";
pub const IMPL:                     &'static str = "impl";
pub const AND:                      &'static str = "and";
pub const NOT:                      &'static str = "not";
pub const OR:                       &'static str = "or";
pub const IS:                       &'static str = "is";
pub const IN:                       &'static str = "in";
pub const NEW:                      &'static str = "new";
pub const TRUE:                     &'static str = "True";
pub const FALSE:                    &'static str = "False";
pub const FUNC:                     &'static str = "func";
pub const RETURN:                   &'static str = "return";
pub const INT:                      &'static str = "int";
pub const INTEGER:                  &'static str = "<integer>";
pub const FLOAT:                    &'static str = "float";
pub const FLOATING_POINT_NUMBER:    &'static str = "<floating point number>";
pub const STRING:                   &'static str = "string";
pub const BOOL:                     &'static str = "bool";
pub const LITERAL:                  &'static str = "<literal>";
pub const IDENTIFIER:               &'static str = "<identifier>";
pub const ATOMIC_TYPE:              &'static str = "<atomic type>";
pub const ENDMARKER:                &'static str = "<endmarker>";
pub const LEXICAL_ERROR:            &'static str = "<lexical error>";
pub const SINGLE_LINE_COMMENT:      &'static str = "<single line comment>";
pub const BLOCK_COMMENT:            &'static str = "<block comment>";
pub const BLANK:                    &'static str = "<blank>";

pub const KEYWORDS: [&'static str; 23] = [
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
    FUNC,
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
pub fn get_token_for_identifier(value: String) -> CoreToken {
    // TODO - try to keep in the same map
    if context::is_keyword(&value) {
        if value.eq(FOR)                        {
            CoreToken::FOR
        } else if value.eq(WHILE)               {
            CoreToken::WHILE
        } else if value.eq(CONTINUE)            {
            CoreToken::CONTINUE
        } else if value.eq(BREAK)               {
            CoreToken::BREAK
        } else if value.eq(IF)                  {
            CoreToken::IF
        } else if value.eq(ELIF)                {
            CoreToken::ELIF
        } else if value.eq(ELSE)                {
            CoreToken::ELSE
        }  else if value.eq(TYPE_KEYWORD)       {
            CoreToken::TYPE_KEYWORD
        } else if value.eq(INTERFACE_KEYWORD)   {
            CoreToken::INTERFACE_KEYWORD
        } else if value.eq(DEF)                 {
            CoreToken::DEF
        } else if value.eq(LET)                 {
            CoreToken::LET
        } else if value.eq(SELF)                {
            CoreToken::SELF
        } else if value.eq(IMPL)                {
            CoreToken::IMPL
        } else if value.eq(AND)                 {
            CoreToken::AND
        } else if value.eq(NOT)                 {
            CoreToken::NOT
        } else if value.eq(OR)                  {
            CoreToken::OR
        } else if value.eq(IS)                  {
            CoreToken::IS
        } else if value.eq(IN)                  {
            CoreToken::IN
        } else if value.eq(NEW)                 {
            CoreToken::NEW
        } else if value.eq(TRUE)                {
            CoreToken::TRUE
        } else if value.eq(FALSE)               {
            CoreToken::FALSE
        } else if value.eq(FUNC)                {
            CoreToken::FUNC
        } else if value.eq(RETURN)              {
            CoreToken::RETURN
        } else {
            unreachable!("keyword missing in the matching arms")
        }
    } else if context::is_type(&value) {
        CoreToken::ATOMIC_TYPE
    } else {
        CoreToken::IDENTIFIER
    }
}