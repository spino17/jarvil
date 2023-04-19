pub const PLUS: &'static str                    = "+";
pub const DASH: &'static str                    = "-";
pub const RIGHT_ARROW: &'static str             = "->";
pub const STAR: &'static str                    = "*";
pub const DOUBLE_STAR: &'static str             = "**";
pub const SLASH: &'static str                   = "/";
pub const LPAREN: &'static str                  = "(";
pub const RPAREN: &'static str                  = ")";
pub const LBRACE: &'static str                  = "{";
pub const RBRACE: &'static str                  = "}";
pub const LSQUARE: &'static str                 = "[";
pub const RSQUARE: &'static str                 = "]";
pub const SEMICOLON: &'static str               = ";";
pub const COLON: &'static str                   = ":";
pub const DOUBLE_COLON: &'static str            = "::";
pub const COMMA: &'static str                   = ",";
pub const DOT: &'static str                     = ".";
pub const NEWLINE: &'static str                 = "newline";
pub const EQUAL: &'static str                   = "=";
pub const DOUBLE_EQUAL: &'static str            = "==";
pub const LBRACKET: &'static str                = "<";
pub const RBRACKET: &'static str                = ">";
pub const LESS_EQUAL: &'static str              = "<=";
pub const GREATER_EQUAL: &'static str           = ">=";
pub const NOT_EQUAL: &'static str               = "!=";
pub const FOR: &'static str                     = "for";
pub const WHILE: &'static str                   = "while";
pub const CONTINUE: &'static str                = "continue";
pub const BREAK: &'static str                   = "break";
pub const IF: &'static str                      = "if";
pub const ELIF: &'static str                    = "elif";
pub const ELSE: &'static str                    = "else";
pub const TYPE_KEYWORD: &'static str            = "type";
pub const INTERFACE_KEYWORD: &'static str       = "interface";
pub const DEF: &'static str                     = "def";
pub const LET: &'static str                     = "let";
pub const SELF: &'static str                    = "self";
pub const IMPL: &'static str                    = "impl";
pub const AND: &'static str                     = "and";
pub const NOT: &'static str                     = "not";
pub const OR: &'static str                      = "or";
pub const IN: &'static str                      = "in";
pub const TRUE: &'static str                    = "True";
pub const FALSE: &'static str                   = "False";
pub const LAMBDA_KEYWORD: &'static str          = "lambda";
pub const RETURN: &'static str                  = "return";
pub const INT: &'static str                     = "int";
pub const INTEGER: &'static str                 = "<integer>";
pub const FLOAT: &'static str                   = "float";
pub const FLOATING_POINT_NUMBER: &'static str   = "<floating-point-number>";
pub const STRING: &'static str                  = "string";
pub const BOOL: &'static str                    = "bool";
pub const LITERAL: &'static str                 = "<literal>";
pub const IDENTIFIER: &'static str              = "<identifier>";
pub const ATOMIC_TYPE: &'static str             = "<atomic-type>";
pub const ENDMARKER: &'static str               = "<endmarker>";
pub const LEXICAL_ERROR: &'static str           = "<lexical-error>";
pub const SINGLE_LINE_COMMENT: &'static str     = "<single-line-comment>";
pub const BLOCK_COMMENT: &'static str           = "<block-comment>";
pub const BLANK: &'static str                   = "<blank>";
pub const NON_TYPED: &'static str               = "<non-typed>";
pub const UNKNOWN: &'static str                 = "<unknown>";

// KEYWORDS + ATOMIC_TYPES => RESERVED_WORDS
pub const KEYWORDS: [&'static str; 21] = [
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
    IN,
    TRUE,
    FALSE,
    LAMBDA_KEYWORD,
    RETURN,
];

pub const ATOMIC_TYPES: [&'static str; 4] = [INT, FLOAT, STRING, BOOL];

pub const EIGHT_BIT_MAX_VALUE: usize = 250;