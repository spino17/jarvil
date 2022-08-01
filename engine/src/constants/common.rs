use crate::context;
use crate::lexer::token::CoreToken;

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
pub const IN:                       &'static str = "in";
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
pub const NON_TYPED:                &'static str = "<non-typed>";

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
    FUNC,
    RETURN,
];

pub const TYPES: [&'static str; 4] = [INT, FLOAT, STRING, BOOL];

// This method is taken from the amazing book `Crafting Interpreters` by `Bob Nystrom`
fn check_keyword(start_index: usize, remaining_str: &str, value: std::slice::Iter<char>, token_type: CoreToken) -> CoreToken {
    let value: String = value.collect();
    if value.len() == remaining_str.len() && value.eq(remaining_str) {
        token_type
    } else {
        CoreToken::IDENTIFIER
    }
}

// Trie implementation for efficient reserved words matching
pub fn token_for_identifier(mut value_iter: std::slice::Iter<char>) -> CoreToken {
    match value_iter.next() {
        Some(c) => {
            match c {
                'f' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => {
                            match next_c {
                                'o' => check_keyword(2, "r", value_iter, CoreToken::FOR),
                                'u' => check_keyword(2, "nc", value_iter, CoreToken::FUNC),
                                'l' => check_keyword(
                                    2, "oat", value_iter, CoreToken::ATOMIC_TYPE
                                ),
                                _ => return CoreToken::IDENTIFIER
                            }
                        },
                        None => return CoreToken::IDENTIFIER
                    }
                }, // for, func, float
                'w' => check_keyword(1, "hile", value_iter, CoreToken::WHILE), // while
                'c' => check_keyword(1, "ontinue", value_iter, CoreToken::CONTINUE), // continue
                'b' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => {
                            match next_c {
                                'r' => check_keyword(2, "eak", value_iter, CoreToken::BREAK),
                                'o' => check_keyword(
                                    2, "ol", value_iter, CoreToken::ATOMIC_TYPE
                                ),
                                _ => return CoreToken::IDENTIFIER
                            }
                        },
                        None => return CoreToken::IDENTIFIER
                    }
                }, // break, bool
                'i' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => {
                            match next_c {
                                'f' => check_keyword(2, "", value_iter, CoreToken::IF),
                                'n' => {
                                    let next_next_c = value_iter.next();
                                    match next_next_c {
                                        Some(next_next_c) => {
                                            match next_next_c {
                                                't' => {
                                                    let next_next_next_c = value_iter.next();
                                                    match next_next_next_c {
                                                        Some(next_next_next_c) => {
                                                            match next_next_next_c {
                                                                'e' => check_keyword(
                                                                    4, 
                                                                    "rface", 
                                                                    value_iter, 
                                                                    CoreToken::INTERFACE_KEYWORD
                                                                ),
                                                                _ => return CoreToken::IDENTIFIER
                                                            }
                                                        },
                                                        None => return CoreToken::ATOMIC_TYPE
                                                    }
                                                },
                                                _ => return CoreToken::IDENTIFIER
                                            }
                                        },
                                        None => return CoreToken::IN
                                    }
                                },
                                'm' => check_keyword(2, "pl", value_iter, CoreToken::IMPL),
                                _ => return CoreToken::IDENTIFIER
                            }
                        },
                        None => return CoreToken::IDENTIFIER
                    }
                }, // if, interface, in, impl, int
                'e' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => {
                            match next_c {
                                'l' => {
                                    let next_next_c = value_iter.next();
                                    match next_next_c {
                                        Some(next_next_c) => {
                                            match next_next_c {
                                                's' => check_keyword(
                                                    3, "e", value_iter, CoreToken::ELSE
                                                ),
                                                'i' => check_keyword(
                                                    1, "f", value_iter, CoreToken::ELIF
                                                ),
                                                _ => return CoreToken::IDENTIFIER
                                            }
                                        },
                                        None => return CoreToken::IDENTIFIER
                                    }
                                },
                                _ => return CoreToken::IDENTIFIER
                            }
                        },
                        None => return CoreToken::IDENTIFIER
                    }
                }, // else, elif
                't' => check_keyword(1, "ype", value_iter, CoreToken::TYPE_KEYWORD), // type
                'd' => check_keyword(1, "ef", value_iter, CoreToken::DEF), // def
                'l' => check_keyword(1, "et", value_iter, CoreToken::LET), // let
                's' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => {
                            match next_c {
                                'e' => check_keyword(2, "lf", value_iter, CoreToken::SELF),
                                't' => check_keyword(
                                    2, "ring", value_iter, CoreToken::ATOMIC_TYPE
                                ),
                                _ => return CoreToken::IDENTIFIER
                            }
                        },
                        None => return CoreToken::IDENTIFIER
                    }
                }, // self, string
                'a' => check_keyword(1, "nd", value_iter, CoreToken::AND), // and
                'n' => check_keyword(1, "ot", value_iter, CoreToken::NOT), // not
                'o' => check_keyword(1, "r", value_iter, CoreToken::OR), // or
                'T' => check_keyword(1, "rue", value_iter, CoreToken::TRUE), // True
                'F' => check_keyword(1, "alse", value_iter, CoreToken::FALSE), // False
                'r' => check_keyword(1, "eturn", value_iter, CoreToken::RETURN), // return
                _ => CoreToken::IDENTIFIER
            }
        },
        None => unreachable!("identifer value should have alteast one character")
    }
}

// everytime there is an addition in keyword, add here also!
/*
pub fn get_token_for_identifier(value: String) -> CoreToken {
    // TODO - try to keep in the same map
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
        } else if value.eq(TYPE_KEYWORD) {
            CoreToken::TYPE_KEYWORD
        } else if value.eq(INTERFACE_KEYWORD) {
            CoreToken::INTERFACE_KEYWORD
        } else if value.eq(DEF) {
            CoreToken::DEF
        } else if value.eq(LET) {
            CoreToken::LET
        } else if value.eq(SELF) {
            CoreToken::SELF
        } else if value.eq(IMPL) {
            CoreToken::IMPL
        } else if value.eq(AND) {
            CoreToken::AND
        } else if value.eq(NOT) {
            CoreToken::NOT
        } else if value.eq(OR) {
            CoreToken::OR
        } else if value.eq(IN) {
            CoreToken::IN
        } else if value.eq(TRUE) {
            CoreToken::TRUE
        } else if value.eq(FALSE) {
            CoreToken::FALSE
        } else if value.eq(FUNC) {
            CoreToken::FUNC
        } else if value.eq(RETURN) {
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

 */