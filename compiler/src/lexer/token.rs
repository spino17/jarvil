#[allow(non_camel_case_types)]
use crate::code::JarvilCodeHandler;
use crate::constants::common::{
    AND, AS, ASSERT_KEYWORD, ASYNC_KEYWORD, ATOMIC_TYPE, AWAIT_KEYWORD, BLANK, BLOCK_COMMENT,
    BREAK, CASE, CLASS_KEYWORD, COLON, COMMA, CONTINUE, DASH, DEF, DEL_KEYWORD, DOT, DOUBLE_COLON,
    DOUBLE_EQUAL, DOUBLE_STAR, ELIF, ELSE, ENDMARKER, ENUM_KEYWORD, EQUAL, EXCEPT_KEYWORD, FALSE,
    FINALLY_KEYWORD, FLOATING_POINT_NUMBER, FOR, FROM_KEYWORD, GLOBAL_KEYWORD, GREATER_EQUAL,
    IDENTIFIER, IF, IMPLEMENTS_KEYWORD, IMPORT_KEYWORD, IN, INTEGER, INTERFACE_KEYWORD,
    INVALID_CHAR, IS, LAMBDA_KEYWORD, LBRACE, LBRACKET, LESS_EQUAL, LET, LITERAL, LPAREN, LSQUARE,
    MATCH, NEWLINE, NONE, NONLOCAL_KEYWORD, NOT, NOT_EQUAL, OR, PASS_KEYWORD, PEG_PARSER, PLUS,
    RAISE_KEYWORD, RBRACE, RBRACKET, RETURN, RIGHT_ARROW, RPAREN, RSQUARE, SELF, SEMICOLON,
    SINGLE_LINE_COMMENT, SLASH, STAR, STRUCT_KEYWORD, TRUE, TRY_KEYWORD, TYPE_KEYWORD,
    UNCLOSED_BLOCK_COMMENT, UNCLOSED_STRING_LITERAL_DOUBLE_QUOTE,
    UNCLOSED_STRING_LITERAL_SINGLE_QUOTE, WHILE, WITH_KEYWORD, YIELD_KEYWORD,
};
use crate::core::string_interner::{Interner, StrId};
use jarvil_macros::Tokenify;
use serde::ser::SerializeStruct;
use serde::Serialize;
use std::fmt::Display;
use text_size::TextRange;

#[derive(Debug, Clone)]
pub struct Token {
    line_number: usize,
    core_token: CoreToken,
    range: TextRange,
    trivia: Option<Vec<Token>>,
}

impl Token {
    pub fn new(
        line_number: usize,
        core_token: CoreToken,
        range: TextRange,
        trivia: Option<Vec<Token>>,
    ) -> Self {
        Token {
            line_number,
            core_token,
            range,
            trivia,
        }
    }

    pub fn line_number(&self) -> usize {
        self.line_number
    }

    pub fn core_token(&self) -> &CoreToken {
        &self.core_token
    }

    pub fn range(&self) -> TextRange {
        self.range
    }

    pub fn trivia(&self) -> Option<&Vec<Token>> {
        self.trivia.as_ref()
    }

    pub fn set_trivia(&mut self, trivia_vec: Vec<Token>) {
        self.trivia = Some(trivia_vec);
    }

    pub fn start_index(&self) -> usize {
        self.range.start().into()
    }

    pub fn end_index(&self) -> usize {
        self.range.end().into()
    }

    pub fn is_trivia(&self) -> bool {
        match self.core_token {
            CoreToken::BLANK | CoreToken::SINGLE_LINE_COMMENT | CoreToken::BLOCK_COMMENT => true,
            _ => false,
        }
    }

    pub fn len(&self) -> usize {
        self.end_index() - self.start_index()
    }

    pub fn name(&self) -> String {
        self.core_token.to_string().to_string()
    }

    pub fn token_value(&self, code: &JarvilCodeHandler, interner: &Interner) -> StrId {
        interner.intern(&code.code.token_from_range(self.range))
    }

    pub fn token_value_str(&self, code: &JarvilCodeHandler) -> String {
        code.code.token_from_range(self.range)
    }

    pub fn is_eq(&self, symbol: &str) -> bool {
        self.core_token.is_eq(symbol)
    }

    pub fn try_as_binary_operator(&self) -> Option<BinaryOperatorKind> {
        match self.core_token {
            CoreToken::NOT_EQUAL => Some(BinaryOperatorKind::NotEqual),
            CoreToken::DOUBLE_EQUAL => Some(BinaryOperatorKind::DoubleEqual),
            CoreToken::RBRACKET => Some(BinaryOperatorKind::Greater),
            CoreToken::GREATER_EQUAL => Some(BinaryOperatorKind::GreaterEqual),
            CoreToken::LBRACKET => Some(BinaryOperatorKind::Less),
            CoreToken::LESS_EQUAL => Some(BinaryOperatorKind::LessEqual),
            CoreToken::DASH => Some(BinaryOperatorKind::Subtract),
            CoreToken::PLUS => Some(BinaryOperatorKind::Add),
            CoreToken::SLASH => Some(BinaryOperatorKind::Divide),
            CoreToken::STAR => Some(BinaryOperatorKind::Multiply),
            CoreToken::AND => Some(BinaryOperatorKind::And),
            CoreToken::OR => Some(BinaryOperatorKind::Or),
            _ => None,
        }
    }

    pub fn precedence(&self) -> u8 {
        match self.core_token {
            CoreToken::OR => 1,
            CoreToken::AND => 2,
            CoreToken::LBRACKET
            | CoreToken::LESS_EQUAL
            | CoreToken::RBRACKET
            | CoreToken::GREATER_EQUAL
            | CoreToken::DOUBLE_EQUAL
            | CoreToken::NOT_EQUAL => 3,
            CoreToken::PLUS | CoreToken::DASH => 4,
            CoreToken::STAR | CoreToken::SLASH => 5,
            _ => 0,
        }
    }
}

impl Serialize for Token {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut s = serializer.serialize_struct("token", 4)?;
        s.serialize_field("line_number", &self.line_number)?;
        s.serialize_field("core_token", &self.core_token)?;
        s.serialize_field::<(u32, u32)>(
            "range",
            &(self.range.start().into(), self.range.end().into()),
        )?;
        s.serialize_field("trivia", &self.trivia)?;
        s.end()
    }
}

#[derive(Debug, Clone, PartialEq, Tokenify, Serialize)]
pub enum CoreToken {
    IF,             // 'if'
    ELSE,           // 'else'
    ELIF,           // 'elif'
    FOR,            // 'for'
    WHILE,          // 'while'
    CONTINUE,       // 'continue'
    BREAK,          // 'break'
    MATCH,          // 'match'
    CASE,           // 'case'
    DEF,            // 'def'
    RETURN,         // 'return'
    LAMBDA_KEYWORD, // 'lambda'
    TYPE_KEYWORD,   // 'type'
    ATOMIC_TYPE,
    LET,                 // 'let'
    SELF,                // 'self'
    IMPLEMENTS_KEYWORD,  // 'implements'
    INTERFACE_KEYWORD,   // 'interface'
    STRUCT_KEYWORD,      // 'struct'
    ENUM_KEYWORD,        // 'enum'
    AND,                 // 'and'
    NOT,                 // 'not'
    OR,                  // 'or'
    IN,                  // 'in'
    TRUE,                // 'True'
    FALSE,               // 'False'
    PLUS,                // '+'
    DASH,                // '-'
    RIGHT_ARROW,         // '->'
    STAR,                // '*'
    DOUBLE_STAR,         // '**'
    SLASH,               // '/'
    LPAREN,              // '('
    RPAREN,              // ')'
    LBRACE,              // '{'
    RBRACE,              // '}'
    LSQUARE,             // '['
    RSQUARE,             // ']'
    SEMICOLON,           // ';'
    COLON,               // ':'
    DOUBLE_COLON,        // '::'
    COMMA,               // ','
    DOT,                 // '.'
    BLANK,               // ' '
    NEWLINE,             // '\n'
    EQUAL,               // '='
    DOUBLE_EQUAL,        // '=='
    LBRACKET,            // '<'
    RBRACKET,            // '>'
    LESS_EQUAL,          // '<='
    GREATER_EQUAL,       // '>='
    NOT_EQUAL,           // '!='
    SINGLE_LINE_COMMENT, // '//...\n' or '#...\n'
    BLOCK_COMMENT,       // '/* ... */'
    INTEGER,
    FLOATING_POINT_NUMBER,
    IDENTIFIER,
    LITERAL,
    ENDMARKER,
    INVALID_CHAR,
    UNCLOSED_BLOCK_COMMENT,
    UNCLOSED_STRING_LITERAL_SINGLE_QUOTE,
    UNCLOSED_STRING_LITERAL_DOUBLE_QUOTE,

    // reserved tokens in Python (3.9.6)
    // NOTE: it may be possible that many of the below keywords are not used in
    // Jarvil but to avoid keyword clashes in generated Python code they have
    // their own token in order to exlude them from identifiers and prevent
    // their normal usage as variable names
    NONE,             // 'None'
    AS,               // 'as'
    ASSERT_KEYWORD,   // 'assert'
    CLASS_KEYWORD,    // 'class'
    DEL_KEYWORD,      // 'del'
    EXCEPT_KEYWORD,   // 'except'
    FINALLY_KEYWORD,  // 'finally'
    FROM_KEYWORD,     // 'from'
    GLOBAL_KEYWORD,   // 'global'
    IMPORT_KEYWORD,   // 'import'
    IS,               // 'is'
    NONLOCAL_KEYWORD, // 'nonlocal'
    PASS_KEYWORD,     // 'pass'
    RAISE_KEYWORD,    // 'raise'
    TRY_KEYWORD,      // 'try'
    WITH_KEYWORD,     // 'with'
    YIELD_KEYWORD,    // 'yield'
    ASYNC_KEYWORD,    // 'async'
    AWAIT_KEYWORD,    // 'await'
    PEG_PARSER,       // '__peg_parser__'
}

// This method is taken from the amazing book `Crafting Interpreters` by `Bob Nystrom`
fn check_keyword(
    remaining_str: &str,
    value: std::slice::Iter<char>,
    token_type: CoreToken,
) -> CoreToken {
    let value: String = value.collect();
    if value.len() == remaining_str.len() && value.eq(remaining_str) {
        token_type
    } else {
        CoreToken::IDENTIFIER
    }
}

impl CoreToken {
    // Trie based implementation for efficient reserved words matching
    pub fn token_for_identifier(mut value_iter: std::slice::Iter<char>) -> CoreToken {
        match value_iter.next() {
            Some(c) => {
                match c {
                    'f' => {
                        let next_c = value_iter.next();
                        match next_c {
                            Some(next_c) => match next_c {
                                'o' => check_keyword("r", value_iter, CoreToken::FOR),
                                'l' => check_keyword("oat", value_iter, CoreToken::ATOMIC_TYPE),
                                'i' => {
                                    check_keyword("nally", value_iter, CoreToken::FINALLY_KEYWORD)
                                }
                                'r' => check_keyword("om", value_iter, CoreToken::FROM_KEYWORD),
                                _ => CoreToken::IDENTIFIER,
                            },
                            None => CoreToken::IDENTIFIER,
                        }
                    } // for, float, finally, from
                    'w' => {
                        let next_c = value_iter.next();
                        match next_c {
                            Some(next_c) => match next_c {
                                'h' => check_keyword("ile", value_iter, CoreToken::WHILE),
                                'i' => check_keyword("th", value_iter, CoreToken::WITH_KEYWORD),
                                _ => CoreToken::IDENTIFIER,
                            },
                            None => CoreToken::IDENTIFIER,
                        }
                    } // with, while
                    'c' => {
                        let next_c = value_iter.next();
                        match next_c {
                            Some(next_c) => match next_c {
                                'o' => check_keyword("ntinue", value_iter, CoreToken::CONTINUE),
                                'l' => check_keyword("ass", value_iter, CoreToken::CLASS_KEYWORD),
                                'a' => check_keyword("se", value_iter, CoreToken::CASE),
                                _ => CoreToken::IDENTIFIER,
                            },
                            None => CoreToken::IDENTIFIER,
                        }
                    } // continue, class, case
                    'b' => {
                        let next_c = value_iter.next();
                        match next_c {
                            Some(next_c) => match next_c {
                                'r' => check_keyword("eak", value_iter, CoreToken::BREAK),
                                'o' => check_keyword("ol", value_iter, CoreToken::ATOMIC_TYPE),
                                _ => CoreToken::IDENTIFIER,
                            },
                            None => CoreToken::IDENTIFIER,
                        }
                    } // break, bool
                    'i' => {
                        let next_c = value_iter.next();
                        match next_c {
                            Some(next_c) => match next_c {
                                's' => check_keyword("", value_iter, CoreToken::IS),
                                'f' => check_keyword("", value_iter, CoreToken::IF),
                                'n' => {
                                    let next_next_c = value_iter.next();
                                    match next_next_c {
                                        Some(next_next_c) => match next_next_c {
                                            't' => {
                                                let next_next_next_c = value_iter.next();
                                                match next_next_next_c {
                                                    Some(next_next_next_c) => {
                                                        match next_next_next_c {
                                                            'e' => check_keyword(
                                                                "rface",
                                                                value_iter,
                                                                CoreToken::INTERFACE_KEYWORD,
                                                            ),
                                                            _ => CoreToken::IDENTIFIER,
                                                        }
                                                    }
                                                    None => CoreToken::ATOMIC_TYPE,
                                                }
                                            }
                                            _ => CoreToken::IDENTIFIER,
                                        },
                                        None => CoreToken::IN,
                                    }
                                }
                                'm' => {
                                    let next_next_c = value_iter.next();
                                    match next_next_c {
                                        Some(next_next_c) => match next_next_c {
                                            'p' => {
                                                let next_next_next_c = value_iter.next();
                                                match next_next_next_c {
                                                    Some(next_next_next_c) => {
                                                        match next_next_next_c {
                                                            'l' => check_keyword(
                                                                "ements",
                                                                value_iter,
                                                                CoreToken::IMPLEMENTS_KEYWORD,
                                                            ),
                                                            'o' => check_keyword(
                                                                "rt",
                                                                value_iter,
                                                                CoreToken::IMPORT_KEYWORD,
                                                            ),
                                                            _ => CoreToken::IDENTIFIER,
                                                        }
                                                    }
                                                    None => CoreToken::IDENTIFIER,
                                                }
                                            }
                                            _ => CoreToken::IDENTIFIER,
                                        },
                                        None => CoreToken::IDENTIFIER,
                                    }
                                }
                                _ => CoreToken::IDENTIFIER,
                            },
                            None => CoreToken::IDENTIFIER,
                        }
                    } // if, interface, in, impl, int, import, is
                    'e' => {
                        let next_c = value_iter.next();
                        match next_c {
                            Some(next_c) => match next_c {
                                'l' => {
                                    let next_next_c = value_iter.next();
                                    match next_next_c {
                                        Some(next_next_c) => match next_next_c {
                                            's' => check_keyword("e", value_iter, CoreToken::ELSE),
                                            'i' => check_keyword("f", value_iter, CoreToken::ELIF),
                                            _ => CoreToken::IDENTIFIER,
                                        },
                                        None => CoreToken::IDENTIFIER,
                                    }
                                }
                                'x' => check_keyword("cept", value_iter, CoreToken::EXCEPT_KEYWORD),
                                'n' => check_keyword("um", value_iter, CoreToken::ENUM_KEYWORD),
                                _ => CoreToken::IDENTIFIER,
                            },
                            None => CoreToken::IDENTIFIER,
                        }
                    } // else, elif, except, enum
                    't' => {
                        let next_c = value_iter.next();
                        match next_c {
                            Some(next_c) => match next_c {
                                'y' => check_keyword("pe", value_iter, CoreToken::TYPE_KEYWORD),
                                'r' => check_keyword("y", value_iter, CoreToken::TRY_KEYWORD),
                                _ => CoreToken::IDENTIFIER,
                            },
                            None => CoreToken::IDENTIFIER,
                        }
                    } // type, try
                    'd' => {
                        let next_c = value_iter.next();
                        match next_c {
                            Some(next_c) => match next_c {
                                'e' => {
                                    let next_next_c = value_iter.next();
                                    match next_next_c {
                                        Some(next_next_c) => match next_next_c {
                                            'f' => check_keyword("", value_iter, CoreToken::DEF),
                                            'l' => check_keyword(
                                                "",
                                                value_iter,
                                                CoreToken::DEL_KEYWORD,
                                            ),
                                            _ => CoreToken::IDENTIFIER,
                                        },
                                        None => CoreToken::IDENTIFIER,
                                    }
                                }
                                _ => CoreToken::IDENTIFIER,
                            },
                            None => CoreToken::IDENTIFIER,
                        }
                    } // del, def
                    'l' => {
                        let next_c = value_iter.next();
                        match next_c {
                            Some(next_c) => match next_c {
                                'e' => check_keyword("t", value_iter, CoreToken::LET),
                                'a' => check_keyword("mbda", value_iter, CoreToken::LAMBDA_KEYWORD),
                                _ => CoreToken::IDENTIFIER,
                            },
                            None => CoreToken::IDENTIFIER,
                        }
                    } // let, lambda
                    's' => {
                        let next_c = value_iter.next();
                        match next_c {
                            Some(next_c) => match next_c {
                                'e' => check_keyword("lf", value_iter, CoreToken::SELF),
                                't' => {
                                    let next_next_c = value_iter.next();
                                    match next_next_c {
                                        Some(next_next_c) => match next_next_c {
                                            'r' => {
                                                let next_next_next_c = value_iter.next();
                                                match next_next_next_c {
                                                    Some(next_next_next_c) => {
                                                        match next_next_next_c {
                                                            'u' => check_keyword(
                                                                "ct",
                                                                value_iter,
                                                                CoreToken::STRUCT_KEYWORD,
                                                            ),
                                                            _ => CoreToken::IDENTIFIER,
                                                        }
                                                    }
                                                    None => CoreToken::ATOMIC_TYPE,
                                                }
                                            }
                                            _ => CoreToken::IDENTIFIER,
                                        },
                                        None => CoreToken::IDENTIFIER,
                                    }
                                }
                                _ => CoreToken::IDENTIFIER,
                            },
                            None => CoreToken::IDENTIFIER,
                        }
                    } // self, str, struct
                    'a' => {
                        let next_c = value_iter.next();
                        match next_c {
                            Some(next_c) => match next_c {
                                'n' => check_keyword("d", value_iter, CoreToken::AND),
                                's' => {
                                    let next_next_c = value_iter.next();
                                    match next_next_c {
                                        Some(next_next_c) => match next_next_c {
                                            's' => check_keyword(
                                                "ert",
                                                value_iter,
                                                CoreToken::ASSERT_KEYWORD,
                                            ),
                                            'y' => check_keyword(
                                                "nc",
                                                value_iter,
                                                CoreToken::ASYNC_KEYWORD,
                                            ),
                                            _ => CoreToken::IDENTIFIER,
                                        },
                                        None => CoreToken::AS,
                                    }
                                }
                                'w' => check_keyword("ait", value_iter, CoreToken::AWAIT_KEYWORD),
                                _ => CoreToken::IDENTIFIER,
                            },
                            None => CoreToken::IDENTIFIER,
                        }
                    } // and, as, assert, async, await
                    'n' => {
                        let next_c = value_iter.next();
                        match next_c {
                            Some(next_c) => match next_c {
                                'o' => {
                                    let next_next_c = value_iter.next();
                                    match next_next_c {
                                        Some(next_next_c) => match next_next_c {
                                            't' => check_keyword("", value_iter, CoreToken::NOT),
                                            'n' => check_keyword(
                                                "local",
                                                value_iter,
                                                CoreToken::NONLOCAL_KEYWORD,
                                            ),
                                            _ => CoreToken::IDENTIFIER,
                                        },
                                        None => CoreToken::IDENTIFIER,
                                    }
                                }
                                _ => CoreToken::IDENTIFIER,
                            },
                            None => CoreToken::IDENTIFIER,
                        }
                    } // not, nonlocal
                    'm' => check_keyword("atch", value_iter, CoreToken::MATCH), // match
                    'o' => check_keyword("r", value_iter, CoreToken::OR),       // or
                    'T' => check_keyword("rue", value_iter, CoreToken::TRUE),   // True
                    'F' => check_keyword("alse", value_iter, CoreToken::FALSE), // False
                    'r' => {
                        let next_c = value_iter.next();
                        match next_c {
                            Some(next_c) => match next_c {
                                'e' => check_keyword("turn", value_iter, CoreToken::RETURN),
                                'a' => check_keyword("ise", value_iter, CoreToken::RAISE_KEYWORD),
                                _ => CoreToken::IDENTIFIER,
                            },
                            None => CoreToken::IDENTIFIER,
                        }
                    } // raise, return
                    'N' => check_keyword("one", value_iter, CoreToken::NONE),   // None
                    'g' => check_keyword("lobal", value_iter, CoreToken::GLOBAL_KEYWORD), // global
                    'p' => check_keyword("ass", value_iter, CoreToken::PASS_KEYWORD), // pass
                    'y' => check_keyword("ield", value_iter, CoreToken::YIELD_KEYWORD), // yield
                    '_' => check_keyword("_peg_parser__", value_iter, CoreToken::PEG_PARSER), // __peg_parser__
                    _ => CoreToken::IDENTIFIER,
                }
            }
            None => unreachable!("identifer value should have alteast one character"),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum UnaryOperatorKind {
    Plus,
    Minus,
    Not,
}

#[derive(Debug, Clone, Serialize)]
pub enum BinaryOperatorKind {
    NotEqual,
    DoubleEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Subtract,
    Add,
    Divide,
    Multiply,
    And,
    Or,
}

impl BinaryOperatorKind {
    pub fn is_comparison(&self) -> bool {
        match self {
            BinaryOperatorKind::Less
            | BinaryOperatorKind::LessEqual
            | BinaryOperatorKind::Greater
            | BinaryOperatorKind::GreaterEqual
            | BinaryOperatorKind::DoubleEqual
            | BinaryOperatorKind::NotEqual => true,
            _ => false,
        }
    }
}

impl Display for BinaryOperatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            BinaryOperatorKind::NotEqual => NOT_EQUAL,
            BinaryOperatorKind::DoubleEqual => DOUBLE_EQUAL,
            BinaryOperatorKind::Greater => RBRACKET,
            BinaryOperatorKind::GreaterEqual => GREATER_EQUAL,
            BinaryOperatorKind::Less => LBRACKET,
            BinaryOperatorKind::LessEqual => LESS_EQUAL,
            BinaryOperatorKind::Subtract => DASH,
            BinaryOperatorKind::Add => PLUS,
            BinaryOperatorKind::Divide => SLASH,
            BinaryOperatorKind::Multiply => STAR,
            BinaryOperatorKind::And => AND,
            BinaryOperatorKind::Or => OR,
        };
        write!(f, "{}", str)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LexicalErrorKind {
    InvalidChar,
    NoClosingSymbols(&'static str),
}

#[derive(Debug, Clone)]
pub struct MissingToken {
    pub expected_symbols: Vec<&'static str>,
    pub received_token: Token,
}
