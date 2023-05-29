#[macro_use]
use jarvil_macros::Tokenify;
use crate::code::JarvilCode;
use crate::constants::common::{
    AND, AS, ASSERT_KEYWORD, ASYNC_KEYWORD, ATOMIC_TYPE, AWAIT_KEYWORD, BLANK, BLOCK_COMMENT,
    BREAK, CLASS_KEYWORD, COLON, COMMA, CONTINUE, DASH, DEF, DEL_KEYWORD, DOT, DOUBLE_COLON,
    DOUBLE_EQUAL, DOUBLE_STAR, ELIF, ELSE, ENDMARKER, EQUAL, EXCEPT_KEYWORD, FALSE,
    FINALLY_KEYWORD, FLOATING_POINT_NUMBER, FOR, FROM_KEYWORD, GLOBAL_KEYWORD, GREATER_EQUAL,
    IDENTIFIER, IF, IMPLEMENTS_KEYWORD, IMPORT_KEYWORD, IN, INTEGER, INTERFACE_KEYWORD, IS,
    LAMBDA_KEYWORD, LBRACE, LBRACKET, LESS_EQUAL, LET, LEXICAL_ERROR, LITERAL, LPAREN, LSQUARE,
    NEWLINE, NONE, NONLOCAL_KEYWORD, NOT, NOT_EQUAL, OR, PASS_KEYWORD, PEG_PARSER, PLUS,
    RAISE_KEYWORD, RBRACE, RBRACKET, RETURN, RIGHT_ARROW, RPAREN, RSQUARE, SELF, SEMICOLON,
    SINGLE_LINE_COMMENT, SLASH, STAR, STRUCT_KEYWORD, TRUE, TRY_KEYWORD, TYPE_KEYWORD, WHILE,
    WITH_KEYWORD, YIELD_KEYWORD,
};
use std::fmt::Display;
use std::rc::Rc;
use text_size::TextRange;

#[derive(Debug, Clone, PartialEq, Tokenify)]
pub enum CoreToken {
    // conditionals
    IF,   // 'if'
    ELSE, // 'else'
    ELIF, // 'elif'

    // loops
    FOR,      // 'for'
    WHILE,    // 'while'
    CONTINUE, // 'continue'
    BREAK,    // 'break'

    // functions
    DEF,            // 'def'
    RETURN,         // 'return'
    LAMBDA_KEYWORD, // 'lambda'

    // types
    TYPE_KEYWORD, // 'type'
    ATOMIC_TYPE,
    LET,                // 'let'
    SELF,               // 'self'
    IMPLEMENTS_KEYWORD, // 'implements'
    INTERFACE_KEYWORD,  // 'interface'
    STRUCT_KEYWORD,     // 'struct'

    // logical operators
    AND, // 'and'
    NOT, // 'not'
    OR,  // 'or'
    IN,  // 'in'

    // booleans
    TRUE,  // 'True'
    FALSE, // 'False'

    // operators
    PLUS,        // '+'
    DASH,        // '-'
    RIGHT_ARROW, // '->'
    STAR,        // '*'
    DOUBLE_STAR, // '**'
    SLASH,       // '/'

    // wrappers
    LPAREN,  // '('
    RPAREN,  // ')'
    LBRACE,  // '{'
    RBRACE,  // '}'
    LSQUARE, // '['
    RSQUARE, // ']'

    // delimiters
    SEMICOLON,    // ';'
    COLON,        // ':'
    DOUBLE_COLON, // '::'
    COMMA,        // ','
    DOT,          // '.'
    BLANK,        // ' '
    // TAB,       // '\t'
    NEWLINE, // '\n'

    // comparison
    EQUAL,         // '='
    DOUBLE_EQUAL,  // '=='
    LBRACKET,      // '<'
    RBRACKET,      // '>'
    LESS_EQUAL,    // '<='
    GREATER_EQUAL, // '>='
    NOT_EQUAL,     // '!='

    // expression terminals
    INTEGER,
    FLOATING_POINT_NUMBER,
    IDENTIFIER,
    LITERAL,

    // ignored by parser
    SINGLE_LINE_COMMENT, // '//...\n' or '#...\n'
    BLOCK_COMMENT,       // '/* ... */'

    // termination
    ENDMARKER,

    // error
    LEXICAL_ERROR(LexicalErrorKind),

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

#[derive(Debug, Clone)]
pub enum UnaryOperatorKind {
    Plus,
    Minus,
    Not,
}

#[derive(Debug, Clone)]
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
    INVALID_CHAR,
    NO_CLOSING_SYMBOLS(&'static str),
}

#[derive(Debug, Clone)]
pub struct MissingToken {
    pub expected_symbols: Vec<&'static str>,
    pub received_token: Token,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub line_number: usize,
    pub core_token: CoreToken,
    pub range: TextRange,
    pub trivia: Option<Vec<Token>>,
}

impl Token {
    pub fn set_trivia(&mut self, trivia_vec: Vec<Token>) {
        self.trivia = Some(trivia_vec);
    }

    pub fn index(&self) -> usize {
        let r: usize = (self.range.start() + self.range.end()).into();
        r / 2
    }

    pub fn start_index(&self) -> usize {
        self.range.start().into()
    }

    pub fn end_index(&self) -> usize {
        self.range.end().into()
    }

    pub fn len(&self) -> usize {
        self.end_index() - self.start_index()
    }

    pub fn name(&self) -> String {
        self.core_token.to_string().to_string()
    }

    pub fn token_value(&self, code: &JarvilCode) -> String {
        code.token_from_range(self.range)
    }

    pub fn is_eq(&self, symbol: &str) -> bool {
        self.core_token.is_eq(symbol)
    }

    pub fn is_identifier(&self) -> bool {
        self.core_token.IDENTIFIER()
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

    pub fn get_precedence(&self) -> u8 {
        let precedence = match self.core_token {
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
        };
        precedence
    }
}
