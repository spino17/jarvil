#[macro_use]
use jarvil_macros::Tokenify;
use crate::code::Code;
use crate::constants::common::{
    AND, ATOMIC_TYPE, BLANK, BLOCK_COMMENT, BREAK, COLON, COMMA, CONTINUE, DASH, DEF, DOT,
    DOUBLE_COLON, DOUBLE_EQUAL, DOUBLE_STAR, ELIF, ELSE, ENDMARKER, EQUAL, FALSE,
    FLOATING_POINT_NUMBER, FOR, FUNC, GREATER_EQUAL, IDENTIFIER, IF, IMPL, IN, INTEGER,
    INTERFACE_KEYWORD, LBRACE, LBRACKET, LESS_EQUAL, LET, LEXICAL_ERROR, LITERAL, LPAREN, LSQUARE,
    NEWLINE, NOT, NOT_EQUAL, OR, PLUS, RBRACE, RBRACKET, RETURN, RIGHT_ARROW, RPAREN, RSQUARE,
    SELF, SEMICOLON, SINGLE_LINE_COMMENT, SLASH, STAR, TRUE, TYPE_KEYWORD, WHILE,
};
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
    DEF,    // 'def'
    RETURN, // 'return'
    FUNC,   // 'func'

    // types
    TYPE_KEYWORD, // 'type'
    ATOMIC_TYPE,
    LET,               // 'let'
    SELF,              // 'self'
    IMPL,              // 'impl'
    INTERFACE_KEYWORD, // 'interface'

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
    // TAB,                             // '\t'
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
    LEXICAL_ERROR((LexicalErrorKind, Rc<String>)),
}

#[derive(Debug, Clone)]
pub enum BinaryOperatorKind {
    NOT_EQUAL,
    DOUBLE_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    MINUS,
    PLUS,
    DIVIDE,
    MULTIPLY,
    AND,
    OR,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexicalErrorKind {
    INVALID_CHAR,
    NO_CLOSING_SYMBOLS,
}

#[derive(Debug, Clone)]
pub struct MissingToken {
    pub expected_symbols: Rc<Vec<&'static str>>,
    pub received_token: Token,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub line_number: usize,
    pub core_token: CoreToken,
    pub range: TextRange,
    pub trivia: Option<Rc<Vec<Token>>>,
}

impl Token {
    pub fn set_trivia(&mut self, trivia_vec: Vec<Token>) {
        self.trivia = Some(Rc::new(trivia_vec));
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

    pub fn name(&self) -> String {
        String::from(self.core_token.to_string())
    }

    pub fn token_value(&self, code: &Code) -> String {
        code.token_from_range(self.range)
    }

    pub fn is_eq(&self, symbol: &str) -> bool {
        self.core_token.is_eq(symbol)
    }

    pub fn is_binary_operator(&self) -> Option<BinaryOperatorKind> {
        match self.core_token {
            CoreToken::NOT_EQUAL => Some(BinaryOperatorKind::NOT_EQUAL),
            CoreToken::DOUBLE_EQUAL => Some(BinaryOperatorKind::DOUBLE_EQUAL),
            CoreToken::RBRACKET => Some(BinaryOperatorKind::GREATER),
            CoreToken::GREATER_EQUAL => Some(BinaryOperatorKind::GREATER_EQUAL),
            CoreToken::LBRACKET => Some(BinaryOperatorKind::LESS),
            CoreToken::LESS_EQUAL => Some(BinaryOperatorKind::LESS_EQUAL),
            CoreToken::DASH => Some(BinaryOperatorKind::MINUS),
            CoreToken::PLUS => Some(BinaryOperatorKind::PLUS),
            CoreToken::SLASH => Some(BinaryOperatorKind::DIVIDE),
            CoreToken::STAR => Some(BinaryOperatorKind::MULTIPLY),
            CoreToken::AND => Some(BinaryOperatorKind::AND),
            CoreToken::OR => Some(BinaryOperatorKind::OR),
            _ => None,
        }
    }

    pub fn is_identifier(&self) -> bool {
        self.core_token.IDENTIFIER()
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
