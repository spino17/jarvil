#[macro_use]
use jarvil_macros::Tokenify;
use super::helper::is_letter;
use super::lexer::CoreLexer;
use crate::code::Code;
use crate::constants::common::{
    AND, ATOMIC_TYPE, BLANK, BLOCK_COMMENT, BREAK, COLON, COMMA, CONTINUE, DASH, DEF, DOT,
    DOUBLE_COLON, DOUBLE_EQUAL, DOUBLE_STAR, ELIF, ELSE, ENDMARKER, EQUAL, FALSE,
    FLOATING_POINT_NUMBER, FOR, FUNC, GREATER_EQUAL, IDENTIFIER, IF, IMPL, IN, INTEGER,
    INTERFACE_KEYWORD, LBRACE, LBRACKET, LESS_EQUAL, LET, LEXICAL_ERROR, LITERAL, LPAREN, LSQUARE,
    NEWLINE, NOT, NOT_EQUAL, OR, PLUS, RBRACE, RBRACKET, RETURN, RIGHT_ARROW, RPAREN, RSQUARE,
    SELF, SEMICOLON, SINGLE_LINE_COMMENT, SLASH, STAR, TRUE, TYPE_KEYWORD, WHILE,
};
use crate::lexer::helper;
use std::convert::TryFrom;
use std::rc::Rc;
use text_size::{TextRange, TextSize};

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
    // This method tokenize the code in O(|code|)
    pub fn extract_lexeme(lexer: &mut CoreLexer, code: &Code) -> Token {
        let begin_lexeme = &mut lexer.begin_lexeme;
        let line_number = &mut lexer.line_number;
        let code_lines = &mut lexer.code_lines;
        let line_start_index = &mut lexer.line_start_index;

        let start_index = *begin_lexeme;
        let start_line_number = *line_number;
        let critical_char = code.get_char(*begin_lexeme);
        let core_token = match critical_char {
            '(' => {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::LPAREN
            }
            ')' => {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::RPAREN
            }
            '{' => {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::LBRACE
            }
            '}' => {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::RBRACE
            }
            '[' => {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::LSQUARE
            }
            ']' => {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::RSQUARE
            }
            ';' => {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::SEMICOLON
            }
            ',' => {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::COMMA
            }
            '.' => {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::DOT
            }
            /*
            '\t'        =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::TAB, String::from("\t"))
            },
             */
            '+' => {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::PLUS
            }
            '\n' => {
                code_lines.push(*line_start_index);
                *line_start_index = *begin_lexeme + 1;
                *begin_lexeme = *begin_lexeme + 1;
                *line_number = *line_number + 1;
                CoreToken::NEWLINE
            }
            '/' => helper::extract_slash_prefix_lexeme(
                begin_lexeme,
                line_number,
                code,
                code_lines,
                line_start_index,
            ),
            '"' => helper::extract_double_quote_prefix_lexeme(
                begin_lexeme,
                line_number,
                code,
                code_lines,
                line_start_index,
            ),
            '\'' => helper::extract_single_quote_prefix_lexeme(
                begin_lexeme,
                line_number,
                code,
                code_lines,
                line_start_index,
            ),
            '!' => helper::extract_exclaimation_prefix_lexeme(begin_lexeme, code),
            ' ' => helper::extract_blank_prefix_lexeme(begin_lexeme, code),
            '-' => helper::extract_dash_prefix_lexeme(begin_lexeme, code),
            '*' => helper::extract_star_prefix_lexeme(begin_lexeme, code),
            '#' => helper::extract_hash_prefix_lexeme(begin_lexeme, code),
            '=' => helper::extract_equal_prefix_lexeme(begin_lexeme, code),
            '>' => helper::extract_rbracket_prefix_lexeme(begin_lexeme, code),
            '<' => helper::extract_lbracket_prefix_lexeme(begin_lexeme, code),
            ':' => helper::extract_colon_prefix_lexeme(begin_lexeme, code),
            c => {
                let token: CoreToken;
                if is_letter(&c) {
                    token = helper::extract_letter_prefix_lexeme(begin_lexeme, code);
                } else if c.is_digit(10) {
                    token = helper::extract_digit_prefix_lexeme(begin_lexeme, code);
                } else {
                    let error_str = Rc::new(format!("invalid character `{}` found", c));
                    token = CoreToken::LEXICAL_ERROR((
                        LexicalErrorKind::INVALID_CHAR,
                        error_str.clone(),
                    ));
                    *begin_lexeme = *begin_lexeme + 1;
                }
                token
            }
        };
        let end_index = *begin_lexeme;
        let end_line_number = *line_number;
        let token = Token {
            line_number: *line_number,
            core_token: core_token.clone(),
            range: TextRange::new(
                TextSize::try_from(start_index).unwrap(),
                TextSize::try_from(end_index).unwrap(),
            ),
            trivia: None,
        };
        match &core_token {
            CoreToken::LEXICAL_ERROR(lexical_err_value) => match lexical_err_value.0 {
                LexicalErrorKind::INVALID_CHAR => {
                    assert!(
                        end_line_number == start_line_number,
                        "invalid char should occur on the same line"
                    );
                    lexer.log_invalid_char_lexical_error(&token, &lexical_err_value.1);
                }
                LexicalErrorKind::NO_CLOSING_SYMBOLS => {
                    lexer.log_no_closing_symbols_lexical_error(
                        start_line_number,
                        end_line_number,
                        &lexical_err_value.1,
                    );
                }
            },
            _ => {}
        }
        token
    }

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
