use crate::errors::{LexicalError, SemanticError};
use std::rc::Rc;
use crate::env::Env;

#[derive(Debug)]
pub struct TokenValue(pub Rc<String>);

#[derive(Debug)]
pub enum CoreToken {

    // conditionals
    IF,                 // 'if'
    ELSE,               // 'else'
    ELIF,               // 'elif'

    // loops
    FOR,                // 'for'
    WHILE,              // 'while'

    // types
    STRUCT,             // 'struct'
    TYPE(TokenValue),   // 'type'

    // bitwise operators
    AND,                // 'and'
    NOT,                // 'not'
    OR,                 // 'or'
    IS,                 // 'is'

    // operators
    PLUS,               // '+'
    DOUBLE_PLUS,        // '++'
    MINUS,              // '-'
    DOUBLE_MINUS,       // '--'
    STAR,               // '*'
    DOUBLE_STAR,        // '**'
    SLASH,              // '/'
    DOUBLE_SLASH,       // '//'

    // wrappers
    LPAREN,             // '('
    RPAREN,             // ')'
    LBRACE,             // '{'
    RBRACE,             // '}'
    LSQUARE,            // '['
    RSQUARE,            // ']'

    // delimiters
    SEMICOLON,          // ';'
    COLON,              // ':'
    COMMA,              // ','
    LCOMMENT,           // '/*'
    RCOMMENT,           // '*/'
    DOT,                // '.'
    BLANK,              // ' '
    TAB,                // '\t'
    NEWLINE,            // '\n'

    // comparison
    EQUAL,              // '='
    DOUBLE_EQUAL,       // '=='
    GREATER_EQUAL,      // '>='
    GREATER,            // '>'
    LESS_EQUAL,         // '<='
    LESS,               // '<'

    // others
    NUMBER(TokenValue),
    IDENTIFIER(TokenValue),
    LITERAL(TokenValue),
}

#[derive(Debug)]
pub struct Token {
    line_number: usize,
    core_token: CoreToken,
}

impl Token {
    pub fn extract_lexeme(begin_lexeme: &mut usize, line_number: &mut usize, code: &Vec<char>) -> Token {
        let critical_char = code[*begin_lexeme];
        let core_token = match critical_char {
            '+'         =>      {
                extract_plus_prefix_lexeme(begin_lexeme)
            },
            '-'         =>      {
                extract_minus_prefix_lexeme(begin_lexeme)
            }
            '*'         =>      {
                extract_star_prefix_lexeme(begin_lexeme)
            },
            '/'         =>      {
                extract_slash_prefix_lexeme(begin_lexeme)
            },
            '('         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::LPAREN
            },
            ')'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::RPAREN
            },
            '{'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::LBRACE
            },
            '}'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::RBRACE
            },
            '['         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::LSQUARE
            },
            ']'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::RSQUARE
            },
            ';'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::SEMICOLON
            },
            ':'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::COLON
            },
            ','         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::COMMA
            },
            '.'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::DOT
            },
            ' '         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::BLANK
            },
            '\t'        =>      {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::TAB
            },
            '\n'        =>      {
                *begin_lexeme = *begin_lexeme + 1;
                *line_number = *line_number + 1;
                CoreToken::NEWLINE
            },
            '='         =>      {
                extract_equal_prefix_lexeme(begin_lexeme)
            },
            '>'        =>      {
                extract_greater_prefix_lexeme(begin_lexeme)
            },
            '<'        =>      {
                extract_less_prefix_lexeme(begin_lexeme)
            },
            _           =>      {
                // check if a letter or num or literal
                panic!("{:?}", LexicalError{})  // This is a bug!
            }
        };
        Token {
            line_number: *line_number,
            core_token,
        }
    }

    pub fn get_value(&self) -> Option<TokenValue> {
        match &self.core_token {
            CoreToken::TYPE(value) => {
                Some(TokenValue(value.0.clone()))
            },
            CoreToken::NUMBER(value) => {
                Some(TokenValue(value.0.clone()))
            },
            CoreToken::IDENTIFIER(value) => {
                Some(TokenValue(value.0.clone()))
            }
            CoreToken::LITERAL(value) => {
                Some(TokenValue(value.0.clone()))
            },
            _ => {
                None
            }
        }
    }
    
    pub fn is_type(&self, env: &Env) -> Result<bool, SemanticError> {
        match &self.core_token {
            CoreToken::TYPE(_) => {
                Ok(true)
            },
            CoreToken::IDENTIFIER(token_value) => {
                let symbol_table = env.check_declaration(token_value)?;
                Ok(symbol_table.is_type())
            },
            _ => {
                Ok(false)
            }
        }
    }
}