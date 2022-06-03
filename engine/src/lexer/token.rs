use crate::errors::{LexicalError, SemanticError};
use std::rc::Rc;
use crate::env::{Env,SymbolData};
use crate::lexer::common;

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

    // ignored by parser
    SINGLE_LINE_COMMENT,// '//......\n'
    BLOCK_COMMENT,      // '/* ..... */'
}

#[derive(Debug)]
pub struct Token {
    pub line_number: usize,
    pub core_token: CoreToken,
}

impl Token {
    pub fn extract_lexeme(begin_lexeme: &mut usize, line_number: &mut usize, code: &Vec<char>) -> Result<Token, LexicalError> {
        let critical_char = code[*begin_lexeme];
        let core_token = match critical_char {
            '+'         =>      {
                common::extract_plus_prefix_lexeme(begin_lexeme, code)?
            },
            '-'         =>      {
                common::extract_minus_prefix_lexeme(begin_lexeme, code)?
            }
            '*'         =>      {
                common::extract_star_prefix_lexeme(begin_lexeme, code)?
            },
            '/'         =>      {
                common::extract_slash_prefix_lexeme(begin_lexeme, code)?
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
                common::extract_equal_prefix_lexeme(begin_lexeme, code)?
            },
            '>'        =>      {
                common::extract_greater_prefix_lexeme(begin_lexeme, code)?
            },
            '<'        =>      {
                common::extract_less_prefix_lexeme(begin_lexeme, code)?
            },
            _           =>      {
                // check if a letter or num or literal or else raise lexical error
                // panic!("{:?}", LexicalError{})  // This is a bug!
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::IF
            }
        };
        Ok(Token {
            line_number: *line_number,
            core_token,
        })
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

    pub fn check_declaration(&self, env: &Env) -> Result<SymbolData, SemanticError> {
        match &self.core_token {
            CoreToken::IDENTIFIER(token_value) => {
                match env.get(token_value) {
                    Some(symbol_data) => Ok(symbol_data),
                    None => {
                        Err(SemanticError::new("identifier is not declared in the current scope"))
                    }
                }
            },
            _ => unreachable!()  // if we call this method for token other than identifier then that's a bug
        }
    }

    /*
    pub fn is_type(&self, env: &Env) -> Result<bool, SemanticError> {
        match &self.core_token {
            CoreToken::TYPE(_) => {
                Ok(true)
            },
            CoreToken::IDENTIFIER(_) => {
                let symbol_table = self.check_declaration(env)?;
                Ok(symbol_table.is_type())
            },
            _ => {
                Ok(false)
            }
        }
    }
    */
}