use crate::errors::{LexicalError, SemanticError};
use std::rc::Rc;
use crate::env::{Env,SymbolData};
use crate::lexer::helper;
use crate::context;

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
    CONTINUE,           // 'continue'

    // functions
    DEF,                // 'def'

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
    INTEGER(TokenValue),
    FLOAT(TokenValue),
    IDENTIFIER(TokenValue),
    LITERAL(TokenValue),

    // booleans
    TRUE,               // 'true'
    FALSE,              // 'false'

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
    // This method tokenize the code in O(|code|)
    pub fn extract_lexeme(begin_lexeme: &mut usize, line_number: &mut usize, code: &Vec<char>) -> Result<Token, LexicalError> {
        let critical_char = code[*begin_lexeme];
        let core_token = match critical_char {
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
            '+'         =>      {
                helper::extract_plus_prefix_lexeme(begin_lexeme, code)?
            },
            '-'         =>      {
                helper::extract_minus_prefix_lexeme(begin_lexeme, code)?
            }
            '*'         =>      {
                helper::extract_star_prefix_lexeme(begin_lexeme, code)?
            },
            '/'         =>      {
                helper::extract_slash_prefix_lexeme(begin_lexeme, line_number, code)?
            },
            '='         =>      {
                helper::extract_equal_prefix_lexeme(begin_lexeme, code)?
            },
            '>'         =>      {
                helper::extract_greater_prefix_lexeme(begin_lexeme, code)?
            },
            '<'         =>      {
                helper::extract_less_prefix_lexeme(begin_lexeme, code)?
            },
            '"'         =>      {
                helper::extract_literal_prefix_lexeme(begin_lexeme, line_number, code)?
            },
            c     =>      {
                let token: CoreToken;
                if context::is_letter(&c) {
                    token = helper::extract_letter_prefix_lexeme(begin_lexeme, code)?;
                } else if context::is_digit(&c) {
                    token = helper::extract_digit_prefix_lexeme(begin_lexeme, code)?;
                } else {
                    unreachable!("token missing for char `{}` prefix missing", c)
                }
                token
            }
        };
        Ok(Token {
            line_number: *line_number,
            core_token,
        })
    }

    pub fn get_value(&self) -> TokenValue {
        match &self.core_token {
            CoreToken::TYPE(value) => {
                TokenValue(value.0.clone())
            },
            CoreToken::NUMBER(value) => {
                TokenValue(value.0.clone())
            },
            CoreToken::IDENTIFIER(value) => {
                TokenValue(value.0.clone())
            }
            CoreToken::LITERAL(value) => {
                TokenValue(value.0.clone())
            },
            _ => {
                unreachable!("get value should only be used for type, number, identifier and literal")
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
            _ => unreachable!("check_declaration cannot be used for tokens other than type identifier")
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