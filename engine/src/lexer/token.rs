use crate::errors::{LexicalError};
use std::rc::Rc;
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
    BREAK,              // 'break'

    // functions
    DEF,                // 'def'
    RETURN,             // 'return'

    // types
    // STRUCT,          // 'struct'
    TYPE_KEYWORD,       // 'type'
    TYPE(TokenValue),
    NEW,                // 'new'
    INTERFACE_KEYWORD,  // 'interface'

    // bitwise operators
    AND,                // 'and'
    NOT,                // 'not'
    OR,                 // 'or'
    IS,                 // 'is'
    IN,                 // 'in'

    // operators
    PLUS,               // '+'
    // DOUBLE_PLUS,        // '++'
    MINUS,              // '-'
    // DOUBLE_MINUS,       // '--'
    RIGHT_ARROW,        // '->'
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
    LANGLE,             // '<'
    RANGLE,             // '>'

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
    // NUMBER(TokenValue),
    INTEGER(TokenValue),
    FLOAT(TokenValue),
    IDENTIFIER(TokenValue),
    LITERAL(TokenValue),

    // booleans
    TRUE,               // 'True'
    FALSE,              // 'False'

    // ignored by parser
    SINGLE_LINE_COMMENT,// '//......\n' or '#.........\n'
    BLOCK_COMMENT,      // '/* ..... */'

    // corner cases
    ENDMARKER,
    NONE,
}

#[derive(Debug)]
pub struct Token {
    pub line_number: usize,
    pub core_token: CoreToken,
    pub name: Rc<String>,
    pub start_index: usize,
    pub end_index: usize,
}

impl Token {
    // This method tokenize the code in O(|code|)
    pub fn extract_lexeme(begin_lexeme: &mut usize, 
        line_number: &mut usize, code: &Vec<char>, 
        code_lines: &mut Vec<(Rc<String>, usize)>, line_start_index: &mut usize) -> Result<Token, LexicalError> {
        let start_index = *begin_lexeme;
        let critical_char = code[*begin_lexeme];
        let (core_token, name) = match critical_char {
            '('         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::LPAREN, String::from("("))
            },
            ')'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::RPAREN, String::from(")"))
            },
            '{'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::LBRACE, String::from("{"))
            },
            '}'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::RBRACE, String::from("}"))
            },
            '['         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::LSQUARE, String::from("["))
            },
            ']'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::RSQUARE, String::from("]"))
            },
            '<'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::LANGLE, String::from("<"))
            },
            '>'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::RANGLE, String::from(">"))
            },
            ';'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::SEMICOLON, String::from(";"))
            },
            ':'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::COLON, String::from(":"))
            },
            ','         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::COMMA, String::from(","))
            },
            '.'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::DOT, String::from("."))
            },
            ' '         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::BLANK, String::from(" "))
            },
            '\t'        =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::TAB, String::from("\t"))
            },
            '\n'        =>      {
                code_lines.push((Rc::new(code[*line_start_index..*begin_lexeme].iter().collect()), *line_start_index));
                *line_start_index = *begin_lexeme + 1;
                *begin_lexeme = *begin_lexeme + 1;
                *line_number = *line_number + 1;
                (CoreToken::NEWLINE, String::from("\n"))
            },
            '+'         =>      {
                // helper::extract_plus_prefix_lexeme(begin_lexeme, code)?
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::PLUS, String::from("+"))
            },
            '-'         =>      {
                helper::extract_minus_prefix_lexeme(begin_lexeme, code)?
            }
            '*'         =>      {
                helper::extract_star_prefix_lexeme(begin_lexeme, code)?
            },
            '/'         =>      {
                helper::extract_slash_prefix_lexeme(begin_lexeme, line_number, code, code_lines, line_start_index)?
            },
            '#'         =>      {
                helper::extract_hash_prefix_lexeme(begin_lexeme, line_number, code, code_lines, line_start_index)?
            }
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
                let name: String;
                if context::is_letter(&c) {
                    (token, name) = helper::extract_letter_prefix_lexeme(begin_lexeme, code)?;
                } else if context::is_digit(&c) {
                    (token, name) = helper::extract_digit_prefix_lexeme(begin_lexeme, line_number, code)?;
                } else {
                    return Err(LexicalError::new(*line_number, format!("invalid character '{}' found", c)))
                }
                (token, name)
            }
        };
        let end_index = *begin_lexeme;
        Ok(Token {
            line_number: *line_number,
            core_token,
            name: Rc::new(name),
            start_index,
            end_index,
        })
    }

    pub fn get_value(&self) -> Option<TokenValue> {
        match &self.core_token {
            CoreToken::TYPE(value)          =>      {
                Some(TokenValue(value.0.clone()))
            },
            CoreToken::INTEGER(value)       =>      {
                Some(TokenValue(value.0.clone()))
            },
            CoreToken::FLOAT(value)         =>      {
                Some(TokenValue(value.0.clone()))
            },
            CoreToken::IDENTIFIER(value)    =>      {
                Some(TokenValue(value.0.clone()))
            }
            CoreToken::LITERAL(value)       =>      {
                Some(TokenValue(value.0.clone()))
            },
            _ => {
                None
            }
        }
    }

    pub fn is_eq(&self, symbol: &str) -> bool {
        self.name.as_ref().eq(symbol)
    }
}