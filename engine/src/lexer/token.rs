use crate::errors::{LexicalError};
use std::rc::Rc;
use crate::lexer::helper;
use crate::context;

#[derive(Debug, Clone)]
pub struct TokenValue(pub Rc<String>);

#[derive(Debug, Clone)]
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
    ATOMIC_TYPE(TokenValue),
    NEW,                // 'new'
    LET,                // 'let'
    SELF,               // 'self'
    IMPL,               // 'impl'
    INTERFACE_KEYWORD,  // 'interface'

    // bitwise operators
    AND,                // 'and'
    NOT,                // 'not'
    OR,                 // 'or'
    IS,                 // 'is'
    IN,                 // 'in'

    // operators
    PLUS,               // '+'
    // DOUBLE_PLUS,     // '++'
    DASH,               // '-'
    // DOUBLE_MINUS,    // '--'
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

    // delimiters
    SEMICOLON,          // ';'
    COLON,              // ':'
    DOUBLE_COLON,       // '::'
    COMMA,              // ','
    DOT,                // '.'
    BLANK,              // ' '
    TAB,                // '\t'
    NEWLINE,            // '\n'

    // comparison
    EQUAL,              // '='
    DOUBLE_EQUAL,       // '=='
    LBRACKET,           // '<'
    RBRACKET,           // '>'
    LESS_EQUAL,         // '<='
    GREATER_EQUAL,      // '>='

    // expression terminals
    // NUMBER((TokenValue, bool)),
    INTEGER(TokenValue),
    FLOAT(TokenValue),
    IDENTIFIER(TokenValue),
    LITERAL(TokenValue),

    // booleans
    TRUE,               // 'True'
    FALSE,              // 'False'

    // ignored by parser
    SINGLE_LINE_COMMENT(TokenValue),// '//...\n' or '#...\n'
    BLOCK_COMMENT(TokenValue),      // '/* ... */'

    // termination
    ENDMARKER,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    SUCCESS_TOKEN(Token),
    ERROR_TOKEN(ErrorToken),
    // SKIPPING_TOKEN(SkippingToken)
}

#[derive(Debug, Clone)]
pub enum ErrorToken {
    MISSING_TOKEN(MissingToken),
    INCORRECT_INDENT(IncorrectIndentToken),
}

#[derive(Debug, Clone)]
pub struct MissingToken {
    pub expected_symbol: Rc<String>,
    pub received_token: Token,
}

#[derive(Debug, Clone)]
pub struct IncorrectIndentToken {
    pub expected_indent: i64,
    pub received_indent: i64,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub line_number: usize,
    pub core_token: CoreToken,
    pub name: Rc<String>,
    pub start_index: usize,
    pub end_index: usize,
    pub trivia: Option<Vec<Token>>,  // whitespaces and comments
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
            ';'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::SEMICOLON, String::from(";"))
            },
            ','         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::COMMA, String::from(","))
            },
            '.'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::DOT, String::from("."))
            },
            /*
            '\t'        =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::TAB, String::from("\t"))
            },
             */
            '\n'        =>      {
                let mut code_str: String = code[*line_start_index..*begin_lexeme].iter().collect();
                code_str.push(' ');
                code_lines.push((Rc::new(code_str), *line_start_index));
                *line_start_index = *begin_lexeme + 1;
                *begin_lexeme = *begin_lexeme + 1;
                *line_number = *line_number + 1;
                (CoreToken::NEWLINE, String::from("\n"))
            },
            '+'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::PLUS, String::from("+"))
            },
            ' '         =>      {
                helper::extract_blank_prefix_lexeme(begin_lexeme, code)?
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
            ':'         =>      {
                helper::extract_colon_prefix_lexeme(begin_lexeme, code)?
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
            trivia: None,
        })
    }

    pub fn set_trivia(&mut self, trivia_vec: Vec<Token>) {
        self.trivia = Some(trivia_vec);
    }

    pub fn is_eq(&self, symbol: &str) -> bool {
        self.name.as_ref().eq(symbol)
    }

    pub fn index(&self) -> usize {
        (self.start_index + self.end_index) / 2 as usize
    }
}