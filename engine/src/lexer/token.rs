use crate::ast::ast::ASTNode;
use crate::constants::common::LEXICAL_ERROR;
use std::rc::Rc;
use crate::lexer::helper;
use crate::context;

use super::lexer::CoreLexer;

#[derive(Debug, Clone)]
pub struct TokenValue(pub Rc<String>);

#[derive(Debug, Clone)]
pub enum CoreToken {

    // conditionals
    IF,                                 // 'if'
    ELSE,                               // 'else'
    ELIF,                               // 'elif'

    // loops
    FOR,                                // 'for'
    WHILE,                              // 'while'
    CONTINUE,                           // 'continue'
    BREAK,                              // 'break'

    // functions
    DEF,                                // 'def'
    RETURN,                             // 'return'

    // types
    TYPE_KEYWORD,                       // 'type'
    ATOMIC_TYPE(TokenValue),
    NEW,                                // 'new'
    LET,                                // 'let'
    SELF,                               // 'self'
    IMPL,                               // 'impl'
    INTERFACE_KEYWORD,                  // 'interface'

    // logical operators
    AND,                                // 'and'
    NOT,                                // 'not'
    OR,                                 // 'or'
    IS,                                 // 'is'
    IN,                                 // 'in'

    // booleans
    TRUE,                               // 'True'
    FALSE,                              // 'False'

    // operators
    PLUS,                               // '+'
    DASH,                               // '-'
    RIGHT_ARROW,                        // '->'
    STAR,                               // '*'
    DOUBLE_STAR,                        // '**'
    SLASH,                              // '/'

    // wrappers
    LPAREN,                             // '('
    RPAREN,                             // ')'
    LBRACE,                             // '{'
    RBRACE,                             // '}'
    LSQUARE,                            // '['
    RSQUARE,                            // ']'

    // delimiters
    SEMICOLON,                          // ';'
    COLON,                              // ':'
    DOUBLE_COLON,                       // '::'
    COMMA,                              // ','
    DOT,                                // '.'
    BLANK,                              // ' '
    // TAB,                             // '\t'
    NEWLINE,                            // '\n'

    // comparison
    EQUAL,                              // '='
    DOUBLE_EQUAL,                       // '=='
    LBRACKET,                           // '<'
    RBRACKET,                           // '>'
    LESS_EQUAL,                         // '<='
    GREATER_EQUAL,                      // '>='
    NOT_EQUAL,                          // '!='

    // expression terminals
    INTEGER(TokenValue),
    FLOATING_POINT_NUMBER(TokenValue),
    IDENTIFIER(TokenValue),
    LITERAL(TokenValue),

    // ignored by parser
    SINGLE_LINE_COMMENT(TokenValue),    // '//...\n' or '#...\n'
    BLOCK_COMMENT(TokenValue),          // '/* ... */'

    // termination
    ENDMARKER,

    // error
    LEXICAL_ERROR((LexicalErrorKind, TokenValue))
}

#[derive(Debug, Clone)]
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
    pub name: Rc<String>,
    pub start_index: usize,
    pub end_index: usize,
    pub trivia: Option<Rc<Vec<Token>>>,
    pub parent: Option<ASTNode>,
}

impl Token {
    // This method tokenize the code in O(|code|)
    pub fn extract_lexeme(lexer: &mut CoreLexer, code: &Vec<char>) -> Token {
        let begin_lexeme = &mut lexer.begin_lexeme;
        let line_number = &mut lexer.line_number;
        let code_lines = &mut lexer.code_lines;
        let line_start_index = &mut lexer.line_start_index;

        let start_index = *begin_lexeme;
        let start_line_number = *line_number;
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
            '+'         =>      {
                *begin_lexeme = *begin_lexeme + 1;
                (CoreToken::PLUS, String::from("+"))
            },
            '\n'        =>      {
                let mut code_str: String = code[*line_start_index..*begin_lexeme].iter().collect();
                code_str.push(' ');
                code_lines.push((Rc::new(code_str), *line_start_index));
                *line_start_index = *begin_lexeme + 1;
                *begin_lexeme = *begin_lexeme + 1;
                *line_number = *line_number + 1;
                (CoreToken::NEWLINE, String::from("\n"))
            },
            '/'         =>      {
                helper::extract_slash_prefix_lexeme(begin_lexeme, line_number, code, code_lines, line_start_index)
            },
            '"'         =>      {
                helper::extract_double_quote_prefix_lexeme(begin_lexeme, line_number, code, code_lines, line_start_index)
            },
            '\''         =>      {
                helper::extract_single_quote_prefix_lexeme(begin_lexeme, line_number, code, code_lines, line_start_index)
            },
            '!'         =>      {
                helper::extract_exclaimation_prefix_lexeme(begin_lexeme, code)
            },
            ' '         =>      {
                helper::extract_blank_prefix_lexeme(begin_lexeme, code)
            },
            '-'         =>      {
                helper::extract_dash_prefix_lexeme(begin_lexeme, code)
            }
            '*'         =>      {
                helper::extract_star_prefix_lexeme(begin_lexeme, code)
            },
            '#'         =>      {
                helper::extract_hash_prefix_lexeme(begin_lexeme, code)
            }
            '='         =>      {
                helper::extract_equal_prefix_lexeme(begin_lexeme, code)
            },
            '>'         =>      {
                helper::extract_rbracket_prefix_lexeme(begin_lexeme, code)
            },
            '<'         =>      {
                helper::extract_lbracket_prefix_lexeme(begin_lexeme, code)
            },
            ':'         =>      {
                helper::extract_colon_prefix_lexeme(begin_lexeme, code)
            },
            c     =>      {
                let token: CoreToken;
                let name: String;
                if context::is_letter(&c) {
                    (token, name) = helper::extract_letter_prefix_lexeme(begin_lexeme, code);
                } else if context::is_digit(&c) {
                    (token, name) = helper::extract_digit_prefix_lexeme(begin_lexeme, code);
                } else {
                    let error_str = Rc::new(format!("invalid character `{}` found", c));
                    (token, name) = (CoreToken::LEXICAL_ERROR((LexicalErrorKind::INVALID_CHAR, TokenValue(error_str.clone()))), String::from(LEXICAL_ERROR));
                    *begin_lexeme = *begin_lexeme + 1;
                }
                (token, name)
            }
        };
        let end_index = *begin_lexeme;
        let end_line_number = *line_number;
        let token = Token {
            line_number: *line_number,
            core_token: core_token.clone(),
            name: Rc::new(name),
            start_index,
            end_index,
            trivia: None,
            parent: None,
        };
        match &core_token {
            CoreToken::LEXICAL_ERROR(lexical_err_value) => {
                match lexical_err_value.0 {
                    LexicalErrorKind::INVALID_CHAR => {
                        if end_line_number != start_line_number {
                            unreachable!("invalid char should occur on the same line")
                        }
                        lexer.log_invalid_char_lexical_error(&token, &lexical_err_value.1.0);
                    },
                    LexicalErrorKind::NO_CLOSING_SYMBOLS => {
                        lexer.log_no_closing_symbols_lexical_error(start_line_number, end_line_number,
                            &lexical_err_value.1.0);
                    }
                }
            },
            _ => {}
        }
        token
    }

    pub fn set_trivia(&mut self, trivia_vec: Vec<Token>) {
        self.trivia = Some(Rc::new(trivia_vec));
    }

    pub fn is_eq(&self, symbol: &str) -> bool {
        self.name.as_ref().eq(symbol)
    }

    pub fn index(&self) -> usize {
        (self.start_index + self.end_index) / 2 as usize
    }

    pub fn name(&self) -> Rc<String> {
        match self.core_token {
            CoreToken::NEWLINE => Rc::new(String::from("newline")),
            _ => self.name.clone(),
        }
    }
}