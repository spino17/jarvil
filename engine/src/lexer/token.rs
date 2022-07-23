use crate::ast::ast::ASTNode;
use crate::code::Code;
use crate::constants::common::{LEXICAL_ERROR, NEWLINE, IF, ELSE, ELIF, IDENTIFIER};
use std::rc::Rc;
use crate::lexer::helper;
use crate::context;
use super::lexer::CoreLexer;

macro_rules! impl_symbol_check {
    ($t: ident) => {
        fn $t(&self) -> bool {
            match self.core_token {
                CoreToken::$t => true,
                _ => false,
            }
        }
    };
}

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
    FUNC,                               // 'func'

    // types
    TYPE_KEYWORD,                       // 'type'
    ATOMIC_TYPE,
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
    INTEGER,
    FLOATING_POINT_NUMBER,
    IDENTIFIER,
    LITERAL,

    // ignored by parser
    SINGLE_LINE_COMMENT,                // '//...\n' or '#...\n'
    BLOCK_COMMENT,                      // '/* ... */'

    // termination
    ENDMARKER,

    // error
    LEXICAL_ERROR((LexicalErrorKind, Rc<String>)),
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
    pub fn extract_lexeme(lexer: &mut CoreLexer, code: &Code) -> Token {
        let begin_lexeme = &mut lexer.begin_lexeme;
        let line_number = &mut lexer.line_number;
        let code_lines = &mut lexer.code_lines;
        let line_start_index = &mut lexer.line_start_index;

        let start_index = *begin_lexeme;
        let start_line_number = *line_number;
        let critical_char = code.get_char(*begin_lexeme);
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
                code_lines.push(*line_start_index);
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
                    (token, name) = (
                        CoreToken::LEXICAL_ERROR((LexicalErrorKind::INVALID_CHAR, error_str.clone())), 
                        String::from(LEXICAL_ERROR)
                    );
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
                        lexer.log_invalid_char_lexical_error(&token, &lexical_err_value.1);
                    },
                    LexicalErrorKind::NO_CLOSING_SYMBOLS => {
                        lexer.log_no_closing_symbols_lexical_error(start_line_number, end_line_number,
                            &lexical_err_value.1);
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
            CoreToken::NEWLINE => Rc::new(String::from(NEWLINE)),
            _ => self.name.clone(),
        }
    }

    impl_symbol_check!(IF);
    impl_symbol_check!(ELSE);
    impl_symbol_check!(ELIF);
    impl_symbol_check!(FOR);
    impl_symbol_check!(WHILE);
    impl_symbol_check!(CONTINUE);
    impl_symbol_check!(BREAK);
    impl_symbol_check!(DEF);
    impl_symbol_check!(RETURN);
    impl_symbol_check!(FUNC);
    impl_symbol_check!(TYPE_KEYWORD);
    impl_symbol_check!(ATOMIC_TYPE);
    impl_symbol_check!(NEW);
    impl_symbol_check!(LET);
    impl_symbol_check!(SELF);
    impl_symbol_check!(IMPL);
    impl_symbol_check!(INTERFACE_KEYWORD);
    impl_symbol_check!(AND);
    impl_symbol_check!(NOT);
    impl_symbol_check!(OR);
    impl_symbol_check!(IS);
    impl_symbol_check!(IN);
    impl_symbol_check!(TRUE);
    impl_symbol_check!(FALSE);
    impl_symbol_check!(PLUS);
    impl_symbol_check!(DASH);
    impl_symbol_check!(RIGHT_ARROW);
    impl_symbol_check!(STAR);
    impl_symbol_check!(DOUBLE_STAR);
    impl_symbol_check!(SLASH);
    impl_symbol_check!(LPAREN);
    impl_symbol_check!(RPAREN);
    impl_symbol_check!(LBRACE);
    impl_symbol_check!(RBRACE);
    impl_symbol_check!(LSQUARE);
    impl_symbol_check!(RSQUARE);
    impl_symbol_check!(SEMICOLON);
    impl_symbol_check!(COLON);
    impl_symbol_check!(DOUBLE_COLON);
    impl_symbol_check!(COMMA);
    impl_symbol_check!(DOT);
    impl_symbol_check!(BLANK);
    impl_symbol_check!(NEWLINE);
    impl_symbol_check!(EQUAL);
    impl_symbol_check!(DOUBLE_EQUAL);
    impl_symbol_check!(LBRACKET);
    impl_symbol_check!(RBRACKET);
    impl_symbol_check!(LESS_EQUAL);
    impl_symbol_check!(GREATER_EQUAL);
    impl_symbol_check!(NOT_EQUAL);
    impl_symbol_check!(INTEGER);
    impl_symbol_check!(FLOATING_POINT_NUMBER);
    impl_symbol_check!(IDENTIFIER);
    impl_symbol_check!(LITERAL);
    impl_symbol_check!(SINGLE_LINE_COMMENT);
    impl_symbol_check!(BLOCK_COMMENT);
    impl_symbol_check!(ENDMARKER);

    pub fn new_is_eq(&self, symbol: &str) -> bool {
        match symbol {
            IF => self.IF(),
            ELSE => self.ELSE(),
            ELIF => self.ELIF(),
            IDENTIFIER => self.IDENTIFIER(),
            _ => unreachable!("token `{}` missing from matching arm", symbol)
        }
    }
}