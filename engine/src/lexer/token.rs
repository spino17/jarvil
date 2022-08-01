use super::helper::is_letter;
use super::lexer::CoreLexer;
use crate::ast::ast::ASTNode;
use crate::code::Code;
use crate::constants::common::{
    AND, ATOMIC_TYPE, BLANK, BLOCK_COMMENT, BREAK, COLON, COMMA, CONTINUE, DASH, DEF, DOT,
    DOUBLE_COLON, DOUBLE_EQUAL, DOUBLE_STAR, ELIF, ELSE, ENDMARKER, EQUAL, FALSE,
    FLOATING_POINT_NUMBER, FOR, FUNC, GREATER_EQUAL, IDENTIFIER, IF, IMPL, IN, INTEGER,
    INTERFACE_KEYWORD, LBRACE, LBRACKET, LESS_EQUAL, LET, LEXICAL_ERROR, LITERAL, LPAREN,
    LSQUARE, NEWLINE, NOT, NOT_EQUAL, OR, PLUS, RBRACE, RBRACKET, RETURN, RIGHT_ARROW, RPAREN,
    RSQUARE, SELF, SEMICOLON, SINGLE_LINE_COMMENT, SLASH, STAR, TRUE, TYPE_KEYWORD, WHILE,
};
use crate::lexer::helper;
use std::rc::Rc;

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

#[derive(Debug, Clone, PartialEq)]
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
            start_index,
            end_index,
            trivia: None,
            parent: None,
        };
        match &core_token {
            CoreToken::LEXICAL_ERROR(lexical_err_value) => match lexical_err_value.0 {
                LexicalErrorKind::INVALID_CHAR => {
                    if end_line_number != start_line_number {
                        unreachable!("invalid char should occur on the same line")
                    }
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
        (self.start_index + self.end_index) / 2 as usize
    }

    pub fn name(&self) -> String {
        self.to_string()
    }

    pub fn token_value(&self, code: &Code) -> String {
        code.token_value(self.start_index, Some(self.end_index))
    }

    pub fn width(&self) -> usize {
        self.end_index - self.start_index
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
    impl_symbol_check!(LET);
    impl_symbol_check!(SELF);
    impl_symbol_check!(IMPL);
    impl_symbol_check!(INTERFACE_KEYWORD);
    impl_symbol_check!(AND);
    impl_symbol_check!(NOT);
    impl_symbol_check!(OR);
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

    pub fn is_eq(&self, symbol: &str) -> bool {
        match symbol {
            IF                      => self.IF(),
            ELSE                    => self.ELSE(),
            ELIF                    => self.ELIF(),
            FOR                     => self.FOR(),
            WHILE                   => self.WHILE(),
            CONTINUE                => self.CONTINUE(),
            BREAK                   => self.BREAK(),
            DEF                     => self.DEF(),
            RETURN                  => self.RETURN(),
            FUNC                    => self.FUNC(),
            TYPE_KEYWORD            => self.TYPE_KEYWORD(),
            ATOMIC_TYPE             => self.ATOMIC_TYPE(),
            LET                     => self.LET(),
            SELF                    => self.SELF(),
            IMPL                    => self.IMPL(),
            INTERFACE_KEYWORD       => self.INTERFACE_KEYWORD(),
            AND                     => self.AND(),
            NOT                     => self.NOT(),
            OR                      => self.OR(),
            IN                      => self.IN(),
            TRUE                    => self.TRUE(),
            FALSE                   => self.FALSE(),
            PLUS                    => self.PLUS(),
            DASH                    => self.DASH(),
            RIGHT_ARROW             => self.RIGHT_ARROW(),
            STAR                    => self.STAR(),
            DOUBLE_STAR             => self.DOUBLE_STAR(),
            SLASH                   => self.SLASH(),
            LPAREN                  => self.LPAREN(),
            RPAREN                  => self.RPAREN(),
            LBRACE                  => self.LBRACE(),
            RBRACE                  => self.RBRACE(),
            LSQUARE                 => self.LSQUARE(),
            RSQUARE                 => self.RSQUARE(),
            SEMICOLON               => self.SEMICOLON(),
            COLON                   => self.COLON(),
            DOUBLE_COLON            => self.DOUBLE_COLON(),
            COMMA                   => self.COMMA(),
            DOT                     => self.DOT(),
            BLANK                   => self.BLANK(),
            "\n"                          => self.NEWLINE(),
            EQUAL                   => self.EQUAL(),
            DOUBLE_EQUAL            => self.DOUBLE_EQUAL(),
            LBRACKET                => self.LBRACKET(),
            RBRACKET                => self.RBRACKET(),
            LESS_EQUAL              => self.LESS_EQUAL(),
            GREATER_EQUAL           => self.GREATER_EQUAL(),
            NOT_EQUAL               => self.NOT_EQUAL(),
            INTEGER                 => self.INTEGER(),
            FLOATING_POINT_NUMBER   => self.FLOATING_POINT_NUMBER(),
            IDENTIFIER              => self.IDENTIFIER(),
            LITERAL                 => self.LITERAL(),
            SINGLE_LINE_COMMENT     => self.SINGLE_LINE_COMMENT(),
            BLOCK_COMMENT           => self.BLOCK_COMMENT(),
            ENDMARKER               => self.ENDMARKER(),
            _ => unreachable!("token `{}` missing from matching arm", symbol),
        }
    }
}

impl ToString for Token {
    fn to_string(&self) -> String {
        let symbol_str = match self.core_token {
            CoreToken::IF                       => IF,
            CoreToken::ELSE                     => ELSE,
            CoreToken::ELIF                     => ELIF,
            CoreToken::FOR                      => FOR,
            CoreToken::WHILE                    => WHILE,
            CoreToken::CONTINUE                 => CONTINUE,
            CoreToken::BREAK                    => BREAK,
            CoreToken::DEF                      => DEF,
            CoreToken::RETURN                   => RETURN,
            CoreToken::FUNC                     => FUNC,
            CoreToken::TYPE_KEYWORD             => TYPE_KEYWORD,
            CoreToken::ATOMIC_TYPE              => ATOMIC_TYPE,
            CoreToken::LET                      => LET,
            CoreToken::SELF                     => SELF,
            CoreToken::IMPL                     => IMPL,
            CoreToken::INTERFACE_KEYWORD        => INTERFACE_KEYWORD,
            CoreToken::AND                      => AND,
            CoreToken::NOT                      => NOT,
            CoreToken::OR                       => OR,
            CoreToken::IN                       => IN,
            CoreToken::TRUE                     => TRUE,
            CoreToken::FALSE                    => FALSE,
            CoreToken::PLUS                     => PLUS,
            CoreToken::DASH                     => DASH,
            CoreToken::RIGHT_ARROW              => RIGHT_ARROW,
            CoreToken::STAR                     => STAR,
            CoreToken::DOUBLE_STAR              => DOUBLE_STAR,
            CoreToken::SLASH                    => SLASH,
            CoreToken::LPAREN                   => LPAREN,
            CoreToken::RPAREN                   => RPAREN,
            CoreToken::LBRACE                   => LBRACE,
            CoreToken::RBRACE                   => RBRACE,
            CoreToken::LSQUARE                  => LSQUARE,
            CoreToken::RSQUARE                  => RSQUARE,
            CoreToken::SEMICOLON                => SEMICOLON,
            CoreToken::COLON                    => COLON,
            CoreToken::DOUBLE_COLON             => DOUBLE_COLON,
            CoreToken::COMMA                    => COMMA,
            CoreToken::DOT                      => DOT,
            CoreToken::BLANK                    => BLANK,
            CoreToken::NEWLINE                  => NEWLINE,
            CoreToken::EQUAL                    => EQUAL,
            CoreToken::DOUBLE_EQUAL             => DOUBLE_EQUAL,
            CoreToken::LBRACKET                 => LBRACKET,
            CoreToken::RBRACKET                 => RBRACKET,
            CoreToken::LESS_EQUAL               => LESS_EQUAL,
            CoreToken::GREATER_EQUAL            => GREATER_EQUAL,
            CoreToken::NOT_EQUAL                => NOT_EQUAL,
            CoreToken::INTEGER                  => INTEGER,
            CoreToken::FLOATING_POINT_NUMBER    => FLOATING_POINT_NUMBER,
            CoreToken::IDENTIFIER               => IDENTIFIER,
            CoreToken::LITERAL                  => LITERAL,
            CoreToken::SINGLE_LINE_COMMENT      => SINGLE_LINE_COMMENT,
            CoreToken::BLOCK_COMMENT            => BLOCK_COMMENT,
            CoreToken::ENDMARKER                => ENDMARKER,
            CoreToken::LEXICAL_ERROR(_)         => LEXICAL_ERROR,
        };
        String::from(symbol_str)
    }
}
