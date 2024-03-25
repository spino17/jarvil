use crate::code::JarvilCode;
use crate::error::diagnostics::{Diagnostics, InvalidCharError, NoClosingSymbolError};
use crate::lexer::token::CoreToken;
use crate::lexer::token::Token;
use std::convert::TryFrom;
use std::vec;
use text_size::TextRange;
use text_size::TextSize;

pub fn is_letter(c: &char) -> bool {
    c.is_ascii_alphabetic() || (*c == '_')
}

pub trait Lexer {
    fn tokenize(self, code: &mut JarvilCode) -> (Vec<Token>, Vec<Diagnostics>, Vec<usize>);
}

pub struct CoreLexer {
    index: usize,
    line_number: usize,
    code_lines: Vec<usize>,
    line_start_index: usize,
    errors: Vec<Diagnostics>,
}

impl Lexer for CoreLexer {
    fn tokenize(mut self, code: &mut JarvilCode) -> (Vec<Token>, Vec<Diagnostics>, Vec<usize>) {
        let mut token_vec: Vec<Token> = Vec::new();
        token_vec.push(Token::new(
            self.line_number,
            CoreToken::NEWLINE,
            TextRange::new(
                TextSize::try_from(0_usize).unwrap(),
                TextSize::try_from(0_usize).unwrap(),
            ),
            None,
        ));
        let mut eof_trivia_vec: Vec<Token> = vec![];
        while self.index < code.len() {
            let token = self.extract_lexeme(code);
            if token.is_trivia() {
                let mut trivia_vec = vec![token];
                let mut is_eof: Option<Token> = None;
                // spin loop to collect other trivia tokens as well
                while self.index < code.len() {
                    let token = self.extract_lexeme(code);
                    if token.is_trivia() {
                        trivia_vec.push(token);
                    } else {
                        is_eof = Some(token);
                        break;
                    }
                }
                match is_eof {
                    Some(mut token) => {
                        token.set_trivia(trivia_vec);
                        token_vec.push(token);
                    }
                    None => eof_trivia_vec = trivia_vec,
                }
            } else {
                token_vec.push(token);
            }
        }
        self.code_lines.push(self.line_start_index);
        let mut token = Token::new(
            self.line_number,
            CoreToken::ENDMARKER,
            TextRange::new(
                // ideally span of `ENDMARKER` should be (code.len() - code.len()), however to display error messages
                // we need to have non-zero range span.
                TextSize::try_from(if code.len() > 0 {
                    code.len() - 1
                } else {
                    code.len()
                })
                .unwrap(),
                TextSize::try_from(code.len()).unwrap(),
            ),
            None,
        );
        if !eof_trivia_vec.is_empty() {
            token.set_trivia(eof_trivia_vec);
        }
        token_vec.push(token);
        (token_vec, self.errors, self.code_lines)
    }
}

impl CoreLexer {
    pub fn new() -> Self {
        CoreLexer {
            index: 0,
            line_number: 1,
            code_lines: vec![],
            line_start_index: 0,
            errors: vec![],
        }
    }

    fn extract_lexeme(&mut self, code: &JarvilCode) -> Token {
        let start_index = self.index;
        let start_line_number = self.line_number;
        let critical_char = code.get_char(self.index);
        let core_token = match critical_char {
            '(' => {
                self.index += 1;
                CoreToken::LPAREN
            }
            ')' => {
                self.index += 1;
                CoreToken::RPAREN
            }
            '{' => {
                self.index += 1;
                CoreToken::LBRACE
            }
            '}' => {
                self.index += 1;
                CoreToken::RBRACE
            }
            '[' => {
                self.index += 1;
                CoreToken::LSQUARE
            }
            ']' => {
                self.index += 1;
                CoreToken::RSQUARE
            }
            ';' => {
                self.index += 1;
                CoreToken::SEMICOLON
            }
            ',' => {
                self.index += 1;
                CoreToken::COMMA
            }
            '.' => {
                self.index += 1;
                CoreToken::DOT
            }
            '+' => {
                self.index += 1;
                CoreToken::PLUS
            }
            '\n' => {
                self.code_lines.push(self.line_start_index);
                self.line_start_index = self.index + 1;
                self.index += 1;
                self.line_number += 1;
                CoreToken::NEWLINE
            }
            '/' => self.extract_slash_prefix_lexeme(code),
            '"' => self.extract_double_quote_prefix_lexeme(code),
            '\'' => self.extract_single_quote_prefix_lexeme(code),
            '!' => self.extract_exclaimation_prefix_lexeme(code),
            ' ' => self.extract_blank_prefix_lexeme(code),
            '-' => self.extract_dash_prefix_lexeme(code),
            '*' => self.extract_star_prefix_lexeme(code),
            '#' => self.extract_hash_prefix_lexeme(code),
            '=' => self.extract_equal_prefix_lexeme(code),
            '>' => self.extract_rbracket_prefix_lexeme(code),
            '<' => self.extract_lbracket_prefix_lexeme(code),
            ':' => self.extract_colon_prefix_lexeme(code),
            c => {
                let token: CoreToken;
                if is_letter(&c) {
                    token = self.extract_letter_prefix_lexeme(code);
                } else if c.is_ascii_digit() {
                    token = self.extract_digit_prefix_lexeme(code);
                } else {
                    self.index += 1;
                    token = CoreToken::INVALID_CHAR;
                }
                token
            }
        };
        let end_index = self.index;
        let end_line_number = self.line_number;
        let range = TextRange::new(
            TextSize::try_from(start_index).unwrap(),
            TextSize::try_from(end_index).unwrap(),
        );

        // log lexical errors
        match &core_token {
            CoreToken::INVALID_CHAR => {
                debug_assert!(
                    end_line_number == start_line_number,
                    "invalid char should occur on the same line",
                );
                self.log_invalid_char_error(range);
            }
            CoreToken::UNCLOSED_BLOCK_COMMENT => self.log_no_closing_symbol_error("*/", range),
            CoreToken::UNCLOSED_STRING_LITERAL_SINGLE_QUOTE => {
                self.log_no_closing_symbol_error("'", range)
            }
            CoreToken::UNCLOSED_STRING_LITERAL_DOUBLE_QUOTE => {
                self.log_no_closing_symbol_error(r#"""#, range)
            }
            _ => {}
        }
        Token::new(start_line_number, core_token, range, None)
    }

    // ------------------- lexeme extraction state machine functions -------------------
    // ' ' -> '...'
    pub fn extract_blank_prefix_lexeme(&mut self, code: &JarvilCode) -> CoreToken {
        let mut forward_lexeme = self.index + 1;
        while forward_lexeme < code.len() {
            let next_char = code.get_char(forward_lexeme);
            if next_char != ' ' {
                self.index = forward_lexeme;
                return CoreToken::BLANK;
            }
            forward_lexeme += 1;
        }
        self.index = forward_lexeme;
        CoreToken::BLANK
    }

    // - -> -, ->
    pub fn extract_dash_prefix_lexeme(&mut self, code: &JarvilCode) -> CoreToken {
        let forward_lexeme = self.index + 1;
        if forward_lexeme < code.len() {
            let next_char = code.get_char(forward_lexeme);
            match next_char {
                '>' => {
                    self.index = forward_lexeme + 1;
                    CoreToken::RIGHT_ARROW
                }
                _ => {
                    self.index += 1;
                    CoreToken::DASH
                }
            }
        } else {
            self.index += 1;
            CoreToken::DASH
        }
    }

    // * -> *, **
    pub fn extract_star_prefix_lexeme(&mut self, code: &JarvilCode) -> CoreToken {
        let forward_lexeme = self.index + 1;
        if forward_lexeme < code.len() {
            let next_char = code.get_char(forward_lexeme);
            match next_char {
                '*' => {
                    self.index = forward_lexeme + 1;
                    CoreToken::DOUBLE_STAR
                }
                _ => {
                    self.index += 1;
                    CoreToken::STAR
                }
            }
        } else {
            self.index += 1;
            CoreToken::STAR
        }
    }

    // / -> /, /*, //
    pub fn extract_slash_prefix_lexeme(&mut self, code: &JarvilCode) -> CoreToken {
        let mut forward_lexeme = self.index + 1;
        let mut state: usize = 0;
        while forward_lexeme < code.len() {
            let next_char = code.get_char(forward_lexeme);
            match state {
                0 => match next_char {
                    '/' => {
                        state = 1;
                    }
                    '*' => {
                        state = 2;
                    }
                    _ => {
                        self.index += 1;
                        return CoreToken::SLASH;
                    }
                },
                1 => match next_char {
                    '\n' => {
                        self.index = forward_lexeme;
                        return CoreToken::SINGLE_LINE_COMMENT;
                    }
                    _ => {}
                },
                2 => match next_char {
                    '*' => {
                        state = 3;
                    }
                    '\n' => {
                        self.code_lines.push(self.line_start_index);
                        self.line_number += 1;
                        self.line_start_index = forward_lexeme + 1;
                    }
                    _ => {}
                },
                3 => match next_char {
                    '/' => {
                        self.index = forward_lexeme + 1;
                        return CoreToken::BLOCK_COMMENT;
                    }
                    _ => {
                        state = 2;
                    }
                },
                _ => {
                    unreachable!("any state other than 0, 1, 2 and 3 is not reachable")
                }
            }
            forward_lexeme += 1;
        }
        match state {
            0 => {
                self.index += 1;
                CoreToken::SLASH
            }
            1 => {
                self.index = forward_lexeme;
                CoreToken::SINGLE_LINE_COMMENT
            }
            2 => {
                self.index = forward_lexeme;
                CoreToken::UNCLOSED_BLOCK_COMMENT
            }
            3 => {
                self.index = forward_lexeme;
                CoreToken::UNCLOSED_BLOCK_COMMENT
            }
            _ => unreachable!("any state other than 0, 1, 2 and 3 is not reachable"),
        }
    }

    // # -> #...\n
    pub fn extract_hash_prefix_lexeme(&mut self, code: &JarvilCode) -> CoreToken {
        let mut forward_lexeme = self.index + 1;
        while forward_lexeme < code.len() {
            let next_char = code.get_char(forward_lexeme);
            match next_char {
                '\n' => {
                    self.index = forward_lexeme;
                    return CoreToken::SINGLE_LINE_COMMENT;
                }
                _ => {}
            }
            forward_lexeme += 1;
        }
        self.index = forward_lexeme;
        CoreToken::SINGLE_LINE_COMMENT
    }

    // = -> =, ==
    pub fn extract_equal_prefix_lexeme(&mut self, code: &JarvilCode) -> CoreToken {
        let forward_lexeme = self.index + 1;
        if forward_lexeme < code.len() {
            let next_char = code.get_char(forward_lexeme);
            match next_char {
                '=' => {
                    self.index = forward_lexeme + 1;
                    CoreToken::DOUBLE_EQUAL
                }
                _ => {
                    self.index += 1;
                    CoreToken::EQUAL
                }
            }
        } else {
            self.index += 1;
            CoreToken::EQUAL
        }
    }

    // > -> >, >=
    pub fn extract_rbracket_prefix_lexeme(&mut self, code: &JarvilCode) -> CoreToken {
        let forward_lexeme = self.index + 1;
        if forward_lexeme < code.len() {
            let next_char = code.get_char(forward_lexeme);
            match next_char {
                '=' => {
                    self.index = forward_lexeme + 1;
                    CoreToken::GREATER_EQUAL
                }
                _ => {
                    self.index += 1;
                    CoreToken::RBRACKET
                }
            }
        } else {
            self.index += 1;
            CoreToken::RBRACKET
        }
    }

    // < -> <, <=
    pub fn extract_lbracket_prefix_lexeme(&mut self, code: &JarvilCode) -> CoreToken {
        let forward_lexeme = self.index + 1;
        if forward_lexeme < code.len() {
            let next_char = code.get_char(forward_lexeme);
            match next_char {
                '=' => {
                    self.index = forward_lexeme + 1;
                    CoreToken::LESS_EQUAL
                }
                _ => {
                    self.index += 1;
                    CoreToken::LBRACKET
                }
            }
        } else {
            self.index += 1;
            CoreToken::LBRACKET
        }
    }

    // ! -> !=
    pub fn extract_exclaimation_prefix_lexeme(&mut self, code: &JarvilCode) -> CoreToken {
        let forward_lexeme = self.index + 1;
        if forward_lexeme < code.len() {
            let next_char = code.get_char(forward_lexeme);
            match next_char {
                '=' => {
                    self.index = forward_lexeme + 1;
                    CoreToken::NOT_EQUAL
                }
                _ => {
                    self.index += 1;
                    CoreToken::INVALID_CHAR
                }
            }
        } else {
            self.index += 1;
            CoreToken::INVALID_CHAR
        }
    }

    // ' -> '...'
    pub fn extract_single_quote_prefix_lexeme(&mut self, code: &JarvilCode) -> CoreToken {
        let mut forward_lexeme = self.index + 1;
        while forward_lexeme < code.len() {
            let next_char = code.get_char(forward_lexeme);
            match next_char {
                '\'' => {
                    self.index = forward_lexeme + 1;
                    return CoreToken::LITERAL;
                }
                '\n' => {
                    self.code_lines.push(self.line_start_index);
                    self.line_number += 1;
                    self.line_start_index = forward_lexeme + 1;
                }
                _ => {}
            }
            forward_lexeme += 1;
        }
        self.index = forward_lexeme;
        CoreToken::UNCLOSED_STRING_LITERAL_SINGLE_QUOTE
    }

    // " -> "..."
    pub fn extract_double_quote_prefix_lexeme(&mut self, code: &JarvilCode) -> CoreToken {
        let mut forward_lexeme = self.index + 1;
        while forward_lexeme < code.len() {
            let next_char = code.get_char(forward_lexeme);
            match next_char {
                '"' => {
                    self.index = forward_lexeme + 1;
                    return CoreToken::LITERAL;
                }
                '\n' => {
                    self.code_lines.push(self.line_start_index);
                    self.line_number += 1;
                    self.line_start_index = forward_lexeme + 1;
                }
                _ => {}
            }
            forward_lexeme += 1;
        }
        self.index = forward_lexeme;
        CoreToken::UNCLOSED_STRING_LITERAL_DOUBLE_QUOTE
    }

    // letter -> letter((letter|digit|_)*) or keyword or type
    pub fn extract_letter_prefix_lexeme(&mut self, code: &JarvilCode) -> CoreToken {
        let mut forward_lexeme = self.index + 1;
        while forward_lexeme < code.len() {
            let next_char = code.get_char(forward_lexeme);
            if is_letter(&next_char) || next_char.is_ascii_digit() {
                // do nothing
            } else {
                let value_iter = code.token_value_as_iter(self.index, Some(forward_lexeme));
                self.index = forward_lexeme;
                return CoreToken::token_for_identifier(value_iter);
            }
            forward_lexeme += 1;
        }
        let value_iter = code.token_value_as_iter(self.index, Some(forward_lexeme));
        self.index = forward_lexeme;
        CoreToken::token_for_identifier(value_iter)
    }

    // digit -> digit((digit)*(.digit(digit*)|empty))
    pub fn extract_digit_prefix_lexeme(&mut self, code: &JarvilCode) -> CoreToken {
        let mut forward_lexeme = self.index + 1;
        let mut state: usize = 0;
        while forward_lexeme < code.len() {
            let next_char = code.get_char(forward_lexeme);
            match state {
                0 => {
                    if next_char.is_ascii_digit() {
                        // do nothing
                    } else if next_char == '.' {
                        state = 1;
                    } else {
                        self.index = forward_lexeme;
                        return CoreToken::INTEGER;
                    }
                }
                1 => {
                    if next_char.is_ascii_digit() {
                        state = 2;
                    } else {
                        self.index = forward_lexeme - 1;
                        return CoreToken::INTEGER;
                    }
                }
                2 => {
                    if next_char.is_ascii_digit() {
                        // do nothing
                    } else {
                        self.index = forward_lexeme;
                        return CoreToken::FLOATING_POINT_NUMBER;
                    }
                }
                _ => {
                    unreachable!("any state other than 0, 1, 2 and 3 is not reachable")
                }
            }
            forward_lexeme += 1;
        }
        match state {
            0 => {
                self.index = forward_lexeme;
                CoreToken::INTEGER
            }
            1 => {
                self.index = forward_lexeme - 1;
                CoreToken::INTEGER
            }
            2 => {
                self.index = forward_lexeme;
                CoreToken::FLOATING_POINT_NUMBER
            }
            _ => unreachable!("any state other than 0, 1, and 2 is not reachable"),
        }
    }

    // : -> :, ::
    pub fn extract_colon_prefix_lexeme(&mut self, code: &JarvilCode) -> CoreToken {
        let forward_lexeme = self.index + 1;
        if forward_lexeme < code.len() {
            let next_char = code.get_char(forward_lexeme);
            match next_char {
                ':' => {
                    self.index = forward_lexeme + 1;
                    CoreToken::DOUBLE_COLON
                }
                _ => {
                    self.index += 1;
                    CoreToken::COLON
                }
            }
        } else {
            self.index += 1;
            CoreToken::COLON
        }
    }

    pub fn log_invalid_char_error(&mut self, range: TextRange) {
        self.errors
            .push(Diagnostics::InvalidChar(InvalidCharError::new(range)));
    }

    pub fn log_no_closing_symbol_error(&mut self, expected_symbol: &'static str, range: TextRange) {
        self.errors
            .push(Diagnostics::NoClosingSymbol(NoClosingSymbolError::new(
                expected_symbol,
                range,
            )));
    }
}
