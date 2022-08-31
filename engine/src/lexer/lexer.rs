use super::token::LexicalErrorKind;
use crate::code::Code;
use crate::error::{
    core::{JarvilError, JarvilErrorKind},
    helper::LexicalErrorData,
};
use crate::lexer::helper;
use crate::lexer::token::CoreToken;
use crate::lexer::token::Token;
use std::convert::TryFrom;
use std::mem;
use std::rc::Rc;
use std::vec;
use text_size::TextRange;
use text_size::TextSize;

pub trait Lexer {
    fn tokenize(&mut self, code: &mut Code) -> (Vec<Token>, Vec<JarvilError>);
}

pub struct CoreLexer {
    pub begin_lexeme: usize,
    pub line_number: usize,
    pub code_lines: Vec<usize>,
    pub line_start_index: usize,
    pub lexical_errors_data: Vec<LexicalErrorData>,
}

impl Lexer for CoreLexer {
    fn tokenize(&mut self, code: &mut Code) -> (Vec<Token>, Vec<JarvilError>) {
        let mut token_vec: Vec<Token> = Vec::new();
        token_vec.push(Token {
            line_number: self.line_number,
            core_token: CoreToken::NEWLINE,
            range: TextRange::new(
                TextSize::try_from(0 as usize).unwrap(),
                TextSize::try_from(0 as usize).unwrap(),
            ),
            trivia: None,
        });
        let mut trivia_vec: Vec<Token> = vec![];
        while self.begin_lexeme < code.len() {
            let mut token = self.extract_lexeme(&code);
            match token.core_token {
                CoreToken::BLANK => trivia_vec.push(token),
                CoreToken::SINGLE_LINE_COMMENT => trivia_vec.push(token),
                CoreToken::BLOCK_COMMENT => trivia_vec.push(token),
                _ => {
                    if trivia_vec.len() > 0 {
                        token.set_trivia(mem::take(&mut trivia_vec));
                    }
                    token_vec.push(token)
                }
            }
        }
        self.code_lines.push(self.line_start_index);
        code.set_code_lines(mem::take(&mut self.code_lines));
        let mut token = Token {
            line_number: self.line_number,
            core_token: CoreToken::ENDMARKER,
            range: TextRange::new(
                TextSize::try_from(code.len()).unwrap(),
                TextSize::try_from(code.len()).unwrap(),
            ),
            trivia: None,
        };
        if trivia_vec.len() > 0 {
            token.set_trivia(mem::take(&mut trivia_vec));
        }
        token_vec.push(token);
        let errors = self.log_all_lexical_errors(code);
        (token_vec, errors)
    }
}

impl CoreLexer {
    pub fn new() -> Self {
        CoreLexer {
            begin_lexeme: 0,
            line_number: 1,
            code_lines: vec![],
            line_start_index: 0,
            lexical_errors_data: vec![],
        }
    }

    fn extract_lexeme(&mut self, code: &Code) -> Token {
        let begin_lexeme = &mut self.begin_lexeme;
        let line_number = &mut self.line_number;
        let code_lines = &mut self.code_lines;
        let line_start_index = &mut self.line_start_index;

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
                if helper::is_letter(&c) {
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
            line_number: start_line_number,
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
                    self.log_invalid_char_lexical_error(&token, &lexical_err_value.1);
                }
                LexicalErrorKind::NO_CLOSING_SYMBOLS => {
                    self.log_no_closing_symbols_lexical_error(
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

    pub fn log_invalid_char_lexical_error(
        &mut self,
        invalid_token: &Token,
        err_message: &Rc<String>,
    ) {
        self.lexical_errors_data
            .push(LexicalErrorData::new_with_invalid_char(
                invalid_token,
                err_message,
            ))
    }

    pub fn log_no_closing_symbols_lexical_error(
        &mut self,
        start_line_number: usize,
        end_line_number: usize,
        err_message: &Rc<String>,
    ) {
        self.lexical_errors_data
            .push(LexicalErrorData::new_with_no_closing_symbols(
                start_line_number,
                end_line_number,
                err_message,
            ))
    }

    pub fn log_all_lexical_errors(&mut self, code: &Code) -> Vec<JarvilError> {
        let mut errors: Vec<JarvilError> = vec![];
        for error_data in &self.lexical_errors_data {
            let err: JarvilError;
            match error_data {
                LexicalErrorData::INVALID_CHAR(invalid_char_lexical_error_data) => {
                    let invalid_token = invalid_char_lexical_error_data.invalid_token.clone();
                    let err_str = invalid_char_lexical_error_data.err_message.clone();
                    let (code_line, line_start_index, line_number, err_index) =
                        code.line_data(invalid_token.line_number, invalid_token.index());
                    err = JarvilError::form_single_line_error(
                        err_index,
                        err_index + 1,
                        line_number,
                        line_start_index,
                        code_line,
                        err_str.to_string(),
                        JarvilErrorKind::LEXICAL_ERROR,
                    );
                }
                LexicalErrorData::NO_CLOSING_SYMBOLS(no_closing_symbols_lexical_error_data) => {
                    let start_line_number = no_closing_symbols_lexical_error_data.start_line_number;
                    let end_line_number = no_closing_symbols_lexical_error_data.end_line_number;
                    let err_str = no_closing_symbols_lexical_error_data.err_message.clone();
                    err = JarvilError::form_multi_line_error(
                        start_line_number,
                        end_line_number,
                        &code,
                        err_str.to_string(),
                        JarvilErrorKind::LEXICAL_ERROR,
                    );
                }
            }
            errors.push(err);
        }
        // context::set_errors(errors);
        let _errors_data = mem::take(&mut self.lexical_errors_data);
        errors
    }
}
