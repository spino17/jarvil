use text_size::TextRange;
use text_size::TextSize;

use crate::code::Code;
use crate::context;
use crate::errors::JarvilError;
use crate::errors::JarvilErrorKind;
use crate::errors::LexicalErrorData;
use crate::lexer::token::CoreToken;
use crate::lexer::token::Token;
use std::convert::TryFrom;
use std::mem;
use std::rc::Rc;
use std::vec;

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

    pub fn extract_lexeme(&mut self, code: &Code) -> Token {
        Token::extract_lexeme(self, code)
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
