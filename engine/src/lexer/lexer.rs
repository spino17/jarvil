use crate::code::Code;
use crate::constants::common::IDENTIFIER;
use crate::errors::LexicalErrorData;
use crate::errors::JarvilError;
use crate::errors::ParseErrorKind;
use crate::lexer::token::Token;
use crate::lexer::token::CoreToken;
use std::rc::Rc;
use std::vec;
use std::mem;
use crate::context;

pub trait Lexer {
    fn tokenize(&mut self, code: &mut Code) -> Vec<Token>;
}

pub struct CoreLexer {
    pub begin_lexeme: usize,
    pub line_number: usize,
    pub code_lines: Vec<usize>,
    pub line_start_index: usize,
    pub lexical_errors_data: Vec<LexicalErrorData>,  // temp storage for lexical error data uptill code_lines are built
    // pub lexical_errors: Vec<ParseError>,
}

impl Lexer for CoreLexer {
    fn tokenize(&mut self, code: &mut Code) -> Vec<Token> {
        let mut token_vec: Vec<Token> = Vec::new();
        token_vec.push(Token {
            line_number: self.line_number,
            core_token: CoreToken::NEWLINE,
            start_index: 0,
            end_index: 0,
            trivia: None,
            parent: None,
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
            start_index: code.len(),
            end_index: code.len(),
            trivia: None,
            parent: None,
        };
        if trivia_vec.len() > 0 {
            token.set_trivia(mem::take(&mut trivia_vec));
        }
        token_vec.push(token);
        self.log_all_lexical_errors(code);
        token_vec
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
            // lexical_errors: vec![],
        }
    }

    pub fn extract_lexeme(&mut self, code: &Code) -> Token {
        Token::extract_lexeme(self, code)
    }

    /*
    pub fn get_lexical_data_useful_for_parser(self) -> Vec<ParseError> {
        self.lexical_errors
    }
     */

    pub fn log_invalid_char_lexical_error(&mut self, invalid_token: &Token, err_message: &Rc<String>) {
        self.lexical_errors_data.push(LexicalErrorData::new_with_invalid_char(invalid_token, err_message))
    }

    pub fn log_no_closing_symbols_lexical_error(&mut self, start_line_number: usize, end_line_number: usize, 
        err_message: &Rc<String>) {
        self.lexical_errors_data.push(LexicalErrorData::new_with_no_closing_symbols(start_line_number, end_line_number, err_message))
    }

    pub fn log_all_lexical_errors(&mut self, code: &Code) {
        let mut errors: Vec<JarvilError> = vec![];
        for error_data in &self.lexical_errors_data {
            let error: JarvilError;
            match error_data {
                LexicalErrorData::INVALID_CHAR(invalid_char_lexical_error_data) => {
                    let invalid_token = invalid_char_lexical_error_data.invalid_token.clone();
                    let err_str = invalid_char_lexical_error_data.err_message.clone();
                    let (code_line, line_start_index, line_number, err_index)
                    = code.line_data(invalid_token.line_number, invalid_token.index());
                    let err_message = JarvilError::form_single_line_single_pointer_error(
                        err_index, line_number, line_start_index, code_line, err_str.to_string(),
                        ParseErrorKind::LEXICAL_ERROR
                    );
                    error = JarvilError::new(line_number, line_number, err_message);
                },
                LexicalErrorData::NO_CLOSING_SYMBOLS(
                    no_closing_symbols_lexical_error_data
                ) => {
                    let start_line_number = no_closing_symbols_lexical_error_data.start_line_number;
                    let end_line_number = no_closing_symbols_lexical_error_data.end_line_number;
                    let err_str = no_closing_symbols_lexical_error_data.err_message.clone();
                    let code_lines = code.lines(start_line_number, end_line_number);
                    let err_message = JarvilError::form_multi_line_error(start_line_number, end_line_number, 
                        code_lines, err_str.to_string(), ParseErrorKind::LEXICAL_ERROR);
                    error = JarvilError::new(start_line_number, end_line_number, err_message);
                }
            }
            errors.push(error);
        }
        context::set_errors(errors);
        let _errors_data = mem::take(&mut self.lexical_errors_data);
    }
}