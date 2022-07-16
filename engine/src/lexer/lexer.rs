use crate::constants::common::ENDMARKER;
use crate::errors::LexicalErrorData;
use crate::lexer::token::Token;
use crate::lexer::token::CoreToken;
use std::rc::Rc;
use std::vec;
use std::mem;

pub trait Lexer {
    fn tokenize(&mut self, code: Vec<char>) -> Vec<Token>;
}

pub struct CoreLexer {
    pub begin_lexeme: usize,
    pub line_number: usize,
    pub code_lines: Vec<(Rc<String>, usize)>,
    pub line_start_index: usize,
    pub lexical_errors: Vec<LexicalErrorData>,
    //
}

impl CoreLexer {
    pub fn new() -> Self {
        CoreLexer {
            begin_lexeme: 0,
            line_number: 1,
            code_lines: vec![],
            line_start_index: 0,
            lexical_errors: vec![],
        }
    }

    pub fn extract_lexeme(&mut self, code: &Vec<char>) -> Token {
        Token::extract_lexeme(self, code)
    }

    pub fn get_code_lines(self) -> Vec<(Rc<String>, usize)> {
        self.code_lines
    }

    pub fn log_invalid_char_lexical_error(&mut self, line_number: usize, invalid_token: &Token, err_message: &Rc<String>) {
        self.lexical_errors.push(LexicalErrorData::new_with_invalid_char(line_number, invalid_token, err_message))
    }

    pub fn log_no_closing_symbols_lexical_error(&mut self, start_line_number: usize, end_line_number: usize, 
        err_message: &Rc<String>) {
        self.lexical_errors.push(LexicalErrorData::new_with_no_closing_symbols(start_line_number, end_line_number, err_message))
    }
}

impl Lexer for CoreLexer {
    fn tokenize(&mut self, code: Vec<char>) -> Vec<Token> {
        let mut token_vec: Vec<Token> = Vec::new();
        token_vec.push(Token {
            line_number: self.line_number,
            core_token: CoreToken::NEWLINE,
            name: Rc::new(String::from("\n")),
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
                CoreToken::SINGLE_LINE_COMMENT(_) => trivia_vec.push(token),
                CoreToken::BLOCK_COMMENT(_) => trivia_vec.push(token),
                _ => {
                    if trivia_vec.len() > 0 {
                        token.set_trivia(mem::take(&mut trivia_vec));
                    }
                    token_vec.push(token)
                }
            }
        }
        let mut code_str: String = code[self.line_start_index..].iter().collect();
        code_str.push(' ');
        self.code_lines.push((Rc::new(code_str), self.line_start_index));
        let mut token = Token {
            line_number: self.line_number,
            core_token: CoreToken::ENDMARKER,
            name: Rc::new(String::from(ENDMARKER)),
            start_index: code.len(),
            end_index: code.len(),
            trivia: None,
            parent: None,
        };
        if trivia_vec.len() > 0 {
            token.set_trivia(mem::take(&mut trivia_vec));
        }
        token_vec.push(token);
        // fill up the errors
        token_vec
    }
}