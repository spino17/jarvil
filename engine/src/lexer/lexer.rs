use crate::constants::common::ENDMARKER;
use crate::lexer::token::Token;
use crate::lexer::token::CoreToken;
use std::rc::Rc;
use std::vec;
use std::mem;

pub trait Lexer {
    fn tokenize(&mut self, code: Vec<char>) -> Vec<Token>;
}

pub struct CoreLexer {
    begin_lexeme: usize,
    line_number: usize,
    code_lines: Vec<(Rc<String>, usize)>,
    line_start_index: usize,
    // errors: Vec<ParseError>
}

impl CoreLexer {
    pub fn new() -> Self {
        CoreLexer {
            begin_lexeme: 0,
            line_number: 1,
            code_lines: vec![],
            line_start_index: 0,
        }
    }

    pub fn extract_lexeme(&mut self, code: &Vec<char>) -> Token {
        Token::extract_lexeme(&mut self.begin_lexeme, &mut self.line_number, code, &mut self.code_lines, 
            &mut self.line_start_index)
    }

    pub fn get_code_lines(self) -> Vec<(Rc<String>, usize)> {
        self.code_lines
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
                    println!("{:?}", token);
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
        println!("{:?}", token);
        token_vec.push(token);
        token_vec
    }
}