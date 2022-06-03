use crate::{lexer::token::{Token, self}, errors::LexicalError};

// TODO - while lexical phase, when traversing through the string, keep account for \n so as to track the line number
// on which a specific token lies.
pub struct CoreLexer {
    begin_lexeme: usize,
    line_number: usize,
}

impl CoreLexer {
    pub fn new() -> Self {
        CoreLexer {
            begin_lexeme: 0,
            line_number: 1,
        }
    }

    pub fn extract_lexeme(&mut self, code: &Vec<char>) -> Result<Token, LexicalError> {
        Token::extract_lexeme(&mut self.begin_lexeme, &mut self.line_number, code)
    }

    pub fn scan(&mut self, code: Vec<char>) -> Result<(), LexicalError> {
        let mut token_vec: Vec<Token> = Vec::new();
        while self.begin_lexeme < code.len() {
            let token = self.extract_lexeme(&code)?;
            println!("{:?}", token);
            token_vec.push(token);
        }
        Ok(())
    }
}

impl Lexer for CoreLexer {
    fn scan(&mut self, code: Vec<char>) -> Box<dyn Iterator<Item=Token>> {
        let mut token_vec: Vec<Token> = Vec::new();
        while self.begin_lexeme < code.len() {
            println!("{}", self.begin_lexeme);
            // self.begin_lexeme = self.begin_lexeme + 1;
        }
        todo!()
    }
}

pub trait Lexer {
    fn scan(&mut self, code: Vec<char>) -> Box<dyn Iterator<Item=Token>>;
}