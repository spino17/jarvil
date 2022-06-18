use crate::lexer::token::Token;
use crate::errors::LexicalError;
use crate::lexer::token::CoreToken;
use std::rc::Rc;

pub trait Lexer {
    fn tokenize(&mut self, code: Vec<char>) -> Result<Vec<Token>, LexicalError>;
}

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
}

impl Lexer for CoreLexer {
    fn tokenize(&mut self, code: Vec<char>) -> Result<Vec<Token>, LexicalError> {
        let mut token_vec: Vec<Token> = Vec::new();
        token_vec.push(Token {
            line_number: self.line_number,
            core_token: CoreToken::NEWLINE,
            name: Rc::new(String::from("\n"))
        });
        while self.begin_lexeme < code.len() {
            let token = self.extract_lexeme(&code)?;
            match token.core_token {
                
                // ignore single line and block comments
                CoreToken::SINGLE_LINE_COMMENT => {
                    // replace a single line comment with a newline token (to maintain the block indentation)
                    token_vec.push(Token{
                        line_number: token.line_number,
                        core_token: CoreToken::NEWLINE,
                        name: Rc::new(String::from("\n"))
                    })
                },
                CoreToken::BLOCK_COMMENT => continue,
                // CoreToken::BLANK => continue,
                _ => {
                    token_vec.push(token)
                }
            }
        }
        token_vec.push(Token {
            line_number: self.line_number,
            core_token: CoreToken::NEWLINE,
            name: Rc::new(String::from("\n"))
        });
        token_vec.push(Token {
            line_number: self.line_number,
            core_token: CoreToken::ENDMARKER,
            name: Rc::new(String::from("endmarker"))
        });
        Ok(token_vec)
    }
}