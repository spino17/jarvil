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
    code_lines: Vec<(Rc<String>, usize)>,
    line_start_index: usize,
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

    pub fn extract_lexeme(&mut self, code: &Vec<char>) -> Result<Token, LexicalError> {
        Token::extract_lexeme(&mut self.begin_lexeme, &mut self.line_number, code, &mut self.code_lines, &mut self.line_start_index)
    }

    pub fn get_code_lines(self) -> Vec<(Rc<String>, usize)> {
        self.code_lines
    }
}

impl Lexer for CoreLexer {
    fn tokenize(&mut self, code: Vec<char>) -> Result<Vec<Token>, LexicalError> {
        let mut token_vec: Vec<Token> = Vec::new();
        token_vec.push(Token {
            line_number: self.line_number,
            core_token: CoreToken::NEWLINE,
            name: Rc::new(String::from("\n")),
            start_index: 0,
            end_index: 0,
        });
        while self.begin_lexeme < code.len() {
            let token = self.extract_lexeme(&code)?;
            token_vec.push(token);
            /*
            match token.core_token {
                
                // ignore single line and block comments
                CoreToken::SINGLE_LINE_COMMENT => {
                    // replace a single line comment with a newline token (to maintain the block indentation)
                    token_vec.push(Token{
                        line_number: token.line_number,
                        core_token: CoreToken::NEWLINE,
                        name: Rc::new(String::from("\n")),
                        start_index: token.start_index,
                        end_index: token.end_index,
                    })
                },
                CoreToken::BLOCK_COMMENT => continue,
                // CoreToken::BLANK => continue,
                _ => {
                    
                }
            }
             */
        }
        /*
        self.code_lines.push((Rc::new(code[self.line_start_index..].iter().collect()), self.line_start_index));
        token_vec.push(Token {
            line_number: self.line_number,
            core_token: CoreToken::NEWLINE,
            name: Rc::new(String::from("\n")),
            start_index: code.len(),
            end_index: code.len(),
        });
         */
        let mut code_str: String = code[self.line_start_index..].iter().collect();
        code_str.push(' ');
        self.code_lines.push((Rc::new(code_str), self.line_start_index));
        token_vec.push(Token {
            line_number: self.line_number,
            core_token: CoreToken::ENDMARKER,
            name: Rc::new(String::from("endmarker")),
            start_index: code.len(),
            end_index: code.len(),
        });
        Ok(token_vec)
    }
}