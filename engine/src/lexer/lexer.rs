use crate::lexer::token::Token;

// TODO - while lexical phase, when traversing through the string, keep account for \n so as to track the line number
// on which a specific token lies.

pub struct CoreLexer {
    begin_lexeme: usize,
}

impl CoreLexer {
    pub fn new() -> Self {
        CoreLexer {
            begin_lexeme: 0,
        }
    }

    pub fn extract_lexeme(&mut self) -> Token {
        // try extracting a lexeme for all possiblity of terminals
        todo!()
    }
}

impl Lexer for CoreLexer {
    fn scan(&mut self, code: Vec<char>) -> Box<dyn Iterator<Item=Token>> {
        let mut token_vec: Vec<Token> = Vec::new();
        while self.begin_lexeme < code.len() {
            println!("{}", self.begin_lexeme);
            // self.begin_lexeme = self.begin_lexeme + 1;
            let token = self.extract_lexeme();
            token_vec.push(token);
        }
        todo!()
    }
}

pub trait Lexer {
    fn scan(&mut self, code: Vec<char>) -> Box<dyn Iterator<Item=Token>>;
}