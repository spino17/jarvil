use crate::errors::{LexicalError, SemanticError};
use std::rc::Rc;
use crate::env::Env;

#[derive(Debug)]
pub struct TokenValue(pub Rc<String>);

#[derive(Debug)]
pub enum CoreToken {

    // conditionals
    IF,                 // 'if'
    ELSE,               // 'else'
    ELIF,               // 'elif'

    // loops
    FOR,                // 'for'
    WHILE,              // 'while'

    // types
    STRUCT,             // 'struct'
    TYPE(TokenValue),   // 'type'

    // bitwise operators
    AND,                // 'and'
    NOT,                // 'not'
    OR,                 // 'or'
    IS,                 // 'is'

    // operators
    PLUS,               // '+'
    DOUBLE_PLUS,        // '++'
    MINUS,              // '-'
    DOUBLE_MINUS,       // '--'
    STAR,               // '*'
    DOUBLE_STAR,        // '**'
    SLASH,              // '/'
    DOUBLE_SLASH,       // '//'

    // wrappers
    LPAREN,             // '('
    RPAREN,             // ')'
    LBRACE,             // '{'
    RBRACE,             // '}'
    LSQUARE,            // '['
    RSQUARE,            // ']'

    // delimiters
    SEMICOLON,          // ';'
    COLON,              // ':'
    COMMA,              // ','
    LCOMMENT,           // '/*'
    RCOMMENT,           // '*/'
    DOT,                // '.'

    // comparison
    EQUAL,              // '='
    DOUBLE_EQUAL,       // '=='
    GREATER_EQUAL,      // '>='
    GREATER,            // '>'
    LESS_EQUAL,         // '<='
    LESS,               // '<'

    // others
    NUMBER(TokenValue),
    IDENTIFIER(TokenValue),
    LITERAL(TokenValue),
}

#[derive(Debug)]
pub struct Token {
    line_number: i64,
    core_token: CoreToken,
}

impl Token {
    pub fn new_with_name(line_number: i64, name: &str) -> Self {
        let core_token: CoreToken = match name {
            "if"        =>      CoreToken::IF,
            "else"      =>      CoreToken::ELSE,
            "elif"      =>      CoreToken::ELIF,
            "for"       =>      CoreToken::FOR,
            "while"     =>      CoreToken::WHILE,
            "struct"    =>      CoreToken::STRUCT,
            "and"       =>      CoreToken::AND,
            "not"       =>      CoreToken::NOT,
            "or"        =>      CoreToken::OR,
            "is"        =>      CoreToken::IS,
            "+"         =>      CoreToken::PLUS,
            "++"        =>      CoreToken::DOUBLE_PLUS,
            "-"         =>      CoreToken::MINUS,
            "--"        =>      CoreToken::DOUBLE_MINUS,
            "*"         =>      CoreToken::STAR,
            "**"        =>      CoreToken::DOUBLE_STAR,
            "/"         =>      CoreToken::SLASH,
            "("         =>      CoreToken::LPAREN,
            ")"         =>      CoreToken::RPAREN,
            "{"         =>      CoreToken::LBRACE,
            "}"         =>      CoreToken::RBRACE,
            "["         =>      CoreToken::LSQUARE,
            "]"         =>      CoreToken::RSQUARE,
            ";"         =>      CoreToken::SEMICOLON,
            ":"         =>      CoreToken::COLON,
            ","         =>      CoreToken::COMMA,
            "//"        =>      CoreToken::DOUBLE_SLASH,
            "/*"        =>      CoreToken::LCOMMENT,
            "*/"        =>      CoreToken::RCOMMENT,
            "."         =>      CoreToken::DOT,
            "="         =>      CoreToken::EQUAL,
            "=="        =>      CoreToken::DOUBLE_EQUAL,
            ">="        =>      CoreToken::GREATER_EQUAL,
            ">"         =>      CoreToken::GREATER,
            "<="        =>      CoreToken::LESS_EQUAL,
            "<"         =>      CoreToken::LESS,
            _           =>      {
                panic!("{:?}", LexicalError{})  // This is a bug!
            }
        };
        Token {
            line_number,
            core_token,
        }
    }

    pub fn new_with_name_and_value(line_number: i64, name: &str, value: &str) -> Self {
        let token_value = TokenValue(Rc::new(String::from(value)));
        let core_token = match name {
            "type"      =>      CoreToken::TYPE(token_value),
            "num"       =>      CoreToken::NUMBER(token_value),
            "id"        =>      CoreToken::IDENTIFIER(token_value),
            "literal"   =>      CoreToken::LITERAL(token_value),
            _           =>      {
                panic!("{:?}", LexicalError{})  // This is a bug!
            }
        };
        Token {
            line_number,
            core_token,
        }
    }

    pub fn get_value(&self) -> Option<TokenValue> {
        match &self.core_token {
            CoreToken::TYPE(value) => {
                Some(TokenValue(value.0.clone()))
            },
            CoreToken::NUMBER(value) => {
                Some(TokenValue(value.0.clone()))
            },
            CoreToken::IDENTIFIER(value) => {
                Some(TokenValue(value.0.clone()))
            }
            CoreToken::LITERAL(value) => {
                Some(TokenValue(value.0.clone()))
            },
            _ => {
                None
            }
        }
    }
    
    pub fn is_type(&self, env: &Env) -> Result<bool, SemanticError> {
        match &self.core_token {
            CoreToken::TYPE(_) => {
                Ok(true)
            },
            CoreToken::IDENTIFIER(token_value) => {
                let symbol_table = env.check_declaration(token_value)?;
                Ok(symbol_table.is_type())
            },
            _ => {
                Ok(false)
            }
        }
    }
}