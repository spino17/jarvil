use crate::errors::LexicalError;

pub struct TokenValue(String);

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
    ADD,                // '+'
    INCREMENT,          // '++'
    SUBTRACT,           // '-'
    DECREMENT,          // '--'
    MULTIPLY,           // '*'
    EXPONENT,           // '**'
    DIVISION,           // '/'

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
    DOUBLESLASH,        // '//'
    LCOMMENT,           // '/*'
    RCOMMENT,           // '*/'
    DOT,                // '.'

    // comparison
    ASSIGN,             // '='
    EQUALS,             // '=='
    GREATER_EQUAL,      // '>='
    GREATER,            // '>'
    LESS_EQUAL,         // '<='
    LESS,               // '<'

    // others
    NUMBER(TokenValue),
    IDENTIFIER(TokenValue),
    LITERAL(TokenValue),
}

pub struct Token {
    // TODO - implement all token types - if, else, elif, l_parentheses, r_parentheses, l_brace, r_brace, equal, double_equal,
    // greater_equal, greater, less_equal, less, semicolon, number, identifier, literal
    line_number: i64,
    core_token: CoreToken,
}

impl Token {
    pub fn new_with_name(line_number: i64, name: &str) -> Result<Self, LexicalError> {
        // large switch case to return appropiate token for the name
        let core_token: CoreToken = match name {
            "if"        => CoreToken::IF,
            "else"      => CoreToken::ELSE,
            "elif"      => CoreToken::ELIF,
            "for"       => CoreToken::FOR,
            "while"     => CoreToken::WHILE,
            "struct"    => CoreToken::STRUCT,
            "and"       => CoreToken::AND,
            "not"       => CoreToken::NOT,
            "or"        => CoreToken::IS,
            "+"         => CoreToken::ADD,
            "++"        => CoreToken::INCREMENT,
            "-"         => CoreToken::SUBTRACT,
            "--"        => CoreToken::DECREMENT,
            "*"         => CoreToken::MULTIPLY,
            "**"        => CoreToken::EXPONENT,
            "/"         => CoreToken::DIVISION,
            "("         => CoreToken::LPAREN,
            ")"         => CoreToken::RPAREN,
            "{"         => CoreToken::LBRACE,
            "}"         => CoreToken::RBRACE,
            "["         => CoreToken::LSQUARE,
            "]"         => CoreToken::RSQUARE,
            ";"         => CoreToken::SEMICOLON,
            ":"         => CoreToken::COLON,
            ","         => CoreToken::COMMA,
            "//"        => CoreToken::DOUBLESLASH,
            "/*"        => CoreToken::LCOMMENT,
            "*/"        => CoreToken::RCOMMENT,
            "."         => CoreToken::DOT,
            "="         => CoreToken::ASSIGN,
            "=="        => CoreToken::EQUALS,
            ">="        => CoreToken::GREATER_EQUAL,
            ">"         => CoreToken::GREATER,
            "<="        => CoreToken::LESS_EQUAL,
            "<"         => CoreToken::LESS,
            _           => {
                return Err(LexicalError{})
            }
        };
        Ok(Token{
            line_number,
            core_token,
        })
    }

    pub fn new_with_name_and_value(line_number: i64, name: &str, value: &str) -> Result<Self, LexicalError> {
        let token_value = TokenValue(String::from(value));
        let core_token = match name {
            "type"      => CoreToken::TYPE(token_value),
            "num"       => CoreToken::NUMBER(token_value),
            "id"        => CoreToken::IDENTIFIER(token_value),
            "literal"   => CoreToken::LITERAL(token_value),
            _           => {
                return Err(LexicalError{})
            }
        };
        Ok(Token{
            line_number,
            core_token,
        })
    }
}