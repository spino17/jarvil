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
    SLASH,              // '/'
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