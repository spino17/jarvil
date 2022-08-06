pub mod token {
    #[macro_use]
    use jarvil_macros::Tokenify;
    use super::helper::is_letter;
    use super::lexer::CoreLexer;
    use crate::ast::ast::ASTNode;
    use crate::code::Code;
    use crate::constants::common::{
        AND, ATOMIC_TYPE, BLANK, BLOCK_COMMENT, BREAK, COLON, COMMA, CONTINUE, DASH, DEF, DOT,
        DOUBLE_COLON, DOUBLE_EQUAL, DOUBLE_STAR, ELIF, ELSE, ENDMARKER, EQUAL, FALSE,
        FLOATING_POINT_NUMBER, FOR, FUNC, GREATER_EQUAL, IDENTIFIER, IF, IMPL, IN, INTEGER,
        INTERFACE_KEYWORD, LBRACE, LBRACKET, LESS_EQUAL, LET, LEXICAL_ERROR, LITERAL, LPAREN,
        LSQUARE, NEWLINE, NOT, NOT_EQUAL, OR, PLUS, RBRACE, RBRACKET, RETURN, RIGHT_ARROW, RPAREN,
        RSQUARE, SELF, SEMICOLON, SINGLE_LINE_COMMENT, SLASH, STAR, TRUE, TYPE_KEYWORD, WHILE,
    };
    use crate::lexer::helper;
    use std::rc::Rc;
    pub enum CoreToken {
        IF,
        ELSE,
        ELIF,
        FOR,
        WHILE,
        CONTINUE,
        BREAK,
        DEF,
        RETURN,
        FUNC,
        TYPE_KEYWORD,
        ATOMIC_TYPE,
        LET,
        SELF,
        IMPL,
        INTERFACE_KEYWORD,
        AND,
        NOT,
        OR,
        IN,
        TRUE,
        FALSE,
        PLUS,
        DASH,
        RIGHT_ARROW,
        STAR,
        DOUBLE_STAR,
        SLASH,
        LPAREN,
        RPAREN,
        LBRACE,
        RBRACE,
        LSQUARE,
        RSQUARE,
        SEMICOLON,
        COLON,
        DOUBLE_COLON,
        COMMA,
        DOT,
        BLANK,
        NEWLINE,
        EQUAL,
        DOUBLE_EQUAL,
        LBRACKET,
        RBRACKET,
        LESS_EQUAL,
        GREATER_EQUAL,
        NOT_EQUAL,
        INTEGER,
        FLOATING_POINT_NUMBER,
        IDENTIFIER,
        LITERAL,
        SINGLE_LINE_COMMENT,
        BLOCK_COMMENT,
        ENDMARKER,
        LEXICAL_ERROR((LexicalErrorKind, Rc<String>)),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreToken {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreToken::IF => ::core::fmt::Formatter::write_str(f, "IF"),
                CoreToken::ELSE => ::core::fmt::Formatter::write_str(f, "ELSE"),
                CoreToken::ELIF => ::core::fmt::Formatter::write_str(f, "ELIF"),
                CoreToken::FOR => ::core::fmt::Formatter::write_str(f, "FOR"),
                CoreToken::WHILE => ::core::fmt::Formatter::write_str(f, "WHILE"),
                CoreToken::CONTINUE => ::core::fmt::Formatter::write_str(f, "CONTINUE"),
                CoreToken::BREAK => ::core::fmt::Formatter::write_str(f, "BREAK"),
                CoreToken::DEF => ::core::fmt::Formatter::write_str(f, "DEF"),
                CoreToken::RETURN => ::core::fmt::Formatter::write_str(f, "RETURN"),
                CoreToken::FUNC => ::core::fmt::Formatter::write_str(f, "FUNC"),
                CoreToken::TYPE_KEYWORD => ::core::fmt::Formatter::write_str(f, "TYPE_KEYWORD"),
                CoreToken::ATOMIC_TYPE => ::core::fmt::Formatter::write_str(f, "ATOMIC_TYPE"),
                CoreToken::LET => ::core::fmt::Formatter::write_str(f, "LET"),
                CoreToken::SELF => ::core::fmt::Formatter::write_str(f, "SELF"),
                CoreToken::IMPL => ::core::fmt::Formatter::write_str(f, "IMPL"),
                CoreToken::INTERFACE_KEYWORD => {
                    ::core::fmt::Formatter::write_str(f, "INTERFACE_KEYWORD")
                }
                CoreToken::AND => ::core::fmt::Formatter::write_str(f, "AND"),
                CoreToken::NOT => ::core::fmt::Formatter::write_str(f, "NOT"),
                CoreToken::OR => ::core::fmt::Formatter::write_str(f, "OR"),
                CoreToken::IN => ::core::fmt::Formatter::write_str(f, "IN"),
                CoreToken::TRUE => ::core::fmt::Formatter::write_str(f, "TRUE"),
                CoreToken::FALSE => ::core::fmt::Formatter::write_str(f, "FALSE"),
                CoreToken::PLUS => ::core::fmt::Formatter::write_str(f, "PLUS"),
                CoreToken::DASH => ::core::fmt::Formatter::write_str(f, "DASH"),
                CoreToken::RIGHT_ARROW => ::core::fmt::Formatter::write_str(f, "RIGHT_ARROW"),
                CoreToken::STAR => ::core::fmt::Formatter::write_str(f, "STAR"),
                CoreToken::DOUBLE_STAR => ::core::fmt::Formatter::write_str(f, "DOUBLE_STAR"),
                CoreToken::SLASH => ::core::fmt::Formatter::write_str(f, "SLASH"),
                CoreToken::LPAREN => ::core::fmt::Formatter::write_str(f, "LPAREN"),
                CoreToken::RPAREN => ::core::fmt::Formatter::write_str(f, "RPAREN"),
                CoreToken::LBRACE => ::core::fmt::Formatter::write_str(f, "LBRACE"),
                CoreToken::RBRACE => ::core::fmt::Formatter::write_str(f, "RBRACE"),
                CoreToken::LSQUARE => ::core::fmt::Formatter::write_str(f, "LSQUARE"),
                CoreToken::RSQUARE => ::core::fmt::Formatter::write_str(f, "RSQUARE"),
                CoreToken::SEMICOLON => ::core::fmt::Formatter::write_str(f, "SEMICOLON"),
                CoreToken::COLON => ::core::fmt::Formatter::write_str(f, "COLON"),
                CoreToken::DOUBLE_COLON => ::core::fmt::Formatter::write_str(f, "DOUBLE_COLON"),
                CoreToken::COMMA => ::core::fmt::Formatter::write_str(f, "COMMA"),
                CoreToken::DOT => ::core::fmt::Formatter::write_str(f, "DOT"),
                CoreToken::BLANK => ::core::fmt::Formatter::write_str(f, "BLANK"),
                CoreToken::NEWLINE => ::core::fmt::Formatter::write_str(f, "NEWLINE"),
                CoreToken::EQUAL => ::core::fmt::Formatter::write_str(f, "EQUAL"),
                CoreToken::DOUBLE_EQUAL => ::core::fmt::Formatter::write_str(f, "DOUBLE_EQUAL"),
                CoreToken::LBRACKET => ::core::fmt::Formatter::write_str(f, "LBRACKET"),
                CoreToken::RBRACKET => ::core::fmt::Formatter::write_str(f, "RBRACKET"),
                CoreToken::LESS_EQUAL => ::core::fmt::Formatter::write_str(f, "LESS_EQUAL"),
                CoreToken::GREATER_EQUAL => ::core::fmt::Formatter::write_str(f, "GREATER_EQUAL"),
                CoreToken::NOT_EQUAL => ::core::fmt::Formatter::write_str(f, "NOT_EQUAL"),
                CoreToken::INTEGER => ::core::fmt::Formatter::write_str(f, "INTEGER"),
                CoreToken::FLOATING_POINT_NUMBER => {
                    ::core::fmt::Formatter::write_str(f, "FLOATING_POINT_NUMBER")
                }
                CoreToken::IDENTIFIER => ::core::fmt::Formatter::write_str(f, "IDENTIFIER"),
                CoreToken::LITERAL => ::core::fmt::Formatter::write_str(f, "LITERAL"),
                CoreToken::SINGLE_LINE_COMMENT => {
                    ::core::fmt::Formatter::write_str(f, "SINGLE_LINE_COMMENT")
                }
                CoreToken::BLOCK_COMMENT => ::core::fmt::Formatter::write_str(f, "BLOCK_COMMENT"),
                CoreToken::ENDMARKER => ::core::fmt::Formatter::write_str(f, "ENDMARKER"),
                CoreToken::LEXICAL_ERROR(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "LEXICAL_ERROR", &__self_0)
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreToken {
        #[inline]
        fn clone(&self) -> CoreToken {
            match self {
                CoreToken::IF => CoreToken::IF,
                CoreToken::ELSE => CoreToken::ELSE,
                CoreToken::ELIF => CoreToken::ELIF,
                CoreToken::FOR => CoreToken::FOR,
                CoreToken::WHILE => CoreToken::WHILE,
                CoreToken::CONTINUE => CoreToken::CONTINUE,
                CoreToken::BREAK => CoreToken::BREAK,
                CoreToken::DEF => CoreToken::DEF,
                CoreToken::RETURN => CoreToken::RETURN,
                CoreToken::FUNC => CoreToken::FUNC,
                CoreToken::TYPE_KEYWORD => CoreToken::TYPE_KEYWORD,
                CoreToken::ATOMIC_TYPE => CoreToken::ATOMIC_TYPE,
                CoreToken::LET => CoreToken::LET,
                CoreToken::SELF => CoreToken::SELF,
                CoreToken::IMPL => CoreToken::IMPL,
                CoreToken::INTERFACE_KEYWORD => CoreToken::INTERFACE_KEYWORD,
                CoreToken::AND => CoreToken::AND,
                CoreToken::NOT => CoreToken::NOT,
                CoreToken::OR => CoreToken::OR,
                CoreToken::IN => CoreToken::IN,
                CoreToken::TRUE => CoreToken::TRUE,
                CoreToken::FALSE => CoreToken::FALSE,
                CoreToken::PLUS => CoreToken::PLUS,
                CoreToken::DASH => CoreToken::DASH,
                CoreToken::RIGHT_ARROW => CoreToken::RIGHT_ARROW,
                CoreToken::STAR => CoreToken::STAR,
                CoreToken::DOUBLE_STAR => CoreToken::DOUBLE_STAR,
                CoreToken::SLASH => CoreToken::SLASH,
                CoreToken::LPAREN => CoreToken::LPAREN,
                CoreToken::RPAREN => CoreToken::RPAREN,
                CoreToken::LBRACE => CoreToken::LBRACE,
                CoreToken::RBRACE => CoreToken::RBRACE,
                CoreToken::LSQUARE => CoreToken::LSQUARE,
                CoreToken::RSQUARE => CoreToken::RSQUARE,
                CoreToken::SEMICOLON => CoreToken::SEMICOLON,
                CoreToken::COLON => CoreToken::COLON,
                CoreToken::DOUBLE_COLON => CoreToken::DOUBLE_COLON,
                CoreToken::COMMA => CoreToken::COMMA,
                CoreToken::DOT => CoreToken::DOT,
                CoreToken::BLANK => CoreToken::BLANK,
                CoreToken::NEWLINE => CoreToken::NEWLINE,
                CoreToken::EQUAL => CoreToken::EQUAL,
                CoreToken::DOUBLE_EQUAL => CoreToken::DOUBLE_EQUAL,
                CoreToken::LBRACKET => CoreToken::LBRACKET,
                CoreToken::RBRACKET => CoreToken::RBRACKET,
                CoreToken::LESS_EQUAL => CoreToken::LESS_EQUAL,
                CoreToken::GREATER_EQUAL => CoreToken::GREATER_EQUAL,
                CoreToken::NOT_EQUAL => CoreToken::NOT_EQUAL,
                CoreToken::INTEGER => CoreToken::INTEGER,
                CoreToken::FLOATING_POINT_NUMBER => CoreToken::FLOATING_POINT_NUMBER,
                CoreToken::IDENTIFIER => CoreToken::IDENTIFIER,
                CoreToken::LITERAL => CoreToken::LITERAL,
                CoreToken::SINGLE_LINE_COMMENT => CoreToken::SINGLE_LINE_COMMENT,
                CoreToken::BLOCK_COMMENT => CoreToken::BLOCK_COMMENT,
                CoreToken::ENDMARKER => CoreToken::ENDMARKER,
                CoreToken::LEXICAL_ERROR(__self_0) => {
                    CoreToken::LEXICAL_ERROR(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for CoreToken {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for CoreToken {
        #[inline]
        fn eq(&self, other: &CoreToken) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
                && match (self, other) {
                    (CoreToken::LEXICAL_ERROR(__self_0), CoreToken::LEXICAL_ERROR(__arg1_0)) => {
                        *__self_0 == *__arg1_0
                    }
                    _ => true,
                }
        }
        #[inline]
        fn ne(&self, other: &CoreToken) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag != __arg1_tag
                || match (self, other) {
                    (CoreToken::LEXICAL_ERROR(__self_0), CoreToken::LEXICAL_ERROR(__arg1_0)) => {
                        *__self_0 != *__arg1_0
                    }
                    _ => false,
                }
        }
    }
    impl CoreToken {
        fn IF(&self) -> bool {
            match self {
                CoreToken::IF => true,
                _ => false,
            }
        }
        fn ELSE(&self) -> bool {
            match self {
                CoreToken::ELSE => true,
                _ => false,
            }
        }
        fn ELIF(&self) -> bool {
            match self {
                CoreToken::ELIF => true,
                _ => false,
            }
        }
        fn FOR(&self) -> bool {
            match self {
                CoreToken::FOR => true,
                _ => false,
            }
        }
        fn WHILE(&self) -> bool {
            match self {
                CoreToken::WHILE => true,
                _ => false,
            }
        }
        fn CONTINUE(&self) -> bool {
            match self {
                CoreToken::CONTINUE => true,
                _ => false,
            }
        }
        fn BREAK(&self) -> bool {
            match self {
                CoreToken::BREAK => true,
                _ => false,
            }
        }
        fn DEF(&self) -> bool {
            match self {
                CoreToken::DEF => true,
                _ => false,
            }
        }
        fn RETURN(&self) -> bool {
            match self {
                CoreToken::RETURN => true,
                _ => false,
            }
        }
        fn FUNC(&self) -> bool {
            match self {
                CoreToken::FUNC => true,
                _ => false,
            }
        }
        fn TYPE_KEYWORD(&self) -> bool {
            match self {
                CoreToken::TYPE_KEYWORD => true,
                _ => false,
            }
        }
        fn ATOMIC_TYPE(&self) -> bool {
            match self {
                CoreToken::ATOMIC_TYPE => true,
                _ => false,
            }
        }
        fn LET(&self) -> bool {
            match self {
                CoreToken::LET => true,
                _ => false,
            }
        }
        fn SELF(&self) -> bool {
            match self {
                CoreToken::SELF => true,
                _ => false,
            }
        }
        fn IMPL(&self) -> bool {
            match self {
                CoreToken::IMPL => true,
                _ => false,
            }
        }
        fn INTERFACE_KEYWORD(&self) -> bool {
            match self {
                CoreToken::INTERFACE_KEYWORD => true,
                _ => false,
            }
        }
        fn AND(&self) -> bool {
            match self {
                CoreToken::AND => true,
                _ => false,
            }
        }
        fn NOT(&self) -> bool {
            match self {
                CoreToken::NOT => true,
                _ => false,
            }
        }
        fn OR(&self) -> bool {
            match self {
                CoreToken::OR => true,
                _ => false,
            }
        }
        fn IN(&self) -> bool {
            match self {
                CoreToken::IN => true,
                _ => false,
            }
        }
        fn TRUE(&self) -> bool {
            match self {
                CoreToken::TRUE => true,
                _ => false,
            }
        }
        fn FALSE(&self) -> bool {
            match self {
                CoreToken::FALSE => true,
                _ => false,
            }
        }
        fn PLUS(&self) -> bool {
            match self {
                CoreToken::PLUS => true,
                _ => false,
            }
        }
        fn DASH(&self) -> bool {
            match self {
                CoreToken::DASH => true,
                _ => false,
            }
        }
        fn RIGHT_ARROW(&self) -> bool {
            match self {
                CoreToken::RIGHT_ARROW => true,
                _ => false,
            }
        }
        fn STAR(&self) -> bool {
            match self {
                CoreToken::STAR => true,
                _ => false,
            }
        }
        fn DOUBLE_STAR(&self) -> bool {
            match self {
                CoreToken::DOUBLE_STAR => true,
                _ => false,
            }
        }
        fn SLASH(&self) -> bool {
            match self {
                CoreToken::SLASH => true,
                _ => false,
            }
        }
        fn LPAREN(&self) -> bool {
            match self {
                CoreToken::LPAREN => true,
                _ => false,
            }
        }
        fn RPAREN(&self) -> bool {
            match self {
                CoreToken::RPAREN => true,
                _ => false,
            }
        }
        fn LBRACE(&self) -> bool {
            match self {
                CoreToken::LBRACE => true,
                _ => false,
            }
        }
        fn RBRACE(&self) -> bool {
            match self {
                CoreToken::RBRACE => true,
                _ => false,
            }
        }
        fn LSQUARE(&self) -> bool {
            match self {
                CoreToken::LSQUARE => true,
                _ => false,
            }
        }
        fn RSQUARE(&self) -> bool {
            match self {
                CoreToken::RSQUARE => true,
                _ => false,
            }
        }
        fn SEMICOLON(&self) -> bool {
            match self {
                CoreToken::SEMICOLON => true,
                _ => false,
            }
        }
        fn COLON(&self) -> bool {
            match self {
                CoreToken::COLON => true,
                _ => false,
            }
        }
        fn DOUBLE_COLON(&self) -> bool {
            match self {
                CoreToken::DOUBLE_COLON => true,
                _ => false,
            }
        }
        fn COMMA(&self) -> bool {
            match self {
                CoreToken::COMMA => true,
                _ => false,
            }
        }
        fn DOT(&self) -> bool {
            match self {
                CoreToken::DOT => true,
                _ => false,
            }
        }
        fn BLANK(&self) -> bool {
            match self {
                CoreToken::BLANK => true,
                _ => false,
            }
        }
        fn NEWLINE(&self) -> bool {
            match self {
                CoreToken::NEWLINE => true,
                _ => false,
            }
        }
        fn EQUAL(&self) -> bool {
            match self {
                CoreToken::EQUAL => true,
                _ => false,
            }
        }
        fn DOUBLE_EQUAL(&self) -> bool {
            match self {
                CoreToken::DOUBLE_EQUAL => true,
                _ => false,
            }
        }
        fn LBRACKET(&self) -> bool {
            match self {
                CoreToken::LBRACKET => true,
                _ => false,
            }
        }
        fn RBRACKET(&self) -> bool {
            match self {
                CoreToken::RBRACKET => true,
                _ => false,
            }
        }
        fn LESS_EQUAL(&self) -> bool {
            match self {
                CoreToken::LESS_EQUAL => true,
                _ => false,
            }
        }
        fn GREATER_EQUAL(&self) -> bool {
            match self {
                CoreToken::GREATER_EQUAL => true,
                _ => false,
            }
        }
        fn NOT_EQUAL(&self) -> bool {
            match self {
                CoreToken::NOT_EQUAL => true,
                _ => false,
            }
        }
        fn INTEGER(&self) -> bool {
            match self {
                CoreToken::INTEGER => true,
                _ => false,
            }
        }
        fn FLOATING_POINT_NUMBER(&self) -> bool {
            match self {
                CoreToken::FLOATING_POINT_NUMBER => true,
                _ => false,
            }
        }
        fn IDENTIFIER(&self) -> bool {
            match self {
                CoreToken::IDENTIFIER => true,
                _ => false,
            }
        }
        fn LITERAL(&self) -> bool {
            match self {
                CoreToken::LITERAL => true,
                _ => false,
            }
        }
        fn SINGLE_LINE_COMMENT(&self) -> bool {
            match self {
                CoreToken::SINGLE_LINE_COMMENT => true,
                _ => false,
            }
        }
        fn BLOCK_COMMENT(&self) -> bool {
            match self {
                CoreToken::BLOCK_COMMENT => true,
                _ => false,
            }
        }
        fn ENDMARKER(&self) -> bool {
            match self {
                CoreToken::ENDMARKER => true,
                _ => false,
            }
        }
        fn LEXICAL_ERROR(&self) -> bool {
            match self {
                CoreToken::LEXICAL_ERROR(_) => true,
                _ => false,
            }
        }
        pub fn is_eq(&self, symbol: &str) -> bool {
            match symbol {
                IF => self.IF(),
                ELSE => self.ELSE(),
                ELIF => self.ELIF(),
                FOR => self.FOR(),
                WHILE => self.WHILE(),
                CONTINUE => self.CONTINUE(),
                BREAK => self.BREAK(),
                DEF => self.DEF(),
                RETURN => self.RETURN(),
                FUNC => self.FUNC(),
                TYPE_KEYWORD => self.TYPE_KEYWORD(),
                ATOMIC_TYPE => self.ATOMIC_TYPE(),
                LET => self.LET(),
                SELF => self.SELF(),
                IMPL => self.IMPL(),
                INTERFACE_KEYWORD => self.INTERFACE_KEYWORD(),
                AND => self.AND(),
                NOT => self.NOT(),
                OR => self.OR(),
                IN => self.IN(),
                TRUE => self.TRUE(),
                FALSE => self.FALSE(),
                PLUS => self.PLUS(),
                DASH => self.DASH(),
                RIGHT_ARROW => self.RIGHT_ARROW(),
                STAR => self.STAR(),
                DOUBLE_STAR => self.DOUBLE_STAR(),
                SLASH => self.SLASH(),
                LPAREN => self.LPAREN(),
                RPAREN => self.RPAREN(),
                LBRACE => self.LBRACE(),
                RBRACE => self.RBRACE(),
                LSQUARE => self.LSQUARE(),
                RSQUARE => self.RSQUARE(),
                SEMICOLON => self.SEMICOLON(),
                COLON => self.COLON(),
                DOUBLE_COLON => self.DOUBLE_COLON(),
                COMMA => self.COMMA(),
                DOT => self.DOT(),
                BLANK => self.BLANK(),
                NEWLINE => self.NEWLINE(),
                EQUAL => self.EQUAL(),
                DOUBLE_EQUAL => self.DOUBLE_EQUAL(),
                LBRACKET => self.LBRACKET(),
                RBRACKET => self.RBRACKET(),
                LESS_EQUAL => self.LESS_EQUAL(),
                GREATER_EQUAL => self.GREATER_EQUAL(),
                NOT_EQUAL => self.NOT_EQUAL(),
                INTEGER => self.INTEGER(),
                FLOATING_POINT_NUMBER => self.FLOATING_POINT_NUMBER(),
                IDENTIFIER => self.IDENTIFIER(),
                LITERAL => self.LITERAL(),
                SINGLE_LINE_COMMENT => self.SINGLE_LINE_COMMENT(),
                BLOCK_COMMENT => self.BLOCK_COMMENT(),
                ENDMARKER => self.ENDMARKER(),
                LEXICAL_ERROR => self.LEXICAL_ERROR(),
                "\n" => self.NEWLINE(),
                _ => ::core::panicking::panic_fmt(::core::fmt::Arguments::new_v1(
                    &[
                        "internal error: entered unreachable code: token `",
                        "` missing from matching arm",
                    ],
                    &[::core::fmt::ArgumentV1::new_display(&symbol)],
                )),
            }
        }
    }
    impl ToString for CoreToken {
        fn to_string(&self) -> String {
            let symbol_str = match self {
                CoreToken::IF => IF,
                CoreToken::ELSE => ELSE,
                CoreToken::ELIF => ELIF,
                CoreToken::FOR => FOR,
                CoreToken::WHILE => WHILE,
                CoreToken::CONTINUE => CONTINUE,
                CoreToken::BREAK => BREAK,
                CoreToken::DEF => DEF,
                CoreToken::RETURN => RETURN,
                CoreToken::FUNC => FUNC,
                CoreToken::TYPE_KEYWORD => TYPE_KEYWORD,
                CoreToken::ATOMIC_TYPE => ATOMIC_TYPE,
                CoreToken::LET => LET,
                CoreToken::SELF => SELF,
                CoreToken::IMPL => IMPL,
                CoreToken::INTERFACE_KEYWORD => INTERFACE_KEYWORD,
                CoreToken::AND => AND,
                CoreToken::NOT => NOT,
                CoreToken::OR => OR,
                CoreToken::IN => IN,
                CoreToken::TRUE => TRUE,
                CoreToken::FALSE => FALSE,
                CoreToken::PLUS => PLUS,
                CoreToken::DASH => DASH,
                CoreToken::RIGHT_ARROW => RIGHT_ARROW,
                CoreToken::STAR => STAR,
                CoreToken::DOUBLE_STAR => DOUBLE_STAR,
                CoreToken::SLASH => SLASH,
                CoreToken::LPAREN => LPAREN,
                CoreToken::RPAREN => RPAREN,
                CoreToken::LBRACE => LBRACE,
                CoreToken::RBRACE => RBRACE,
                CoreToken::LSQUARE => LSQUARE,
                CoreToken::RSQUARE => RSQUARE,
                CoreToken::SEMICOLON => SEMICOLON,
                CoreToken::COLON => COLON,
                CoreToken::DOUBLE_COLON => DOUBLE_COLON,
                CoreToken::COMMA => COMMA,
                CoreToken::DOT => DOT,
                CoreToken::BLANK => BLANK,
                CoreToken::NEWLINE => NEWLINE,
                CoreToken::EQUAL => EQUAL,
                CoreToken::DOUBLE_EQUAL => DOUBLE_EQUAL,
                CoreToken::LBRACKET => LBRACKET,
                CoreToken::RBRACKET => RBRACKET,
                CoreToken::LESS_EQUAL => LESS_EQUAL,
                CoreToken::GREATER_EQUAL => GREATER_EQUAL,
                CoreToken::NOT_EQUAL => NOT_EQUAL,
                CoreToken::INTEGER => INTEGER,
                CoreToken::FLOATING_POINT_NUMBER => FLOATING_POINT_NUMBER,
                CoreToken::IDENTIFIER => IDENTIFIER,
                CoreToken::LITERAL => LITERAL,
                CoreToken::SINGLE_LINE_COMMENT => SINGLE_LINE_COMMENT,
                CoreToken::BLOCK_COMMENT => BLOCK_COMMENT,
                CoreToken::ENDMARKER => ENDMARKER,
                CoreToken::LEXICAL_ERROR(_) => LEXICAL_ERROR,
            };
            String::from(symbol_str)
        }
    }
    pub enum LexicalErrorKind {
        INVALID_CHAR,
        NO_CLOSING_SYMBOLS,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for LexicalErrorKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                LexicalErrorKind::INVALID_CHAR => {
                    ::core::fmt::Formatter::write_str(f, "INVALID_CHAR")
                }
                LexicalErrorKind::NO_CLOSING_SYMBOLS => {
                    ::core::fmt::Formatter::write_str(f, "NO_CLOSING_SYMBOLS")
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for LexicalErrorKind {
        #[inline]
        fn clone(&self) -> LexicalErrorKind {
            match self {
                LexicalErrorKind::INVALID_CHAR => LexicalErrorKind::INVALID_CHAR,
                LexicalErrorKind::NO_CLOSING_SYMBOLS => LexicalErrorKind::NO_CLOSING_SYMBOLS,
            }
        }
    }
    impl ::core::marker::StructuralPartialEq for LexicalErrorKind {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for LexicalErrorKind {
        #[inline]
        fn eq(&self, other: &LexicalErrorKind) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
        }
    }
    pub struct MissingToken {
        pub expected_symbols: Rc<Vec<&'static str>>,
        pub received_token: Token,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for MissingToken {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "MissingToken",
                "expected_symbols",
                &&self.expected_symbols,
                "received_token",
                &&self.received_token,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for MissingToken {
        #[inline]
        fn clone(&self) -> MissingToken {
            MissingToken {
                expected_symbols: ::core::clone::Clone::clone(&self.expected_symbols),
                received_token: ::core::clone::Clone::clone(&self.received_token),
            }
        }
    }
    pub struct Token {
        pub line_number: usize,
        pub core_token: CoreToken,
        pub start_index: usize,
        pub end_index: usize,
        pub trivia: Option<Rc<Vec<Token>>>,
        pub parent: Option<ASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Token {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            let names: &'static _ = &[
                "line_number",
                "core_token",
                "start_index",
                "end_index",
                "trivia",
                "parent",
            ];
            let values: &[&dyn ::core::fmt::Debug] = &[
                &&self.line_number,
                &&self.core_token,
                &&self.start_index,
                &&self.end_index,
                &&self.trivia,
                &&self.parent,
            ];
            ::core::fmt::Formatter::debug_struct_fields_finish(f, "Token", names, values)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Token {
        #[inline]
        fn clone(&self) -> Token {
            Token {
                line_number: ::core::clone::Clone::clone(&self.line_number),
                core_token: ::core::clone::Clone::clone(&self.core_token),
                start_index: ::core::clone::Clone::clone(&self.start_index),
                end_index: ::core::clone::Clone::clone(&self.end_index),
                trivia: ::core::clone::Clone::clone(&self.trivia),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    impl Token {
        pub fn extract_lexeme(lexer: &mut CoreLexer, code: &Code) -> Token {
            let begin_lexeme = &mut lexer.begin_lexeme;
            let line_number = &mut lexer.line_number;
            let code_lines = &mut lexer.code_lines;
            let line_start_index = &mut lexer.line_start_index;
            let start_index = *begin_lexeme;
            let start_line_number = *line_number;
            let critical_char = code.get_char(*begin_lexeme);
            let core_token = match critical_char {
                '(' => {
                    *begin_lexeme = *begin_lexeme + 1;
                    CoreToken::LPAREN
                }
                ')' => {
                    *begin_lexeme = *begin_lexeme + 1;
                    CoreToken::RPAREN
                }
                '{' => {
                    *begin_lexeme = *begin_lexeme + 1;
                    CoreToken::LBRACE
                }
                '}' => {
                    *begin_lexeme = *begin_lexeme + 1;
                    CoreToken::RBRACE
                }
                '[' => {
                    *begin_lexeme = *begin_lexeme + 1;
                    CoreToken::LSQUARE
                }
                ']' => {
                    *begin_lexeme = *begin_lexeme + 1;
                    CoreToken::RSQUARE
                }
                ';' => {
                    *begin_lexeme = *begin_lexeme + 1;
                    CoreToken::SEMICOLON
                }
                ',' => {
                    *begin_lexeme = *begin_lexeme + 1;
                    CoreToken::COMMA
                }
                '.' => {
                    *begin_lexeme = *begin_lexeme + 1;
                    CoreToken::DOT
                }
                '+' => {
                    *begin_lexeme = *begin_lexeme + 1;
                    CoreToken::PLUS
                }
                '\n' => {
                    code_lines.push(*line_start_index);
                    *line_start_index = *begin_lexeme + 1;
                    *begin_lexeme = *begin_lexeme + 1;
                    *line_number = *line_number + 1;
                    CoreToken::NEWLINE
                }
                '/' => helper::extract_slash_prefix_lexeme(
                    begin_lexeme,
                    line_number,
                    code,
                    code_lines,
                    line_start_index,
                ),
                '"' => helper::extract_double_quote_prefix_lexeme(
                    begin_lexeme,
                    line_number,
                    code,
                    code_lines,
                    line_start_index,
                ),
                '\'' => helper::extract_single_quote_prefix_lexeme(
                    begin_lexeme,
                    line_number,
                    code,
                    code_lines,
                    line_start_index,
                ),
                '!' => helper::extract_exclaimation_prefix_lexeme(begin_lexeme, code),
                ' ' => helper::extract_blank_prefix_lexeme(begin_lexeme, code),
                '-' => helper::extract_dash_prefix_lexeme(begin_lexeme, code),
                '*' => helper::extract_star_prefix_lexeme(begin_lexeme, code),
                '#' => helper::extract_hash_prefix_lexeme(begin_lexeme, code),
                '=' => helper::extract_equal_prefix_lexeme(begin_lexeme, code),
                '>' => helper::extract_rbracket_prefix_lexeme(begin_lexeme, code),
                '<' => helper::extract_lbracket_prefix_lexeme(begin_lexeme, code),
                ':' => helper::extract_colon_prefix_lexeme(begin_lexeme, code),
                c => {
                    let token: CoreToken;
                    if is_letter(&c) {
                        token = helper::extract_letter_prefix_lexeme(begin_lexeme, code);
                    } else if c.is_digit(10) {
                        token = helper::extract_digit_prefix_lexeme(begin_lexeme, code);
                    } else {
                        let error_str = Rc::new({
                            let res = ::alloc::fmt::format(::core::fmt::Arguments::new_v1(
                                &["invalid character `", "` found"],
                                &[::core::fmt::ArgumentV1::new_display(&c)],
                            ));
                            res
                        });
                        token = CoreToken::LEXICAL_ERROR((
                            LexicalErrorKind::INVALID_CHAR,
                            error_str.clone(),
                        ));
                        *begin_lexeme = *begin_lexeme + 1;
                    }
                    token
                }
            };
            let end_index = *begin_lexeme;
            let end_line_number = *line_number;
            let token = Token {
                line_number: *line_number,
                core_token: core_token.clone(),
                start_index,
                end_index,
                trivia: None,
                parent: None,
            };
            match &core_token {
                CoreToken::LEXICAL_ERROR(lexical_err_value) => match lexical_err_value.0 {
                    LexicalErrorKind::INVALID_CHAR => {
                        if end_line_number != start_line_number {
                            ::core::panicking::unreachable_display(
                                &"invalid char should occur on the same line",
                            )
                        }
                        lexer.log_invalid_char_lexical_error(&token, &lexical_err_value.1);
                    }
                    LexicalErrorKind::NO_CLOSING_SYMBOLS => {
                        lexer.log_no_closing_symbols_lexical_error(
                            start_line_number,
                            end_line_number,
                            &lexical_err_value.1,
                        );
                    }
                },
                _ => {}
            }
            token
        }
        pub fn set_trivia(&mut self, trivia_vec: Vec<Token>) {
            self.trivia = Some(Rc::new(trivia_vec));
        }
        pub fn index(&self) -> usize {
            (self.start_index + self.end_index) / 2 as usize
        }
        pub fn name(&self) -> String {
            self.core_token.to_string()
        }
        pub fn token_value(&self, code: &Code) -> String {
            code.token_value(self.start_index, Some(self.end_index))
        }
        pub fn width(&self) -> usize {
            self.end_index - self.start_index
        }
        pub fn is_eq(&self, symbol: &str) -> bool {
            self.core_token.is_eq(symbol)
        }
    }
}
