use std::{io::Error as IOError, fmt::Display};
use std::fmt::Formatter;

#[derive(Debug)]
pub struct LexicalError {
    err_message: &'static str,
}

impl LexicalError {
    pub fn new(err_message: &'static str) -> Self {
        LexicalError{
            err_message,
        }
    }
}

#[derive(Debug)]
pub struct SyntaxError {
    err_message: &'static str,
}

impl SyntaxError {
    pub fn new(err_message: &'static str) -> Self {
        SyntaxError{
            err_message,
        }
    }
}

#[derive(Debug)]
pub struct SemanticError {
    err_message: &'static str
}

impl SemanticError {
    pub fn new(err_message: &'static str) -> Self {
        SemanticError{
            err_message,
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    SYNTAX_ERROR(SyntaxError),
    SEMANTIC_ERROR(SemanticError)
}

#[derive(Debug)]
pub enum CompilationError {
    IO_ERROR(IOError),
    LEXICAL_ERROR(LexicalError),
    PARSE_ERROR(ParseError)
}

impl From<IOError> for CompilationError {
    fn from(err: IOError) -> Self {
        CompilationError::IO_ERROR(err)
    }
}

impl From<LexicalError> for CompilationError {
    fn from(err: LexicalError) -> Self {
        CompilationError::LEXICAL_ERROR(err)
    }
}

impl From<ParseError> for CompilationError {
    fn from(err: ParseError) -> Self {
        CompilationError::PARSE_ERROR(err)
    }
}

impl Display for CompilationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            CompilationError::IO_ERROR(err) => write!(f, "(Error occured while compilation\nIO Errror\n{})", err.to_string()),
            CompilationError::LEXICAL_ERROR(err) => write!(f, "(Error occured while compilation\nLexical Error\n{})", err.err_message),
            CompilationError::PARSE_ERROR(err) => {
                match err {
                    ParseError::SYNTAX_ERROR(syntax_error) => write!(f, "(Error occured while compilation\nSynatx Error\n{})", syntax_error.err_message),
                    ParseError::SEMANTIC_ERROR(semantic_error) => write!(f, "(Error occured while compilation\nSemantic Error\n{})", semantic_error.err_message)
                }
            }
        }
    }
}
