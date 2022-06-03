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
pub enum CompilationError {
    IO_ERROR(IOError),
    LEXICAL_ERROR(LexicalError),
    SYNTAX_ERROR(SyntaxError),
    SEMANTIC_ERROR(SemanticError)
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

impl From<SyntaxError> for CompilationError {
    fn from(err: SyntaxError) -> Self {
        CompilationError::SYNTAX_ERROR(err)
    }
}

impl From<SemanticError> for CompilationError {
    fn from(err: SemanticError) -> Self {
        CompilationError::SEMANTIC_ERROR(err)
    }
}

impl Display for CompilationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            CompilationError::IO_ERROR(err) => write!(f, "(Error occured while compilation\nIO Errror\n{})", err.to_string()),
            CompilationError::LEXICAL_ERROR(err) => write!(f, "(Error occured while compilation\nLexical Error\n{})", err.err_message),
            CompilationError::SYNTAX_ERROR(err) => write!(f, "(Error occured while compilation\nSynatx Error\n{})", err.err_message),
            CompilationError::SEMANTIC_ERROR(err) => write!(f, "(Error occured while compilation\nSemantic Error\n{})", err.err_message)
        }
    }
}
