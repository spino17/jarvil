use std::{io::Error as IOError, fmt::Display};
use std::fmt::Formatter;

#[derive(Debug)]
pub struct LexicalError {
    line_number: usize,
    err_message: &'static str,
}

impl LexicalError {
    pub fn new(line_number: usize, err_message: &'static str) -> Self {
        LexicalError{
            line_number,
            err_message,
        }
    }
}

#[derive(Debug)]
pub struct SyntaxError {
    line_number: usize,
    lookahead_index: usize,
    err_message: &'static str,
}

impl SyntaxError {
    pub fn new(line_number: usize, lookahead_index: usize, err_message: &'static str) -> Self {
        SyntaxError{
            line_number,
            lookahead_index,
            err_message,
        }
    }
}

#[derive(Debug)]
pub struct SemanticError {
    line_number: usize,
    lookahead_index: usize,
    err_message: &'static str
}

impl SemanticError {
    pub fn new(line_number: usize, lookahead_index: usize, err_message: &'static str) -> Self {
        SemanticError{
            line_number,
            lookahead_index,
            err_message,
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    SYNTAX_ERROR(SyntaxError),
    SEMANTIC_ERROR(SemanticError)
}

impl From<SyntaxError> for ParseError {
    fn from(err: SyntaxError) -> Self {
        ParseError::SYNTAX_ERROR(err)
    }
}

impl From<SemanticError> for ParseError {
    fn from(err: SemanticError) -> Self {
        ParseError::SEMANTIC_ERROR(err)
    }
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
                    ParseError::SYNTAX_ERROR(syntax_error) => write!(f, "(Error occured while compilation\nSynatx Error\n on line {} - {})", syntax_error.line_number, syntax_error.err_message),
                    ParseError::SEMANTIC_ERROR(semantic_error) => write!(f, "(Error occured while compilation\nSemantic Error\n on line {} - {})", semantic_error.line_number, semantic_error.err_message)
                }
            }
        }
    }
}

pub fn aggregate_errors(errors: Vec<ParseError>) -> ParseError {
    todo!()
}
