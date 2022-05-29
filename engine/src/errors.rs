use std::{io::Error as ioError, fmt::Display};
use std::fmt::Formatter;
use std::error::Error;

#[derive(Debug)]
pub struct IOError {

}

#[derive(Debug)]
pub struct SyntaxError {

}

#[derive(Debug)]
pub struct SemanticError {

}

#[derive(Debug)]
pub enum CompilationError {
    IO_ERROR(IOError),
    SYNTAX_ERROR(SyntaxError),
    SEMANTIC_ERROR(SemanticError)
}

impl From<ioError> for CompilationError {
    fn from(err: ioError) -> Self {
        todo!()
    }
}

impl From<SyntaxError> for CompilationError {
    fn from(err: SyntaxError) -> Self {
        todo!()
    }
}

impl From<SemanticError> for CompilationError {
    fn from(err: SemanticError) -> Self {
        todo!()
    }
}

impl Display for CompilationError {
    fn fmt(&self, _: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        todo!()
    }
}


impl Error for CompilationError {
    fn description(&self) -> &str {
        todo!()
    }
}