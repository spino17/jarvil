use std::error::Error;
use std::{io::Error as IOError, fmt::Display};
use std::fmt::{Formatter, write};
use std::rc::Rc;

#[derive(Debug)]
pub struct LexicalError {
    line_number: usize,
    err_message: String,
}

impl LexicalError {
    pub fn new(line_number: usize, err_message: String) -> Self {
        LexicalError{
            line_number,
            err_message,
        }
    }
}

pub enum ErrorKind {
    SYNTAX_ERROR,
    SEMANTIC_ERROR,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ErrorKind::SYNTAX_ERROR => write!(f, "Syntax Error"),
            ErrorKind::SEMANTIC_ERROR => write!(f, "Semantic Error"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    //code_line: Rc<String>,
    //line_start_index: usize,
    pub start_line_number: usize,
    pub end_line_number: usize,
    pub err_message: String,
}
impl ParseError {
    pub fn new(start_line_number: usize, end_line_number: usize, err_message: String) -> Self {
        ParseError {
            start_line_number,
            end_line_number,
            err_message,
        }
    }

    pub fn form_single_line_error(err_index: usize, line_number: usize, line_start_index: usize, 
        code_line: Rc<String>, err_message: String, err_kind: ErrorKind) -> String {
        if err_index < line_start_index {
            unreachable!("lookahead at which error occured can never be less than the start index of the line")
        }
        let pointer_index = err_index - line_start_index;
        let mut pointer_line: Vec<char> = vec![];
        for (i, _) in code_line.as_ref().chars().enumerate() {
            if i == pointer_index {
                pointer_line.push('^');
            } else {
                pointer_line.push(' ');
            }
        }
        let pointer_line: String = pointer_line.iter().collect();
        let err_code_part = format!("{}\n    {}", code_line.clone(), pointer_line);
        format!(">>> {}: line {}\n    {}\n    {}", err_kind, line_number, err_code_part, err_message)
    }
}

#[derive(Debug)]
pub enum CompilationError {
    IO_ERROR(IOError),
    LEXICAL_ERROR(LexicalError),
    SYNTAX_ERROR(ParseError),
    // SEMANTIC_ERROR(ParseError),
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
        CompilationError::SYNTAX_ERROR(err)
    }
}

impl Display for CompilationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            CompilationError::IO_ERROR(err) => write!(
                f, ">>> IOErrror:\n    {}", err.to_string()),
            CompilationError::LEXICAL_ERROR(lexical_err) => write!(f, 
                ">>> LexicalError: line {}\n    {}", lexical_err.line_number,
                lexical_err.err_message),
            CompilationError::SYNTAX_ERROR(syntax_err) => {
                write!(f, "{}", syntax_err.err_message)
            }
        }
    }
}

/*
pub fn aggregate_errors(errors: Vec<ParseError>) -> ParseError {
    let mut curr_line_number = std::usize::MAX;
    let mut curr_err_index = 0;
    let mut curr_error = None;
    for err in errors {
        if err.err_index > curr_err_index {
            curr_err_index = err.err_index;
            curr_line_number = err.line_number;
            curr_error = Some(err);
        } else if err.err_index == curr_err_index {
            if err.line_number < curr_line_number {
                curr_line_number = err.line_number;
                curr_error = Some(err);
            }
        }
    }
    match curr_error {
        Some(err) => err,
        None => unreachable!("aggregated error can be None only when provided vector of errors were empty which is not possible")
    }
}
 */
