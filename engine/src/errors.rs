use std::{io::Error as IOError, fmt::Display};
use std::fmt::Formatter;
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

#[derive(Debug)]
pub struct SyntaxError {
    code_line: Rc<String>,
    line_start_index: usize,
    line_number: usize,
    err_index: usize,
    err_message: Rc<String>,
}

impl SyntaxError {
    pub fn new(code_line: (Rc<String>, usize, usize, usize), err_message: String) -> Self {
        SyntaxError {
            code_line: code_line.0.clone(),
            line_start_index: code_line.1,
            line_number: code_line.2,
            err_index: code_line.3,
            err_message: Rc::new(err_message),
        }
    }

    fn form_err_code_line(&self) -> String {
        let err_index = self.err_index;
        let line_start_index = self.line_start_index;
        if err_index < line_start_index {
            unreachable!("lookahead at which error occured can never be less than the start index of the line")
        }
        let pointer_index = err_index - line_start_index;
        let mut pointer_line: Vec<char> = vec![];
        for (i, _) in self.code_line.as_ref().chars().enumerate() {
            if i == pointer_index {
                pointer_line.push('^');
            } else {
                pointer_line.push(' ');
            }
        }
        let pointer_line: String = pointer_line.iter().collect();
        format!("{}\n    {}", self.code_line.clone(), pointer_line)
    }

    pub fn clone(&self) -> Self {
        SyntaxError {
            code_line: self.code_line.clone(),
            line_start_index: self.line_start_index,
            line_number: self.line_number,
            err_index: self.err_index,
            err_message: self.err_message.clone(),
        }
    }
}

#[derive(Debug)]
pub enum CompilationError {
    IO_ERROR(IOError),
    LEXICAL_ERROR(LexicalError),
    SYNTAX_ERROR(SyntaxError)
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

impl Display for CompilationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            CompilationError::IO_ERROR(err) => write!(
                f, ">>> IOErrror:\n    {}", err.to_string()),
            CompilationError::LEXICAL_ERROR(lexical_err) => write!(f, 
                ">>> LexicalError: line {}\n    {}", lexical_err.line_number, 
                lexical_err.err_message),
            CompilationError::SYNTAX_ERROR(syntax_err) => {
                write!(f, ">>> SynatxError: line {}\n    {}\n    {}",
                syntax_err.line_number, 
                syntax_err.form_err_code_line(), 
                syntax_err.err_message)
            }
        }
    }
}

pub fn aggregate_errors(errors: Vec<SyntaxError>) -> SyntaxError {
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
