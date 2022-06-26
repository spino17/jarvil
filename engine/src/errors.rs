use std::{io::Error as IOError, fmt::Display};
use std::fmt::Formatter;
use std::rc::Rc;

fn form_code_line(code_line: &(Rc<String>, usize, usize, usize)) -> String {
    let err_index = code_line.3;
    let line_start_index = code_line.1;
    if err_index < line_start_index {
        unreachable!("lookahead at which error occured can never be less than the start index of the line")
    }
    let pointer_index = err_index - line_start_index;
    let mut pointer_line: Vec<char> = vec![];
    for (i, _) in code_line.0.as_ref().chars().enumerate() {
        if i == pointer_index {
            pointer_line.push('^');
        } else {
            pointer_line.push(' ');
        }
    }
    let pointer_line: String = pointer_line.iter().collect();
    format!("{}\n    {}", code_line.0.clone(), pointer_line)
}

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
    // line_number: usize,
    code_line: (Rc<String>, usize, usize, usize),  // (code_line string, line_start_index, line_number, err_index)
    // err_index: usize,
    err_message: String,
}

impl SyntaxError {
    pub fn new(code_line: (Rc<String>, usize, usize, usize), err_message: String) -> Self {
        SyntaxError {
            // line_number,
            code_line: (code_line.0.clone(), code_line.1, code_line.2, code_line.3),
            err_message,
        }
    }
}

#[derive(Debug)]
pub struct SemanticError {
    // line_number: usize,
    code_line: (Rc<String>, usize, usize, usize),
    // err_index: usize,
    err_message: String,
}

impl SemanticError {
    pub fn new(code_line: (Rc<String>, usize, usize, usize), err_message: String) -> Self {
        // make complete code line here
        SemanticError {
            // line_number,
            code_line: (code_line.0.clone(), code_line.1, code_line.2, code_line.3),
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
            CompilationError::IO_ERROR(err) => write!(
                f, ">>> IOErrror:\n    {}", err.to_string()),
            CompilationError::LEXICAL_ERROR(lexical_err) => write!(f, 
                ">>> LexicalError: line {}\n    {}", lexical_err.line_number, 
                lexical_err.err_message),
            CompilationError::PARSE_ERROR(err) => {
                match err {
                    ParseError::SYNTAX_ERROR(syntax_error) => write!(f, 
                        ">>> SynatxError: line {}\n    {}\n    {}",
                        syntax_error.code_line.2, form_code_line(&syntax_error.code_line), 
                        syntax_error.err_message),
                    ParseError::SEMANTIC_ERROR(semantic_error) => write!(f, 
                        ">>> SemanticError: line {}\n    {}\n    {}", 
                        semantic_error.code_line.2, form_code_line(&semantic_error.code_line), 
                        semantic_error.err_message)
                }
            }
        }
    }
}

pub fn aggregate_errors(errors: Vec<ParseError>) -> ParseError {
    let mut line_number = std::usize::MAX;
    let mut err_index = 0;
    let mut curr_error = None;
    for err in errors {
        match err {
            ParseError::SYNTAX_ERROR(error) => {
                if error.code_line.3 > err_index {
                    err_index = error.code_line.3;
                    line_number = error.code_line.2;
                    curr_error = Some(ParseError::SYNTAX_ERROR(error));
                } else if error.code_line.3 == err_index {
                    if error.code_line.2 < line_number {
                        line_number = error.code_line.2;
                        curr_error = Some(ParseError::SYNTAX_ERROR(error));
                    }
                }
            },
            ParseError::SEMANTIC_ERROR(error) => {
                if error.code_line.3 > err_index {
                    err_index = error.code_line.3;
                    line_number = error.code_line.2;
                    curr_error = Some(ParseError::SEMANTIC_ERROR(error));
                } else if error.code_line.3 == err_index {
                    if error.code_line.2 < line_number {
                        line_number = error.code_line.2;
                        curr_error = Some(ParseError::SEMANTIC_ERROR(error));
                    }
                }
            }
        }
    }
    match curr_error {
        Some(err) => err,
        None => unreachable!("aggregated error can be None only when provided vector of errors were empty which is not possible")
    }
}
