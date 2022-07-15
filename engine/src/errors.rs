use std::error::Error;
use std::{io::Error as IOError, fmt::Display};
use std::fmt::{Formatter};
use std::rc::Rc;
use crate::context;
use crate::lexer::token::Token;

#[derive(Debug)]
pub struct InvalidCharLexicalErrorData {
    line_number: usize,
    invalid_token: Token,
    err_message: Rc<String>,
}

#[derive(Debug)]
pub struct NoClosingSymbolsLexicalErrorData {
    start_line_number: usize,
    end_line_number: usize,
    err_message: Rc<String>,
}

#[derive(Debug)]
pub enum LexicalErrorData {
    INVALID_CHAR(InvalidCharLexicalErrorData),
    NO_CLOSING_SYMBOLS(NoClosingSymbolsLexicalErrorData),
}

impl LexicalErrorData {
    pub fn new_with_invalid_char(line_number: usize, invalid_token: &Token, err_message: &Rc<String>) -> Self {
        LexicalErrorData::INVALID_CHAR(InvalidCharLexicalErrorData{
            line_number,
            invalid_token: invalid_token.clone(),
            err_message: err_message.clone(),
        })
    }

    pub fn new_with_no_closing_symbols(start_line_number: usize, end_line_number: usize, err_message: &Rc<String>) -> Self {
        LexicalErrorData::NO_CLOSING_SYMBOLS(NoClosingSymbolsLexicalErrorData{
            start_line_number,
            end_line_number,
            err_message: err_message.clone(),
        })
    }
}

pub enum ErrorKind {
    LEXICAL_ERROR,
    SYNTAX_ERROR,
    SEMANTIC_ERROR,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ErrorKind::LEXICAL_ERROR    => write!(f, "Lexical Error"),
            ErrorKind::SYNTAX_ERROR     => write!(f, "Syntax Error"),
            ErrorKind::SEMANTIC_ERROR   => write!(f, "Semantic Error"),
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

    pub fn form_multi_line_error(start_line_number: usize, end_line_number: usize, mut code_lines: Vec<Rc<String>>, 
        err_message: String, err_kind: ErrorKind) -> String {
        if end_line_number < start_line_number {
            unreachable!("end line number cannot be less than start line number")
        }
        if end_line_number == start_line_number {
            unreachable!("use `form_single_line_error` method for formaing errors occuring on the same line")
        }
        if end_line_number - start_line_number + 1 > context::max_error_lines() {
            code_lines = code_lines[start_line_number..(start_line_number + context::max_error_lines())].to_vec();
            code_lines.push(Rc::new(String::from("...")));
        }
        let mut err_code_part: String = String::from("");
        let mut flag = false;
        for code_line in &code_lines {
            if flag {
                err_code_part.push_str("\n    ");
            }
            err_code_part.push_str(&format!("| {}", code_line));
            flag = true;
        }
        format!(">>> {}: lines {} - {}\n    {}\n    {}", err_kind, start_line_number, end_line_number, err_code_part, err_message)
    }
}

#[derive(Debug)]
pub enum CompilationError {
    IO_ERROR(IOError),
    LEXICAL_ERROR(ParseError),
    SYNTAX_ERROR(ParseError),
    // SEMANTIC_ERROR(ParseError),
}

impl From<IOError> for CompilationError {
    fn from(err: IOError) -> Self {
        CompilationError::IO_ERROR(err)
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
            CompilationError::LEXICAL_ERROR(lexical_err) => {
                write!(f, "{}", lexical_err.err_message)
            }
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
