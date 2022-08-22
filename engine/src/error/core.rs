use crate::code::Code;
use crate::context;
use crate::lexer::token::Token;
use colored::Colorize;
use text_size::{TextRange, TextSize};
use std::convert::TryFrom;
use std::fmt::Formatter;
use std::rc::Rc;
use std::fmt::Display;

pub fn int_length(n: usize) -> usize {
    let base = 10;
    let mut power = base;
    let mut count = 1;
    while n >= power {
        count += 1;
        if let Some(new_power) = power.checked_mul(base) {
            power = new_power;
        } else {
            break;
        }
    }
    count
}

pub fn format_line_number(line_number: usize, max_line_number: usize) -> String {
    let line_number_len = int_length(line_number);
    let max_line_number_len = int_length(max_line_number);
    let blank_str = " ".repeat(max_line_number_len - line_number_len);
    format!("{}{}", line_number, blank_str)
}

#[derive(Debug)]
pub struct InvalidCharLexicalErrorData {
    pub invalid_token: Token,
    pub err_message: Rc<String>,
}

#[derive(Debug)]
pub struct NoClosingSymbolsLexicalErrorData {
    pub start_line_number: usize,
    pub end_line_number: usize,
    pub err_message: Rc<String>,
}

#[derive(Debug)]
pub enum LexicalErrorData {
    INVALID_CHAR(InvalidCharLexicalErrorData),
    NO_CLOSING_SYMBOLS(NoClosingSymbolsLexicalErrorData),
}

impl LexicalErrorData {
    pub fn new_with_invalid_char(invalid_token: &Token, err_message: &Rc<String>) -> Self {
        LexicalErrorData::INVALID_CHAR(InvalidCharLexicalErrorData {
            invalid_token: invalid_token.clone(),
            err_message: err_message.clone(),
        })
    }

    pub fn new_with_no_closing_symbols(
        start_line_number: usize,
        end_line_number: usize,
        err_message: &Rc<String>,
    ) -> Self {
        LexicalErrorData::NO_CLOSING_SYMBOLS(NoClosingSymbolsLexicalErrorData {
            start_line_number,
            end_line_number,
            err_message: err_message.clone(),
        })
    }
}

#[derive(Debug, Clone)]
pub enum JarvilErrorKind {
    LEXICAL_ERROR,
    SYNTAX_ERROR,
    SEMANTIC_ERROR,
}

impl Display for JarvilErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            JarvilErrorKind::LEXICAL_ERROR => {
                write!(f, "{}", format!("{}", "---> Lexical Error".bright_red()))
            }
            JarvilErrorKind::SYNTAX_ERROR => {
                write!(f, "{}", format!("{}", "---> Syntax Error".bright_red()))
            }
            JarvilErrorKind::SEMANTIC_ERROR => {
                write!(f, "{}", format!("{}", "---> Semantic Error".bright_red()))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct JarvilError {
    range: TextRange,
    err_message: Rc<String>,
    kind: JarvilErrorKind,
}
impl JarvilError {
    fn new(
        start_index: usize,
        end_index: usize,
        err_message: String,
        kind: JarvilErrorKind,
    ) -> Self {
        let range = TextRange::new(TextSize::try_from(start_index).unwrap(), TextSize::try_from(end_index).unwrap());
        JarvilError {
            range,
            err_message: Rc::new(err_message),
            kind,
        }
    }

    pub fn form_single_line_error(
        start_err_index: usize,
        end_err_index: usize,
        line_number: usize,
        line_start_index: usize,
        code_line: String,
        err_message: String,
        err_kind: JarvilErrorKind,
    ) -> Self {
        assert!(
            !(start_err_index < line_start_index || end_err_index < line_start_index),
            "lookahead at which error occured can never be less than the start index of the line"
        );
        let start_pointer_index = start_err_index - line_start_index;
        let end_pointer_index = end_err_index - line_start_index;
        let mut pointer_line: Vec<char> = vec![];
        for (i, _) in code_line.chars().enumerate() {
            if i >= start_pointer_index && i < end_pointer_index {
                pointer_line.push('^');
            } else {
                pointer_line.push(' ');
            }
        }
        let pointer_line: String = pointer_line.iter().collect();
        let blank_str = " ".repeat(int_length(line_number));
        let err_code_part = format!(
            "{} |\n{} | {}\n{} | {}",
            blank_str,
            line_number,
            code_line.clone(),
            blank_str,
            pointer_line.yellow()
        )
        .bright_blue();
        let err_message = format!(
            "\n{}\n{}\n{}\n",
            err_kind,
            err_code_part,
            err_message.yellow().bold()
        );
        JarvilError::new(start_err_index, end_err_index, err_message, err_kind)
    }

    pub fn form_multi_line_error(
        start_line_number: usize,
        end_line_number: usize,
        code: &Code,
        err_message: String,
        err_kind: JarvilErrorKind,
    ) -> Self {
        assert!(
            end_line_number >= start_line_number,
            "end line number cannot be less than start line number"
        );
        let mut code_lines = code.lines(start_line_number, end_line_number);
        let code_lines_len = code_lines.len();
        let max_error_lines = context::max_error_lines();
        if code_lines_len > max_error_lines {
            code_lines.resize(max_error_lines, String::default());
            code_lines.push(String::from("..."));
        }
        let mut flag = false;
        let mut line_number = start_line_number;
        let max_line_number = start_line_number + code_lines_len - 1;
        let blank_str = " ".repeat(int_length(max_line_number));
        let mut err_code_part: String = format!("{} |\n", blank_str);
        for code_line in &code_lines {
            if flag {
                err_code_part.push_str("\n");
            }
            err_code_part.push_str(&format!(
                "{} | {}",
                format_line_number(line_number, max_line_number),
                code_line
            ));
            flag = true;
            line_number = line_number + 1;
        }
        err_code_part.push_str("\n");
        err_code_part.push_str(&format!("{} |\n", blank_str));
        let err_message = format!(
            "\n{}\n{}\n{}\n",
            err_kind,
            err_code_part.bright_blue(),
            err_message.yellow().bold()
        );
        JarvilError::new(
            code.get_line_start_index(start_line_number),
            code.get_line_start_index(end_line_number),
            err_message,
            err_kind,
        )
    }
}

impl Display for JarvilError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.err_message)
    }
}
