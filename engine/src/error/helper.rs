use crate::lexer::token::Token;
use std::fmt::{self};
use std::{fmt::Display, rc::Rc};

#[derive(Clone, Debug)]
pub enum IdentifierKind {
    VARIABLE,
    FUNCTION,
    TYPE,
}
impl Display for IdentifierKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IdentifierKind::VARIABLE => write!(f, "variable"),
            IdentifierKind::FUNCTION => write!(f, "function"),
            IdentifierKind::TYPE => write!(f, "type"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum PropertyKind {
    FIELD,
    METHOD,
}
impl Display for PropertyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PropertyKind::FIELD => write!(f, "field"),
            PropertyKind::METHOD => write!(f, "method"),
        }
    }
}

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
