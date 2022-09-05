use crate::code::Code;
use crate::context;
use crate::error::helper::{format_line_number, int_length};
use colored::Colorize;
use std::cell::RefCell;
use std::cmp::min;
use std::convert::TryFrom;
use std::fmt::Display;
use std::fmt::Formatter;
use std::rc::Rc;
use text_size::{TextRange, TextSize};

pub struct JarvilErrorVec {
    errors: Vec<Rc<RefCell<Vec<JarvilError>>>>,
    len: usize,
}
impl JarvilErrorVec {
    pub fn new() -> Self {
        JarvilErrorVec {
            errors: vec![],
            len: 0,
        }
    }

    pub fn push(&mut self, err: JarvilError) {
        let line_number = err.line_number();
        let errors_len = self.errors.len();
        if line_number - 1 < errors_len {
            self.errors[line_number - 1].as_ref().borrow_mut().push(err);
        } else {
            self.errors
                .resize(line_number, Rc::new(RefCell::new(vec![])));
            self.errors[line_number - 1].as_ref().borrow_mut().push(err);
        }
        self.len = self.len + 1;
    }

    pub fn append(&mut self, err_vec: &mut JarvilErrorVec) {
        let len_1 = self.errors.len();
        let len_2 = err_vec.errors.len();
        let min_len = min(len_1, len_2);
        if min_len == len_1 {
            for i in 0..min_len {
                err_vec.errors[i]
                    .as_ref()
                    .borrow_mut()
                    .append(&mut *self.errors[i].as_ref().borrow_mut());
            }
        } else {
            for i in 0..min_len {
                self.errors[i]
                    .as_ref()
                    .borrow_mut()
                    .append(&mut *err_vec.errors[i].as_ref().borrow_mut());
            }
        }
    }

    pub fn errors_on_line(&self, line_number: usize) -> Rc<RefCell<Vec<JarvilError>>> {
        self.errors[line_number - 1].clone()
    }

    pub fn vec_start_index(&self, index: usize) -> usize {
        if index == 0 {
            return 0;
        } else {
            self.errors[index - 1].as_ref().borrow().len()
        }
    }

    pub fn get(&self, index: usize) -> Option<JarvilError> {
        if index >= self.len || index < 0 {
            return None;
        }
        let mut counter = 0;
        let mut total_index = 0;
        loop {
            total_index = total_index + self.errors[counter].as_ref().borrow().len();
            if total_index - 1 >= index {
                break;
            }
            counter = counter + 1;
        }
        let start_index = self.vec_start_index(counter);
        Some(self.errors[counter].as_ref().borrow()[index - start_index].clone())
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
    start_line_number: usize,
}
impl JarvilError {
    fn new(
        start_index: usize,
        end_index: usize,
        err_message: String,
        start_line_number: usize,
        kind: JarvilErrorKind,
    ) -> Self {
        let range = TextRange::new(
            TextSize::try_from(start_index).unwrap(),
            TextSize::try_from(end_index).unwrap(),
        );
        JarvilError {
            range,
            err_message: Rc::new(err_message),
            kind,
            start_line_number,
        }
    }

    fn line_number(&self) -> usize {
        self.start_line_number
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
        JarvilError::new(
            start_err_index,
            end_err_index,
            err_message,
            line_number,
            err_kind,
        )
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
            start_line_number,
            err_kind,
        )
    }

    pub fn form_error(
        start_index: usize,
        end_index: usize,
        start_line_number: usize,
        code: &Code,
        err_message: String,
        err_kind: JarvilErrorKind,
    ) -> Self {
        let (start_line_number, end_line_number) =
            code.line_range_from_indexes(start_index, end_index, start_line_number);
        let err = if start_line_number == end_line_number {
            JarvilError::form_single_line_error(
                start_index,
                end_index,
                start_line_number,
                code.get_line_start_index(start_line_number),
                code.line(start_line_number),
                err_message,
                err_kind,
            )
        } else {
            JarvilError::form_multi_line_error(
                start_line_number,
                end_line_number,
                code,
                err_message,
                err_kind,
            )
        };
        err
    }
}

impl Display for JarvilError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.err_message)
    }
}
