use crate::{
    errors::JarvilError,
};
use std::cell::RefCell;

thread_local! {
    static CONTEXT: RefCell<Context> = RefCell::new(Context::new())
}

// TODO - indent_spaces and max_error_lines take default values while initialization of thread local but should provide
// overriding mechanism to set custom values through command line or IDE settings.

struct Context {
    parse_errors: Vec<JarvilError>,
    indent_spaces: i64,
    max_error_lines: usize,
    max_line_width: usize,
}

impl Context {
    fn new() -> Self {
        Context {
            parse_errors: vec![],
            indent_spaces: 4,    // default indentation is 4 spaces
            max_error_lines: 10, // default max lines shown in error messages
            max_line_width: 88,  // default max line width used while formatting (same as black)
        }
    }

    fn set_errors(&mut self, err: Vec<JarvilError>) {
        self.parse_errors = err;
    }

    fn push_error(&mut self, err: JarvilError) {
        self.parse_errors.push(err);
    }

    fn curr_error_line_number(&self) -> usize {
        self.parse_errors[self.parse_errors.len() - 1].end_line_number
    }

    pub fn first_error(&self) -> Option<JarvilError> {
        let errors_len = self.parse_errors.len();
        if errors_len == 0 {
            None
        } else {
            Some(self.parse_errors[0].clone())
        }
    }

    fn errors_len(&self) -> usize {
        self.parse_errors.len()
    }

    fn set_indent(&mut self, indent_spaces: i64) {
        self.indent_spaces = indent_spaces;
    }

    fn indent_spaces(&self) -> i64 {
        self.indent_spaces
    }

    fn set_max_error_lines(&mut self, max_error_lines: usize) {
        self.max_error_lines = max_error_lines;
    }

    fn max_error_lines(&self) -> usize {
        self.max_error_lines
    }

    fn set_max_line_width(&mut self, max_line_width: usize) {
        self.max_line_width = max_line_width;
    }

    fn max_line_width(&self) -> usize {
        self.max_line_width
    }
}

pub fn set_errors(err: Vec<JarvilError>) {
    match CONTEXT.try_with(|ctx| ctx.borrow_mut().set_errors(err)) {
        Err(err) => {
            panic!("{}", err)
        }
        _ => {}
    }
}

pub fn push_error(err: JarvilError) {
    match CONTEXT.try_with(|ctx| ctx.borrow_mut().push_error(err)) {
        Err(err) => {
            panic!("{}", err)
        }
        _ => {}
    }
}

pub fn curr_error_line_number() -> usize {
    match CONTEXT.try_with(|ctx| ctx.borrow().curr_error_line_number()) {
        Ok(val) => val,
        Err(err) => {
            panic!("{}", err)
        }
    }
}

pub fn first_error() -> Option<JarvilError> {
    match CONTEXT.try_with(|ctx| ctx.borrow().first_error()) {
        Ok(val) => val,
        Err(err) => {
            panic!("{}", err)
        }
    }
}

pub fn errors_len() -> usize {
    match CONTEXT.try_with(|ctx| ctx.borrow_mut().errors_len()) {
        Ok(val) => val,
        Err(err) => {
            panic!("{}", err)
        }
    }
}

pub fn set_indent(indent_spaces: i64) {
    match CONTEXT.try_with(|ctx| ctx.borrow_mut().set_indent(indent_spaces)) {
        Err(err) => {
            panic!("{}", err)
        }
        _ => {}
    }
}

pub fn indent_spaces() -> i64 {
    match CONTEXT.try_with(|ctx| ctx.borrow().indent_spaces()) {
        Ok(val) => val,
        Err(err) => {
            panic!("{}", err)
        }
    }
}

pub fn set_max_error_lines(max_error_lines: usize) {
    match CONTEXT.try_with(|ctx| ctx.borrow_mut().set_max_error_lines(max_error_lines)) {
        Err(err) => {
            panic!("{}", err)
        }
        _ => {}
    }
}

pub fn max_error_lines() -> usize {
    match CONTEXT.try_with(|ctx| ctx.borrow().max_error_lines()) {
        Ok(val) => val,
        Err(err) => {
            panic!("{}", err)
        }
    }
}

pub fn set_max_line_width(max_line_width: usize) {
    match CONTEXT.try_with(|ctx| ctx.borrow_mut().set_max_line_width(max_line_width)) {
        Err(err) => {
            panic!("{}", err)
        }
        _ => {}
    }
}

pub fn max_line_width() -> usize {
    match CONTEXT.try_with(|ctx| ctx.borrow().max_line_width()) {
        Ok(val) => val,
        Err(err) => {
            panic!("{}", err)
        }
    }
}
