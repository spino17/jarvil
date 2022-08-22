use std::cell::RefCell;

thread_local! {
    static CONTEXT: RefCell<Context> = RefCell::new(Context::new())
}

// TODO - indent_spaces and max_error_lines take default values while initialization of thread local but should provide
// overriding mechanism to set custom values through command line or IDE settings.

struct Context {
    indent_spaces: i64,
    max_error_lines: usize,
    max_line_width: usize,
}

impl Context {
    fn new() -> Self {
        Context {
            indent_spaces: 4,    // default indentation is 4 spaces
            max_error_lines: 10, // default max lines shown in error messages
            max_line_width: 88,  // default max line width used while formatting (same as black)
        }
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
