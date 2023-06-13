use std::cell::RefCell;

thread_local! {
    static CONTEXT: RefCell<Context> = RefCell::new(Context::new())
}

struct Context {
    indent_spaces: usize,
}

impl Context {
    fn new() -> Self {
        Context {
            indent_spaces: 4, // default indentation is 4 spaces
        }
    }

    fn set_indent(&mut self, indent_spaces: usize) {
        self.indent_spaces = indent_spaces;
    }

    fn indent_spaces(&self) -> usize {
        self.indent_spaces
    }
}

pub fn set_indent(indent_spaces: usize) {
    match CONTEXT.try_with(|ctx| ctx.borrow_mut().set_indent(indent_spaces)) {
        Err(err) => {
            panic!("{}", err)
        }
        _ => {}
    }
}

pub fn indent_spaces() -> usize {
    match CONTEXT.try_with(|ctx| ctx.borrow().indent_spaces()) {
        Ok(val) => val,
        Err(err) => {
            panic!("{}", err)
        }
    }
}
