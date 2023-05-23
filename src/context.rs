use std::cell::RefCell;
use std::env;
use std::rc::Rc;

thread_local! {
    static CONTEXT: RefCell<Context> = RefCell::new(Context::new())
}

struct Context {
    indent_spaces: usize,
    curr_dir_path: Rc<String>,
}

impl Context {
    fn new() -> Self {
        let curr_dir = env::current_dir().expect("failed to get current directory");
        let curr_dir_str = curr_dir.to_string_lossy();
        Context {
            indent_spaces: 4, // default indentation is 4 spaces
            curr_dir_path: Rc::new(curr_dir_str.to_string()),
        }
    }

    fn set_indent(&mut self, indent_spaces: usize) {
        self.indent_spaces = indent_spaces;
    }

    fn indent_spaces(&self) -> usize {
        self.indent_spaces
    }

    fn curr_dir_path(&self) -> Rc<String> {
        self.curr_dir_path.clone()
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

pub fn curr_dir_path() -> Rc<String> {
    match CONTEXT.try_with(|ctx| ctx.borrow().curr_dir_path()) {
        Ok(val) => val,
        Err(err) => {
            panic!("{}", err)
        }
    }
}
