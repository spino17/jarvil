use crate::{backend::chunk::Chunk, parser::resolver::UpValue};
use core::num;
use std::{cell::RefCell, rc::Rc};

pub struct Local {
    pub depth: usize,
    pub is_captured: bool,
}

pub struct CoreCompiler {
    pub chunk: Chunk,
    pub curr_depth: usize,
    pub locals: Vec<Local>,
    pub upvalues: Rc<RefCell<Vec<UpValue>>>,
    pub parent: Option<Compiler>,
}

impl CoreCompiler {
    fn chunk(&mut self) -> Chunk {
        std::mem::take(&mut self.chunk)
    }

    pub fn depth(&self) -> usize {
        self.curr_depth
    }

    pub fn variable_decl_callback(&mut self, is_captured: bool) {
        self.locals.push(Local {
            depth: self.curr_depth,
            is_captured,
        });
    }

    pub fn open_block(&mut self) {
        self.curr_depth += 1;
    }

    pub fn close_block(&mut self, num_of_popped_elements: usize) {
        let len = self.locals.len();
        self.locals.truncate(len - num_of_popped_elements);
        self.curr_depth -= 1;
    }
}

#[derive(Clone)]
pub struct Compiler(pub Rc<RefCell<CoreCompiler>>);

impl Compiler {
    pub fn new() -> Self {
        Compiler(Rc::new(RefCell::new(CoreCompiler {
            chunk: Chunk::default(),
            curr_depth: 0,
            locals: vec![],
            upvalues: Rc::new(RefCell::new(vec![])),
            parent: None,
        })))
    }

    pub fn new_with_parent(parent: &Compiler, upvalues: Rc<RefCell<Vec<UpValue>>>) -> Self {
        Compiler(Rc::new(RefCell::new(CoreCompiler {
            chunk: Chunk::default(),
            curr_depth: 0,
            locals: vec![],
            upvalues,
            parent: Some(parent.clone()),
        })))
    }

    pub fn chunk(&self) -> Chunk {
        self.0.as_ref().borrow_mut().chunk()
    }
}
