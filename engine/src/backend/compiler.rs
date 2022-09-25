use crate::backend::chunk::Chunk;
use std::{cell::RefCell, rc::Rc};

pub struct CoreCompiler {
    pub chunk: Chunk,
    pub stack_simulated: Vec<usize>, // simulation of runtime snapshots of stack
    pub curr_depth: usize,           // curr depth in blocks starting with 0
    pub curr_local_var_index: usize, // relative index of the local variable
    pub parent: Option<Compiler>,
}

impl CoreCompiler {
    fn chunk(&mut self) -> Chunk {
        std::mem::take(&mut self.chunk)
    }

    // below methods simulate the `stack during runtime`
    pub fn variable_decl_callback(&mut self) -> usize {
        self.stack_simulated[self.curr_depth] += 1;
        self.curr_local_var_index += 1;
        self.curr_local_var_index - 1
    }

    pub fn open_block(&mut self) {
        self.curr_depth += 1;
        if self.curr_depth >= self.stack_simulated.len() {
            self.stack_simulated.push(0);
        } else {
            self.stack_simulated[self.curr_depth] = 0;
        }
    }

    pub fn close_block(&mut self) -> usize {
        let num_of_popped_elements = self.stack_simulated[self.curr_depth];
        self.curr_local_var_index =
            self.curr_local_var_index - self.stack_simulated[self.curr_depth];
        self.curr_depth -= 1;
        num_of_popped_elements
    }
}

#[derive(Clone)]
pub struct Compiler(pub Rc<RefCell<CoreCompiler>>);

impl Compiler {
    pub fn new() -> Self {
        Compiler(Rc::new(RefCell::new(CoreCompiler {
            chunk: Chunk::default(),
            stack_simulated: vec![],
            curr_depth: 0,
            curr_local_var_index: 0,
            parent: None,
        })))
    }

    pub fn new_with_parent(parent: &Compiler) -> Self {
        Compiler(Rc::new(RefCell::new(CoreCompiler {
            chunk: Chunk::default(),
            stack_simulated: vec![],
            curr_depth: 0,
            curr_local_var_index: 0,
            parent: Some(parent.clone()),
        })))
    }

    pub fn chunk(&self) -> Chunk {
        self.0.as_ref().borrow_mut().chunk()
    }
}
