use super::chunk::OpCode;
use crate::backend::chunk::Chunk;
use std::{cell::RefCell, rc::Rc};

pub struct CoreCompiler {
    pub chunk: Chunk,
    pub stack_simulated: Vec<usize>,
    pub curr_depth: usize,
    pub curr_local_var_index: usize,
    pub parent: Option<Compiler>,
}

impl CoreCompiler {
    fn chunk(&mut self) -> Chunk {
        std::mem::take(&mut self.chunk)
    }

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

    pub fn close_block(&mut self) {
        self.curr_local_var_index =
            self.curr_local_var_index - self.stack_simulated[self.curr_depth];
        self.curr_depth -= 1;
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

    pub fn emit_bytecode(&mut self, op_code: OpCode, line_number: usize) {
        self.0
            .as_ref()
            .borrow_mut()
            .chunk
            .write_instruction(op_code, line_number);
    }
}
