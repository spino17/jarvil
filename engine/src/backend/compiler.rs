use crate::backend::{chunk::Chunk, object::core::ObjectTracker};
use std::{cell::RefCell, rc::Rc};

use super::chunk::OpCode;

pub struct CoreCompiler {
    object_tracker: ObjectTracker,
    pub chunk: Chunk,
    pub parent: Option<Compiler>,
}

impl CoreCompiler {
    fn chunk(&mut self) -> Chunk {
        std::mem::take(&mut self.chunk)
    }
}

#[derive(Clone)]
pub struct Compiler(pub Rc<RefCell<CoreCompiler>>);

impl Compiler {
    pub fn new(tracker: &ObjectTracker) -> Self {
        Compiler(Rc::new(RefCell::new(CoreCompiler {
            object_tracker: tracker.clone(),
            chunk: Chunk::default(),
            parent: None,
        })))
    }

    pub fn new_with_parent(tracker: &ObjectTracker, parent: &Compiler) -> Self {
        Compiler(Rc::new(RefCell::new(CoreCompiler {
            object_tracker: tracker.clone(),
            chunk: Chunk::default(),
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
