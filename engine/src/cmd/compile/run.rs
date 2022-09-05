use crate::backend::{chunk::Chunk, vm::VM};

pub fn run(chunk: Chunk) {
    let mut vm = VM::new(chunk);
    vm.run();
}
