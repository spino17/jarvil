use super::chunk::Chunk;

pub enum InterpretResult {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
}

pub struct VM {
    chunk: Chunk,
    ip: usize,
}
impl VM {
    pub fn new(chunk: Chunk) -> Self {
        VM { chunk, ip: 0 }
    }

    pub fn run(&self) -> InterpretResult {
        todo!()
    }
}
