use crate::{
    ast::{
        ast::{ASTNode, BlockNode},
        walk::Visitor,
    },
    backend::chunk::Chunk,
};

pub struct ByteCodeGenerator {
    chunk: Chunk,
}

impl ByteCodeGenerator {
    fn new() -> Self {
        ByteCodeGenerator {
            chunk: Chunk::default(),
        }
    }

    fn generate_byte_code(&mut self, ast: &BlockNode) -> Chunk {
        std::mem::take(&mut self.chunk)
    }
}

impl Visitor for ByteCodeGenerator {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        todo!()
    }
}
