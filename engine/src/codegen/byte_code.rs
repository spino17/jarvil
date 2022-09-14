use crate::{
    ast::{
        ast::{ASTNode, BlockNode},
        walk::Visitor,
    },
    backend::chunk::Chunk,
};

#[derive(Default)]
pub struct ByteCodeGenerator {
    chunk: Chunk,
}

impl ByteCodeGenerator {
    fn new() -> Self {
        ByteCodeGenerator {
            chunk: Chunk::default(),
        }
    }

    pub fn emit_byte_code(&mut self, ast: &BlockNode) {
        todo!()
    }
}

impl Visitor for ByteCodeGenerator {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        todo!()
    }
}
