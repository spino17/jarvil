use crate::{
    ast::{
        ast::{ASTNode, BlockNode},
        walk::Visitor,
    },
    backend::chunk::Chunk,
};

pub struct CodeGenerator {
    chunk: Chunk,
}
impl CodeGenerator {
    fn new() -> Self {
        CodeGenerator {
            chunk: Chunk::default(),
        }
    }

    fn generate_byte_code(&mut self, ast: &BlockNode) -> Chunk {
        std::mem::take(&mut self.chunk)
    }
}
impl Visitor for CodeGenerator {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        todo!()
    }
}