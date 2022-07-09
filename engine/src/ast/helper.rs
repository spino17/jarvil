use super::ast::{TokenNode, BlockNode};

pub enum IndentNode {
    TOKEN(TokenNode),
    BLOCK(BlockNode),
}