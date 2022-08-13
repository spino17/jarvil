use crate::{
    ast::ast::ASTNode, parser::components::expression::common::params, scope::function, types::expr,
};

// This kind of visitor pattern implementation is taken from Golang Programming Language
// See /src/go/ast/walk.go
pub trait Visitor {
    fn visit(&mut self, node: &ASTNode) -> Option<()>;

    // This method is AST walk which means it does not visit symbols. Visiting symbols can be useful while formatting
    fn walk(&mut self, node: ASTNode) {
        match self.visit(&node) {
            None => return,
            _ => {}
        }
    }
}
