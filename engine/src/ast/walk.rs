use crate::{
    ast::ast::ASTNode, parser::components::{expression::common::params, block}, scope::{function, core::Namespace}, types::expr,
};

use super::ast::{StatementNode, StatemenIndentWrapperNode};

// This kind of visitor pattern implementation is taken from Golang Programming Language
// See /src/go/ast/walk.go

fn walk_stmt<T: Visitor>(visitor: &mut T, stmt: &StatemenIndentWrapperNode) {
    visitor.walk(ASTNode::new_with_StatemenIndentWrapperNode(stmt));
}

struct Resolver {
    namespace: Namespace
}

impl Visitor for Resolver {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        todo!()
    }
}

pub trait Visitor {
    fn visit(&mut self, node: &ASTNode) -> Option<()>;

    // This method is AST walk which means it does not visit symbols. Visiting symbols can be useful while formatting
    fn walk(&mut self, node: ASTNode) where Self: Sized {
        match self.visit(&node) {
            None => return,
            _ => {}
        }
    }
}
