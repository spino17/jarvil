use super::ast::{BlockNode, StatementNode, ASTNode};

pub trait Visitor {
    fn visitBlock(&self, block: &BlockNode) {
        todo!()
    }
    fn visitStatement(&self, stmt: &StatementNode);
    // TODO - define for all other nodes
}

struct Resolver {
    //
}

impl Visitor for Resolver {
    fn visitStatement(&self, stmt: &StatementNode) {
        //
    }
}

struct Formatter {
    //
}

impl Visitor for Formatter {
    fn visitStatement(&self, stmt: &StatementNode) {
        //
    }
}

pub trait Node {
    fn accept<T: Visitor>(visitor: &T);
}