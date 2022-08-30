use crate::{scope::core::Namespace, code::Code, ast::{ast::{BlockNode, ASTNode, StatementNode}, walk::Visitor}, error::core::JarvilError};

pub struct TypeChecker {
    namespace: Namespace,
    code: Code,
    errors: Vec<JarvilError>,
}
impl TypeChecker {
    pub fn new(code: &Code, scope: &Namespace) -> Self {
        TypeChecker{
            namespace: scope.clone(),
            code: code.clone(),
            errors: vec![],
        }
    }

    pub fn check_ast(&mut self, ast: &BlockNode) -> Vec<JarvilError> {
        let core_block = ast.0.as_ref().borrow();
        for stmt in &core_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        std::mem::take(&mut self.errors)
    }

    pub fn check_stmt(&mut self, stmt: &StatementNode) {
        // TODO - type-check all the statements here
        todo!()
    }
}
impl Visitor for TypeChecker {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match node {
            ASTNode::STATEMENT(stmt) => {
                self.check_stmt(stmt);
                return None
            },
            _ => Some(())
        }
    }
}