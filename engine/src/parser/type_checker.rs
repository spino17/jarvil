use crate::{scope::core::Namespace, code::Code, ast::{ast::{BlockNode, ASTNode}, walk::Visitor}};

pub struct TypeChecker {
    namespace: Namespace,
    code: Code,
    ast: BlockNode,
}
impl TypeChecker {
    pub fn new(code: &Code, ast: &BlockNode) -> Self {
        TypeChecker{
            namespace: ast.scope().expect("scope should be set to the `BlockNode` during resolver phase"),
            code: code.clone(),
            ast: ast.clone(),
        }
    }
}
impl Visitor for TypeChecker {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        todo!()
    }
}