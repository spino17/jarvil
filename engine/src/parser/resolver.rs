use crate::{scope::core::Namespace, code::Code, ast::{walk::Visitor, ast::{ASTNode, BlockNode}}, errors::JarvilError};

pub struct Resolver {
    namespace: Namespace,
    code: Code,
    errors: Vec<JarvilError>,
}
impl Resolver {
    pub fn new(code: &Code) -> Self {
        Resolver{
            namespace: Namespace::new(),
            code: code.clone(),
            errors: vec![],
        }
    }

    pub fn resolve_ast(&mut self, ast: &BlockNode) -> (Namespace, Vec<JarvilError>) {
        let code_block = ast.0.as_ref().borrow();
        for stmt in &code_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        (std::mem::take(&mut self.namespace), std::mem::take(&mut self.errors))
    }
}
impl Visitor for Resolver {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        // TODO - check critical nodes like declaration and for them walk the tree manually and return `None` else return Some(())
        // to continue walking deep nodes
        match node {
            ASTNode::VARIABLE_DECLARATION(variable_decl) => {
                todo!();
                return None
            },
            ASTNode::OK_FUNCTION_DECLARATION(func_decl) => {
                todo!();
                return None
            },
            ASTNode::STRUCT_DECLARATION(struct_decl) => {
                todo!();
                return None
            },
            ASTNode::OK_LAMBDA_DECLARATION(ok_lambda_decl) => {
                todo!();
                return None
            },
            ASTNode::OK_TOKEN(ok_token) => {
                todo!();
                return None
            },
            _ => return Some(())
        }
    }
}