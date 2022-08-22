use crate::{scope::core::Namespace, code::Code, ast::{walk::Visitor, ast::{ASTNode, BlockNode}}, error::core::JarvilError};

pub enum ResolverMode {
    DECLARE,  // first pass
    RESOLVE   // second pass
}

pub struct Resolver {
    namespace: Namespace,
    code: Code,
    errors: Vec<JarvilError>,
    mode: ResolverMode,
}
impl Resolver {
    pub fn new(code: &Code) -> Self {
        Resolver{
            namespace: Namespace::new(),
            code: code.clone(),
            errors: vec![],
            mode: ResolverMode::DECLARE
        }
    }

    pub fn resolve_ast(&mut self, ast: &BlockNode) -> (Namespace, Vec<JarvilError>) {
        let code_block = ast.0.as_ref().borrow();
        for stmt in &code_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        self.mode = ResolverMode::RESOLVE;
        for stmt in &code_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        (std::mem::take(&mut self.namespace), std::mem::take(&mut self.errors))
    }
}
impl Visitor for Resolver {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match self.mode {
            ResolverMode::DECLARE => {
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
                    ASTNode::ATOM_START(atom_start) => {
                        todo!();
                        return None
                    },
                    _ => return Some(())
                }
            },
            ResolverMode::RESOLVE => {
                todo!()
            }
        }
    }
}