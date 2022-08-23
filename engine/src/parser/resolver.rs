use crate::{scope::core::Namespace, code::Code, ast::{walk::Visitor, ast::{ASTNode, BlockNode, CoreAtomStartNode, VariableDeclarationNode, FunctionDeclarationNode, OkFunctionDeclarationNode, StructDeclarationNode, LambdaDeclarationNode, OkLambdaTypeDeclarationNode}}, error::core::JarvilError};

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

    pub fn declare_variable(&mut self, variable_decl: &VariableDeclarationNode) {
        todo!()
    }

    pub fn declare_function(&mut self, func_decl: &OkFunctionDeclarationNode) {
        todo!()
    }

    pub fn declare_struct(&mut self, struct_decl: &StructDeclarationNode) {
        todo!()
    }

    pub fn declare_lambda(&mut self, lambda_decl: &OkLambdaTypeDeclarationNode) {
        todo!()
    }
}
impl Visitor for Resolver {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match self.mode {
            ResolverMode::DECLARE => {
                match node {
                    ASTNode::VARIABLE_DECLARATION(variable_decl) => {
                        self.declare_variable(variable_decl);
                        return None
                    },
                    ASTNode::OK_FUNCTION_DECLARATION(func_decl) => {
                        self.declare_function(func_decl);
                        return None
                    },
                    ASTNode::STRUCT_DECLARATION(struct_decl) => {
                        self.declare_struct(struct_decl);
                        return None
                    },
                    ASTNode::OK_LAMBDA_TYPE_DECLARATION(lambda_type_decl) => {
                        self.declare_lambda(lambda_type_decl);
                        return None
                    },
                    ASTNode::ATOM_START(atom_start) => {
                        match atom_start.core_ref() {
                            CoreAtomStartNode::IDENTIFIER(identifier) => todo!(),
                            CoreAtomStartNode::FUNCTION_CALL(func_call) => todo!(),
                            _ => {}
                        }
                        return None
                    },
                    _ => return Some(())
                }
            },
            ResolverMode::RESOLVE => {
                match node {
                    ASTNode::ATOM_START(atom_start) => {
                        match atom_start.core_ref() {
                            CoreAtomStartNode::FUNCTION_CALL(func_call) => todo!(),
                            CoreAtomStartNode::CLASS_METHOD_CALL(class_method_call) => todo!(),
                            _ => {}
                        }
                        return None
                    },
                    ASTNode::USER_DEFINED_TYPE(user_defined_type) => {
                        todo!();
                        return None
                    },
                    _ => return Some(())
                }
            }
        }
    }
}