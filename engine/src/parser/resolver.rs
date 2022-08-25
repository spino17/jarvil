use crate::{
    scope::{core::{Namespace, SymbolData}, variables::VariableData}, code::Code, ast::{walk::Visitor, ast::{ASTNode, BlockNode, 
    CoreAtomStartNode, VariableDeclarationNode, FunctionDeclarationNode, OkFunctionDeclarationNode, StructDeclarationNode, 
    LambdaDeclarationNode, OkLambdaTypeDeclarationNode, CoreRAssignmentNode, Node
    }}, 
    error::core::JarvilError
};

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

    pub fn declare_variable(&mut self, variable_name: String, is_init: bool, line_number: usize) {
        self.namespace.declare_variable(variable_name, is_init, line_number);
    }

    pub fn declare_function(&mut self, func_decl: &OkFunctionDeclarationNode) {
        todo!()
    }

    pub fn declare_struct(&mut self, struct_decl: &StructDeclarationNode) {
        todo!()
    }

    pub fn declare_lambda_type(&mut self, lambda: &OkLambdaTypeDeclarationNode) {
        todo!()
    }
}
impl Visitor for Resolver {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match self.mode {
            ResolverMode::DECLARE => {
                match node {
                    ASTNode::VARIABLE_DECLARATION(variable_decl) => {
                        let core_variable_decl = variable_decl.core_ref();
                        self.walk_r_assignment(&core_variable_decl.r_assign);
                        if let Some(name) = core_variable_decl.name.value(&self.code) {
                            match core_variable_decl.r_assign.core_ref() {
                                CoreRAssignmentNode::EXPRESSION(_) | CoreRAssignmentNode::LAMBDA(_) => {
                                    self.declare_variable(name, true, variable_decl.start_line_number())
                                },
                                _ => {}
                            };
                        }
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
                        self.declare_lambda_type(lambda_type_decl);
                        return None
                    },
                    ASTNode::ATOM_START(atom_start) => {
                        match atom_start.core_ref() {
                            CoreAtomStartNode::IDENTIFIER(identifier) => {
                                if let Some(variable_name) = identifier.value(&self.code) {
                                    match self.namespace.lookup_in_variables_namespace(&variable_name) {
                                        Some(symbol_data) => todo!(), // bind the symbol data to identifier node
                                        None => todo!(), // TODO - raise error variable `{}` is not declared in the scope
                                    }
                                }
                            },
                            CoreAtomStartNode::FUNCTION_CALL(func_call) => {
                                let core_func_call = func_call.core_ref();
                                if let Some(lambda_name) = core_func_call.function_name.value(&self.code) {
                                    if let Some(symbol_data) 
                                    = self.namespace.lookup_in_variables_namespace(&lambda_name) {
                                        todo!()  // bind the symbol data to identifier node
                                    }
                                }
                            },
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
                            CoreAtomStartNode::FUNCTION_CALL(func_call) => {
                                // TODO - check if the node is already bind to some declaration
                                todo!()
                            },
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