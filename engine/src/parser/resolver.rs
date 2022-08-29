use text_size::TextRange;
use crate::{
        scope::{core::{Namespace, SymbolData}, variables::VariableData, function::FunctionData, user_defined_types::UserDefinedTypeData}, 
        code::Code, ast::{walk::Visitor, ast::{ASTNode, BlockNode, CoreAtomStartNode, VariableDeclarationNode, FunctionDeclarationNode, 
        OkFunctionDeclarationNode, StructDeclarationNode, LambdaDeclarationNode, OkLambdaTypeDeclarationNode, CoreRAssignmentNode, Node, 
        CoreIdentifierNode, CoreNameTypeSpecsNode, OkIdentifierNode
    }}, 
    error::core::{JarvilError, JarvilErrorKind}
};
use std::rc::Rc;

pub enum ResolverMode {
    DECLARE,  // first pass
    RESOLVE   // second pass
}

pub struct Resolver {
    namespace: Namespace,
    pub code: Code,
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

    pub fn try_declare_and_bind<
        T, 
        U: Fn(&Namespace, &Rc<String>, usize) -> Result<SymbolData<T>, usize>, 
        V: Fn(&OkIdentifierNode, &SymbolData<T>)
    >(
        &mut self, 
        identifier: &OkIdentifierNode,
        declare_fn: U,
        bind_fn: V,
    ) {
        let name = Rc::new(identifier.token_value(&self.code));
        let symbol_data = declare_fn(
            &self.namespace, &name, identifier.start_line_number()
        );
        match symbol_data {
            Ok(symbol_data) => bind_fn(identifier, &symbol_data),
            Err(previous_decl_line_number) => {
                self.log_identifier_already_declared_in_current_scope_error(
                    &name, 
                    identifier.range(), 
                    previous_decl_line_number, 
                    identifier.start_line_number(), 
                    false
                );
            }
        }
    }

    pub fn try_resolving<
        T, 
        U: Fn(&Namespace, &Rc<String>) -> Option<(SymbolData<T>, usize)>,
        V: Fn(&OkIdentifierNode, &SymbolData<T>, usize)
    >(
        &mut self, 
        identifer: &OkIdentifierNode, 
        lookup_fn: U, 
        bind_fn: V
    ) {
        let name = Rc::new(identifer.token_value(&self.code));
        match lookup_fn(&self.namespace, &name) {
            Some(symbol_data) => {
                bind_fn(identifer, &symbol_data.0, symbol_data.1);
            },
            None => self.log_undefined_identifier_in_scope_error(
                &name, identifer.range(), identifer.start_line_number()
            )
        }
    }

    pub fn declare_variable(&mut self, variable_decl: &VariableDeclarationNode) {
        let core_variable_decl = variable_decl.core_ref();
        self.walk_r_assignment(&core_variable_decl.r_assign);
        if let CoreIdentifierNode::OK(ok_identifier) = core_variable_decl.name.core_ref() {
            let declare_fn = |namespace: &Namespace, name: &Rc<String>, start_line_number: usize| {
                namespace.declare_variable(name, start_line_number)
            };
            let bind_fn = |identifier: &OkIdentifierNode, symbol_data: &SymbolData<VariableData>| {
                identifier.bind_variable_decl(symbol_data, 0)
            };
            self.try_declare_and_bind(ok_identifier, declare_fn, bind_fn);
        }
    }

    pub fn declare_function(&mut self, func_decl: &OkFunctionDeclarationNode) {
        let core_func_decl = func_decl.core_ref();
        let func_name = &core_func_decl.name;
        let params = &core_func_decl.params;
        let func_body = &core_func_decl.block;
        if let Some(identifier) = func_name {
            if let CoreIdentifierNode::OK(ok_identifier) = identifier.core_ref() {
                let declare_fn = |namespace: &Namespace, name: &Rc<String>, start_line_number: usize| {
                    namespace.declare_function(name, start_line_number)
                };
                let bind_fn = |identifier: &OkIdentifierNode, symbol_data: &SymbolData<FunctionData>| {
                    identifier.bind_function_decl(symbol_data, 0)
                };
                self.try_declare_and_bind(ok_identifier, declare_fn, bind_fn);
            }
        }
        self.namespace.open_scope();
        if let Some(params) = params {
            let params_iter = params.iter();
            for param in params_iter {
                let core_param = param.core_ref();
                let param_name = &core_param.name;
                if let CoreIdentifierNode::OK(ok_identifier) = param_name.core_ref() {
                    let name = Rc::new(ok_identifier.token_value(&self.code));
                    match self.namespace.declare_variable(&name, ok_identifier.start_line_number()) {
                        Ok(symbol_data) => ok_identifier.bind_variable_decl(&symbol_data, 0),
                        Err(_) => unreachable!("new scope cannot have params already set")
                    }
                }
            }
        }
        self.walk_block(func_body);
        func_body.set_scope(&self.namespace);
        self.namespace.close_scope();
    }

    pub fn declare_struct(&mut self, struct_decl: &StructDeclarationNode) {
        let core_struct_decl = struct_decl.core_ref();
        if let CoreIdentifierNode::OK(ok_identifier) = core_struct_decl.name.core_ref() {
            let declare_fn = |namespace: &Namespace, name: &Rc<String>, start_line_number: usize| {
                namespace.declare_struct_type(name, start_line_number)
            };
            let bind_fn = |identifier: &OkIdentifierNode, symbol_data: &SymbolData<UserDefinedTypeData>| {
                identifier.bind_user_defined_type_decl(symbol_data, 0)
            };
            self.try_declare_and_bind(ok_identifier, declare_fn, bind_fn);
        }
    }

    pub fn declare_lambda_type(&mut self, lambda_type_decl: &OkLambdaTypeDeclarationNode) {
        let core_lambda_type_decl = lambda_type_decl.core_ref();
        if let CoreIdentifierNode::OK(ok_identifier) = core_lambda_type_decl.name.core_ref() {
            let declare_fn = |namespace: &Namespace, name: &Rc<String>, start_line_number: usize| {
                namespace.declare_lambda_type(name, start_line_number)
            };
            let bind_fn = |identifier: &OkIdentifierNode, symbol_data: &SymbolData<UserDefinedTypeData>| {
                identifier.bind_user_defined_type_decl(symbol_data, 0)
            };
            self.try_declare_and_bind(ok_identifier, declare_fn, bind_fn);
        }
    }

    pub fn log_undefined_identifier_in_scope_error(&mut self, name: &Rc<String>, error_range: TextRange, line_number: usize) {
        let start_err_index: usize = error_range.start().into();
        let end_err_index: usize = error_range.end().into();
        let (code_line, line_start_index, line_number, start_err_index) = self.code.line_data(
            line_number,
            start_err_index,
        );
        let err_str = format!("identifier `{}` not declared in the scope", name);
        let err = JarvilError::form_single_line_error(
            start_err_index,
            end_err_index,
            line_number,
            line_start_index,
            code_line,
            err_str,
            JarvilErrorKind::SEMANTIC_ERROR,
        );
        self.errors.push(err);
    }

    pub fn log_identifier_already_declared_in_current_scope_error(
        &mut self, 
        name: &Rc<String>, 
        error_range: TextRange, 
        previous_decl_line: usize, 
        line_number: usize,
        is_type: bool,
    ) {
        let start_err_index: usize = error_range.start().into();
        let end_err_index: usize = error_range.end().into();
        let (code_line, line_start_index, line_number, start_err_index) = self.code.line_data(
            line_number,
            start_err_index,
        );
        let err_str = if is_type {
            format!("type `{}` is already declared in the scope on line {}", name, previous_decl_line)
        } else {
            format!("identifier `{}` is already declared in the current block on line {}", name, previous_decl_line)
        };
        let err = JarvilError::form_single_line_error(
            start_err_index,
            end_err_index,
            line_number,
            line_start_index,
            code_line,
            err_str,
            JarvilErrorKind::SEMANTIC_ERROR,
        );
        self.errors.push(err);
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
                        self.declare_lambda_type(lambda_type_decl);
                        return None
                    },
                    ASTNode::ATOM_START(atom_start) => {
                        match atom_start.core_ref() {
                            CoreAtomStartNode::IDENTIFIER(identifier) => {
                                if let CoreIdentifierNode::OK(ok_identifier) = identifier.core_ref() {
                                    let lookup_fn = |namespace: &Namespace, key: &Rc<String>| {
                                        namespace.lookup_in_variables_namespace(key)
                                    };
                                    let bind_fn 
                                    = |identifier: &OkIdentifierNode, symbol_data: &SymbolData<VariableData>, depth: usize| {
                                        identifier.bind_variable_decl(symbol_data, depth)
                                    };
                                    self.try_resolving(ok_identifier, lookup_fn, bind_fn);
                                }
                            },
                            CoreAtomStartNode::FUNCTION_CALL(func_call) => {
                                let core_func_call = func_call.core_ref();
                                if let CoreIdentifierNode::OK(ok_identifier) = core_func_call.function_name.core_ref() {
                                    let lambda_name = Rc::new(ok_identifier.token_value(&self.code));
                                    if let Some(symbol_data)
                                    = self.namespace.lookup_in_variables_namespace(&lambda_name) {
                                        ok_identifier.bind_variable_decl(&symbol_data.0, symbol_data.1);
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
                    ASTNode::OK_FUNCTION_DECLARATION(func_decl) => {
                        todo!();
                        return None
                    },
                    ASTNode::STRUCT_DECLARATION(struct_decl) => {
                        todo!();
                        return None
                    },
                    ASTNode::OK_LAMBDA_TYPE_DECLARATION(lambda_type_decl) => {
                        todo!();
                        return None
                    },
                    ASTNode::ATOM_START(atom_start) => {
                        match atom_start.core_ref() {
                            CoreAtomStartNode::FUNCTION_CALL(func_call) => {
                                let core_func_call = func_call.core_ref();
                                if let CoreIdentifierNode::OK(ok_identifier) = core_func_call.function_name.core_ref() {
                                    if !ok_identifier.is_resolved() {
                                        let lookup_fn = |namespace: &Namespace, key: &Rc<String>| {
                                            namespace.lookup_in_functions_namespace(key)
                                        };
                                        let bind_fn 
                                        = |identifier: &OkIdentifierNode, symbol_data: &SymbolData<FunctionData>, depth: usize| {
                                            identifier.bind_function_decl(symbol_data, depth)
                                        };
                                        self.try_resolving(ok_identifier, lookup_fn, bind_fn);
                                    }
                                }
                            },
                            CoreAtomStartNode::CLASS_METHOD_CALL(class_method_call) => {
                                let core_class_method_call = class_method_call.core_ref();
                                if let CoreIdentifierNode::OK(ok_identifier) = core_class_method_call.class_name.core_ref() {
                                    let lookup_fn = |namespace: &Namespace, key: &Rc<String>| {
                                        namespace.lookup_in_types_namespace(key)
                                    };
                                    let bind_fn 
                                    = |identifier: &OkIdentifierNode, symbol_data: &SymbolData<UserDefinedTypeData>, depth: usize| {
                                        identifier.bind_user_defined_type_decl(symbol_data, depth)
                                    };
                                    self.try_resolving(ok_identifier, lookup_fn, bind_fn);
                                }
                            },
                            _ => {}
                        }
                        return None
                    },
                    _ => return Some(())
                }
            }
        }
    }
}