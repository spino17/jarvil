use rustc_hash::FxHashMap;
use text_size::TextRange;
use crate::{
        scope::{core::{Namespace, SymbolData, IdentifierKind}, variables::VariableData, function::FunctionData, user_defined_types::UserDefinedTypeData}, 
        code::Code, ast::{walk::Visitor, ast::{ASTNode, BlockNode, CoreAtomStartNode, VariableDeclarationNode, FunctionDeclarationNode, 
        OkFunctionDeclarationNode, StructDeclarationNode, LambdaDeclarationNode, OkLambdaTypeDeclarationNode, CoreRAssignmentNode, Node, 
        CoreIdentifierNode, CoreNameTypeSpecsNode, OkIdentifierNode, TypeResolveKind, TypeExpressionNode, CoreStatemenIndentWrapperNode, CoreStatementNode
    }}, 
    error::core::{JarvilError, JarvilErrorKind}, types::core::Type
};
use std::{rc::Rc, vec};

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
        is_type: bool,
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
                    is_type
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
        identifier: &OkIdentifierNode, 
        lookup_fn: U, 
        bind_fn: V
    ) {
        let name = Rc::new(identifier.token_value(&self.code));
        match lookup_fn(&self.namespace, &name) {
            Some(symbol_data) => {
                bind_fn(identifier, &symbol_data.0, symbol_data.1);
            },
            None => self.log_undefined_identifier_in_scope_error(
                &name, identifier.range(), identifier.start_line_number()
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
            self.try_declare_and_bind(ok_identifier, declare_fn, bind_fn, false);
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
                self.try_declare_and_bind(ok_identifier, declare_fn, bind_fn, false);
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
                        Err(previous_decl_line_number) => {
                            self.log_param_with_same_name_already_exist_error(
                                &name,
                                ok_identifier.range(), 
                                ok_identifier.start_line_number(), 
                                previous_decl_line_number
                            )
                        }
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
            self.try_declare_and_bind(ok_identifier, declare_fn, bind_fn, true);
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
            self.try_declare_and_bind(ok_identifier, declare_fn, bind_fn, true);
        }
    }

    pub fn type_obj_from_expression(&mut self, type_expr: &TypeExpressionNode) -> Type {
        match type_expr.type_obj(&self.namespace, &self.code) {
            TypeResolveKind::RESOLVED(type_obj) => type_obj,
            TypeResolveKind::UNRESOLVED(identifier) => {
                let name = Rc::new(identifier.token_value(&self.code));
                self.log_undefined_identifier_in_scope_error(
                    &name, identifier.range(), identifier.start_line_number()
                );
                return Type::new_with_unknown()
            },
            TypeResolveKind::INVALID => Type::new_with_unknown()
        }
    }

    pub fn resolve_function(&mut self, func_decl: &OkFunctionDeclarationNode) {
        let core_func_decl = func_decl.core_ref();
        let func_name = &core_func_decl.name;
        let params = &core_func_decl.params;
        let return_type = &core_func_decl.return_type;
        let func_body = &core_func_decl.block;
        let mut params_vec: Vec<(String, Type)> = vec![];
        let return_type: Option<Type> = match return_type {
            Some(return_type_expr) => {
                let type_obj = self.type_obj_from_expression(return_type_expr);
                Some(type_obj)
            },
            None => None
        };
        if let Some(params) = params {
            let params_iter = params.iter();
            for param in params_iter {
                let core_param = param.core_ref();
                let name = &core_param.name;
                if let CoreIdentifierNode::OK(ok_identifier) = name.core_ref() {
                    let variable_name = ok_identifier.token_value(&self.code);
                    let type_obj = self.type_obj_from_expression(&core_param.data_type);
                    match ok_identifier.symbol_data() {
                        Some(symbol_data) => {
                            match symbol_data.0 {
                                IdentifierKind::VARIABLE(variable_symbol_data) => {
                                    variable_symbol_data.0.as_ref().borrow_mut().set_data_type(&type_obj);
                                },
                                _ => unreachable!("param name should be resolved to `SymbolData<VariableData>`")
                            }
                        },
                        None => continue
                    }
                    params_vec.push((variable_name, type_obj));
                }
            }
        }
        self.namespace = func_body.scope().expect("scope should be set to the `BlockNode` in the first phase");
        self.walk_block(func_body);
        self.namespace.close_scope();
        if let Some(identifier) = func_name {
            if let CoreIdentifierNode::OK(ok_identifier) = identifier.core_ref() {
                if let Some(symbol_data) = ok_identifier.symbol_data() {
                    match symbol_data.0 {
                        IdentifierKind::FUNCTION(func_symbol_data) => {
                            func_symbol_data.0.as_ref().borrow_mut().set_data(params_vec, return_type);
                        },
                        IdentifierKind::VARIABLE(variable_symbol_data) => {
                            todo!()
                        }
                        _ => unreachable!("function name `{}` should be resolved to `SymbolData<FunctionData>`", ok_identifier.token_value(&self.code))
                    }
                }
            }
        }
    }

    pub fn resolve_struct(&mut self, struct_decl: &StructDeclarationNode) {
        let core_struct_decl = struct_decl.core_ref();
        let mut fields_map: FxHashMap<Rc<String>, Type> = FxHashMap::default();
        let struct_body = &core_struct_decl.block;
        for stmt in &struct_body.0.as_ref().borrow().stmts {
            let stmt = match stmt.core_ref() {
                CoreStatemenIndentWrapperNode::CORRECTLY_INDENTED(stmt) => stmt.clone(),
                CoreStatemenIndentWrapperNode::INCORRECTLY_INDENTED(stmt) => {
                    stmt.core_ref().stmt.clone()
                },
                _ => continue
            };
            match stmt.core_ref() {
                CoreStatementNode::STRUCT_STATEMENT(struct_stmt) => {
                    let core_struct_stmt = struct_stmt.core_ref();
                    let name = &core_struct_stmt.name_type_spec.core_ref().name;
                    if let CoreIdentifierNode::OK(ok_identifier) = name.core_ref() {
                        let field_name = Rc::new(ok_identifier.token_value(&self.code));
                        let type_obj = self.type_obj_from_expression(
                            &core_struct_stmt.name_type_spec.core_ref().data_type
                        );
                        match fields_map.get(&field_name) {
                            Some(type_obj) => {
                                self.log_struct_field_with_same_name_exist_error(
                                    &field_name, ok_identifier.range(), ok_identifier.start_line_number(), type_obj
                                );
                            },
                            None => {
                                fields_map.insert(field_name.clone(), type_obj);
                            }
                        }
                    }
                },
                _ => unreachable!("statements other than `StructStatementNode` are not allowed in struct declaration block"),
            }
        }
        if let CoreIdentifierNode::OK(ok_identifier) = core_struct_decl.name.core_ref() {
            if let Some(symbol_data) = ok_identifier.symbol_data() {
                match symbol_data.0 {
                    IdentifierKind::USER_DEFINED_TYPE(user_defined_type_symbol_data) => {
                        match &mut *user_defined_type_symbol_data.0.as_ref().borrow_mut() {
                            UserDefinedTypeData::STRUCT(struct_data) => {
                                struct_data.set_fields(fields_map);
                            },
                            _ => unreachable!("struct name should be binded with `StructData` variant of `SymbolData<UserDefinedTypeData>`")
                        }
                    },
                    _ => unreachable!("struct name should be resolved to `SymbolData<UserDefinedTypeData>`")
                }
            }
        }
    }
    
    pub fn resolve_lambda_type(&mut self, lambda_type_decl: &OkLambdaTypeDeclarationNode) {
        let core_lambda_type_decl = lambda_type_decl.core_ref();
        let mut params_vec: Vec<(String, Type)> = vec![];
        let params = &core_lambda_type_decl.params;
        let return_type = &core_lambda_type_decl.return_type;
        let return_type: Option<Type> = match return_type {
            Some(return_type_expr) => {
                let type_obj = self.type_obj_from_expression(return_type_expr);
                Some(type_obj)
            },
            None => None
        };
        if let Some(params) = params {
            let params_iter = params.iter();
            for param in params_iter {
                let core_param = param.core_ref();
                let name = &core_param.name;
                if let CoreIdentifierNode::OK(ok_identifier) = name.core_ref() {
                    let variable_name = ok_identifier.token_value(&self.code);
                    let type_obj = self.type_obj_from_expression(&core_param.data_type);
                    params_vec.push((variable_name, type_obj));
                }
            }
        }
        if let CoreIdentifierNode::OK(ok_identifier) = core_lambda_type_decl.name.core_ref() {
            if let Some(symbol_data) = ok_identifier.symbol_data() {
                match symbol_data.0 {
                    IdentifierKind::USER_DEFINED_TYPE(user_defined_type_symbol_data) => {
                        match &mut *user_defined_type_symbol_data.0.as_ref().borrow_mut() {
                            UserDefinedTypeData::LAMBDA(lambda_data) => {
                                lambda_data.set_params_and_return_type(params_vec, return_type);
                            },
                            _ => unreachable!(
                                "lambda type name should be binded with `LambdaTypeData` variant of `SymbolData<UserDefinedTypeData>`"
                            )
                        }
                    },
                    _ => unreachable!("lambda type name should be resolved to `SymbolData<UserDefinedTypeData>`")
                }
            }
        }
    }

    pub fn log_undefined_identifier_in_scope_error(&mut self, name: &Rc<String>, error_range: TextRange, line_number: usize) {
        let start_err_index: usize = error_range.start().into();
        let end_err_index: usize = error_range.end().into();
        let (code_line, line_start_index, line_number, start_err_index) = self.code.line_data(
            line_number,
            start_err_index,
        );
        let err_str = format!("identifier `{}` is not declared in the scope", name);
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

    pub fn log_struct_field_with_same_name_exist_error(
        &mut self, 
        name: &Rc<String>, 
        error_range: TextRange, 
        line_number: usize,
        type_obj: &Type
    ) { // TODO - make this a multi-line error consisting of both the fields and the struct declaration snippet
        let start_err_index: usize = error_range.start().into();
        let end_err_index: usize = error_range.end().into();
        let (code_line, line_start_index, line_number, start_err_index) = self.code.line_data(
            line_number,
            start_err_index,
        );
        let err_str = format!("field `{}` already exists with type `{}`", name, type_obj);
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

    pub fn log_param_with_same_name_already_exist_error(
        &mut self, 
        param_name: &Rc<String>, 
        error_range: TextRange, 
        line_number: usize, 
        previous_decl_line_number: usize
    ) {
        let start_err_index: usize = error_range.start().into();
        let end_err_index: usize = error_range.end().into();
        let (code_line, line_start_index, line_number, start_err_index) = self.code.line_data(
            line_number,
            start_err_index,
        );
        let err_str = format!("param with name `{}` already exist on line {}", param_name, previous_decl_line_number);
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
                            CoreAtomStartNode::CALL(func_call) => {
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
                        self.resolve_function(func_decl);
                        return None
                    },
                    ASTNode::STRUCT_DECLARATION(struct_decl) => {
                        self.resolve_struct(struct_decl);
                        return None
                    },
                    ASTNode::OK_LAMBDA_TYPE_DECLARATION(lambda_type_decl) => {
                        self.resolve_lambda_type(lambda_type_decl);
                        return None
                    },
                    ASTNode::ATOM_START(atom_start) => {
                        match atom_start.core_ref() {
                            CoreAtomStartNode::CALL(func_call) => {
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