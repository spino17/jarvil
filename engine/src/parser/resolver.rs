use crate::{
    ast::{
        ast::{
            ASTNode, BlockNode, CoreAtomStartNode, CoreIdentifierNode, CoreNameTypeSpecsNode,
            CoreRAssignmentNode, CoreStatemenIndentWrapperNode, CoreStatementNode,
            FunctionDeclarationNode, FunctionKind, LambdaDeclarationNode, Node,
            OkFunctionDeclarationNode, OkIdentifierNode, OkLambdaTypeDeclarationNode,
            StructDeclarationNode, TypeExpressionNode, TypeResolveKind, VariableDeclarationNode,
        },
        walk::Visitor,
    },
    code::Code,
    error::{
        constants::{
            LAMBDA_NAME_NOT_BINDED_WITH_LAMBDA_VARIANT_SYMBOL_DATA_MSG, SCOPE_NOT_SET_TO_BLOCK_MSG,
            STRUCT_NAME_NOT_BINDED_WITH_STRUCT_VARIANT_SYMBOL_DATA_MSG,
        },
        core::{JarvilError, JarvilErrorKind},
    },
    scope::{
        core::{IdentifierKind, Namespace, SymbolData},
        function::FunctionData,
        user_defined_types::{LambdaTypeData, UserDefinedTypeData},
        variables::VariableData,
    },
    types::core::Type,
};
use rustc_hash::FxHashMap;
use std::{rc::Rc, vec};
use text_size::TextRange;

type Set<K> = FxHashMap<K, ()>;

pub enum ResolverMode {
    DECLARE, // first pass
    RESOLVE, // second pass
}

pub struct Resolver {
    namespace: Namespace,
    pub code: Code,
    errors: Vec<JarvilError>,
    mode: ResolverMode,
}
impl Resolver {
    pub fn new(code: &Code) -> Self {
        Resolver {
            namespace: Namespace::new(),
            code: code.clone(),
            errors: vec![],
            mode: ResolverMode::DECLARE,
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
        (
            std::mem::take(&mut self.namespace),
            std::mem::take(&mut self.errors),
        )
    }

    pub fn try_declare_and_bind<
        T,
        U: Fn(&Namespace, &Rc<String>, usize) -> Result<SymbolData<T>, usize>,
        V: Fn(&OkIdentifierNode, &SymbolData<T>),
    >(
        &mut self,
        identifier: &OkIdentifierNode,
        declare_fn: U,
        bind_fn: V,
    ) -> Option<(Rc<String>, usize)> {
        let name = Rc::new(identifier.token_value(&self.code));
        let symbol_data = declare_fn(&self.namespace, &name, identifier.start_line_number());
        match symbol_data {
            Ok(symbol_data) => {
                bind_fn(identifier, &symbol_data);
                None
            }
            Err(previous_decl_line_number) => Some((name, previous_decl_line_number)),
        }
    }

    pub fn try_resolving<
        T,
        U: Fn(&Namespace, &Rc<String>) -> Option<(SymbolData<T>, usize)>,
        V: Fn(&OkIdentifierNode, &SymbolData<T>, usize),
    >(
        &mut self,
        identifier: &OkIdentifierNode,
        lookup_fn: U,
        bind_fn: V,
    ) -> Option<Rc<String>> {
        let name = Rc::new(identifier.token_value(&self.code));
        match lookup_fn(&self.namespace, &name) {
            Some(symbol_data) => {
                bind_fn(identifier, &symbol_data.0, symbol_data.1);
                None
            }
            None => Some(name),
        }
    }

    pub fn try_resolving_variable(&mut self, identifier: &OkIdentifierNode) -> Option<Rc<String>> {
        let lookup_fn =
            |namespace: &Namespace, key: &Rc<String>| namespace.lookup_in_variables_namespace(key);
        let bind_fn =
            |identifier: &OkIdentifierNode,
             symbol_data: &SymbolData<VariableData>,
             depth: usize| { identifier.bind_variable_decl(symbol_data, depth) };
        self.try_resolving(identifier, lookup_fn, bind_fn)
    }

    pub fn try_resolving_function(&mut self, identifier: &OkIdentifierNode) -> Option<Rc<String>> {
        let lookup_fn =
            |namespace: &Namespace, key: &Rc<String>| namespace.lookup_in_functions_namespace(key);
        let bind_fn =
            |identifier: &OkIdentifierNode,
             symbol_data: &SymbolData<FunctionData>,
             depth: usize| { identifier.bind_function_decl(symbol_data, depth) };
        self.try_resolving(identifier, lookup_fn, bind_fn)
    }

    pub fn try_resolving_user_defined_type(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<Rc<String>> {
        let lookup_fn =
            |namespace: &Namespace, key: &Rc<String>| namespace.lookup_in_types_namespace(key);
        let bind_fn = |identifier: &OkIdentifierNode,
                       symbol_data: &SymbolData<UserDefinedTypeData>,
                       depth: usize| {
            identifier.bind_user_defined_type_decl(symbol_data, depth)
        };
        self.try_resolving(identifier, lookup_fn, bind_fn)
    }

    pub fn try_declare_and_bind_variable(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<(Rc<String>, usize)> {
        let declare_fn = |namespace: &Namespace, name: &Rc<String>, start_line_number: usize| {
            namespace.declare_variable(name, start_line_number)
        };
        let bind_fn = |identifier: &OkIdentifierNode, symbol_data: &SymbolData<VariableData>| {
            identifier.bind_variable_decl(symbol_data, 0)
        };
        self.try_declare_and_bind(identifier, declare_fn, bind_fn)
    }

    pub fn try_declare_and_bind_function(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<(Rc<String>, usize)> {
        let declare_fn = |namespace: &Namespace, name: &Rc<String>, start_line_number: usize| {
            namespace.declare_function(name, start_line_number)
        };
        let bind_fn = |identifier: &OkIdentifierNode, symbol_data: &SymbolData<FunctionData>| {
            identifier.bind_function_decl(symbol_data, 0)
        };
        self.try_declare_and_bind(identifier, declare_fn, bind_fn)
    }

    pub fn try_declare_and_bind_struct_type(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<(Rc<String>, usize)> {
        let declare_fn = |namespace: &Namespace, name: &Rc<String>, start_line_number: usize| {
            namespace.declare_struct_type(name, start_line_number)
        };
        let bind_fn = |identifier: &OkIdentifierNode,
                       symbol_data: &SymbolData<UserDefinedTypeData>| {
            identifier.bind_user_defined_type_decl(symbol_data, 0)
        };
        self.try_declare_and_bind(identifier, declare_fn, bind_fn)
    }

    pub fn try_declare_and_bind_lambda_type(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<(Rc<String>, usize)> {
        let declare_fn = |namespace: &Namespace, name: &Rc<String>, start_line_number: usize| {
            namespace.declare_lambda_type(name, start_line_number)
        };
        let bind_fn = |identifier: &OkIdentifierNode,
                       symbol_data: &SymbolData<UserDefinedTypeData>| {
            identifier.bind_user_defined_type_decl(symbol_data, 0)
        };
        self.try_declare_and_bind(identifier, declare_fn, bind_fn)
    }

    pub fn declare_variable(&mut self, variable_decl: &VariableDeclarationNode) {
        let core_variable_decl = variable_decl.core_ref();
        self.walk_r_assignment(&core_variable_decl.r_assign);
        if let CoreRAssignmentNode::LAMBDA(_) = core_variable_decl.r_assign.core_ref() {
            return;
        }
        if let CoreIdentifierNode::OK(ok_identifier) = core_variable_decl.name.core_ref() {
            if let Some((name, previous_decl_line)) =
                self.try_declare_and_bind_variable(ok_identifier)
            {
                let err_message = format!(
                    "variable `{}` is already declared in the current block on line {}",
                    name, previous_decl_line
                );
                self.log_error(
                    ok_identifier.range(),
                    ok_identifier.start_line_number(),
                    err_message,
                );
            }
        }
    }

    pub fn declare_function(&mut self, func_decl: &OkFunctionDeclarationNode) {
        let core_func_decl = func_decl.core_ref();
        let func_name = &core_func_decl.name;
        let params = &core_func_decl.params;
        let func_body = &core_func_decl.block;
        let kind = &core_func_decl.kind;
        self.namespace.open_scope();
        if let Some(params) = params {
            let params_iter = params.iter();
            for param in params_iter {
                let core_param = param.core_ref();
                let param_name = &core_param.name;
                if let CoreIdentifierNode::OK(ok_identifier) = param_name.core_ref() {
                    let name = Rc::new(ok_identifier.token_value(&self.code));
                    match self
                        .namespace
                        .declare_variable(&name, ok_identifier.start_line_number())
                    {
                        Ok(symbol_data) => ok_identifier.bind_variable_decl(&symbol_data, 0),
                        Err(_) => {
                            let err_message =
                                format!("argument with name `{}` already exist", name);
                            self.log_error(
                                ok_identifier.range(),
                                ok_identifier.start_line_number(),
                                err_message,
                            )
                        }
                    }
                }
            }
        }
        self.walk_block(func_body);
        func_body.set_scope(&self.namespace);
        self.namespace.close_scope();
        if let Some(identifier) = func_name {
            if let CoreIdentifierNode::OK(ok_identifier) = identifier.core_ref() {
                match kind {
                    FunctionKind::FUNC => {
                        if let Some((name, previous_decl_line)) =
                            self.try_declare_and_bind_function(ok_identifier)
                        {
                            let err_message = format!(
                                "function `{}` is already declared in the current block on line {}",
                                name, previous_decl_line
                            );
                            self.log_error(
                                ok_identifier.range(),
                                ok_identifier.start_line_number(),
                                err_message,
                            );
                        }
                    }
                    FunctionKind::LAMBDA => {
                        if let Some((name, previous_decl_line)) =
                            self.try_declare_and_bind_variable(ok_identifier)
                        {
                            let err_message = format!(
                                "variable `{}` is already declared in the current block on line {}",
                                name, previous_decl_line
                            );
                            self.log_error(
                                ok_identifier.range(),
                                ok_identifier.start_line_number(),
                                err_message,
                            );
                        }
                    }
                    FunctionKind::METHOD => todo!(),
                }
            }
        }
    }

    pub fn declare_struct(&mut self, struct_decl: &StructDeclarationNode) {
        let core_struct_decl = struct_decl.core_ref();
        if let CoreIdentifierNode::OK(ok_identifier) = core_struct_decl.name.core_ref() {
            if let Some((name, previous_decl_line)) =
                self.try_declare_and_bind_struct_type(ok_identifier)
            {
                let err_message = format!(
                    "type `{}` is already declared in the scope on line {}",
                    name, previous_decl_line
                );
                self.log_error(
                    ok_identifier.range(),
                    ok_identifier.start_line_number(),
                    err_message,
                );
            }
        }
    }

    pub fn declare_lambda_type(&mut self, lambda_type_decl: &OkLambdaTypeDeclarationNode) {
        let core_lambda_type_decl = lambda_type_decl.core_ref();
        if let CoreIdentifierNode::OK(ok_identifier) = core_lambda_type_decl.name.core_ref() {
            if let Some((name, previous_decl_line)) =
                self.try_declare_and_bind_lambda_type(ok_identifier)
            {
                let err_message = format!(
                    "type `{}` is already declared in the scope on line {}",
                    name, previous_decl_line
                );
                self.log_error(
                    ok_identifier.range(),
                    ok_identifier.start_line_number(),
                    err_message,
                );
            }
        }
    }

    pub fn type_obj_from_expression(&mut self, type_expr: &TypeExpressionNode) -> Type {
        match type_expr.type_obj(&self.namespace, &self.code) {
            TypeResolveKind::RESOLVED(type_obj) => type_obj,
            TypeResolveKind::UNRESOLVED(identifier) => {
                let name = Rc::new(identifier.token_value(&self.code));
                let err_message = format!("identifier `{}` is not declared in the scope", name);
                self.log_error(
                    identifier.range(),
                    identifier.start_line_number(),
                    err_message,
                );
                return Type::new_with_unknown();
            }
            TypeResolveKind::INVALID => Type::new_with_unknown(),
        }
    }

    pub fn resolve_function(&mut self, func_decl: &OkFunctionDeclarationNode) {
        let core_func_decl = func_decl.core_ref();
        let func_name = &core_func_decl.name;
        let params = &core_func_decl.params;
        let return_type = &core_func_decl.return_type;
        let func_body = &core_func_decl.block;
        let mut params_vec: Vec<(Rc<String>, Type)> = vec![];
        let return_type: Type = match return_type {
            Some(return_type_expr) => {
                let type_obj = self.type_obj_from_expression(return_type_expr);
                type_obj
            }
            None => Type::new_with_void(),
        };
        if let Some(params) = params {
            let params_iter = params.iter();
            for param in params_iter {
                let core_param = param.core_ref();
                let name = &core_param.name;
                if let CoreIdentifierNode::OK(ok_identifier) = name.core_ref() {
                    if let Some(symbol_data) = ok_identifier.variable_symbol_data(
                        "param name should be resolved to `SymbolData<VariableData>`",
                    ) {
                        let variable_name = Rc::new(ok_identifier.token_value(&self.code));
                        let type_obj = self.type_obj_from_expression(&core_param.data_type);
                        symbol_data.0.as_ref().borrow_mut().set_data_type(&type_obj);
                        params_vec.push((variable_name, type_obj));
                    }
                }
            }
        }
        self.namespace = func_body.scope().expect(SCOPE_NOT_SET_TO_BLOCK_MSG);
        self.walk_block(func_body);
        self.namespace.close_scope();
        let kind = &core_func_decl.kind;
        if let Some(identifier) = func_name {
            if let CoreIdentifierNode::OK(ok_identifier) = identifier.core_ref() {
                if let Some(symbol_data) = ok_identifier.symbol_data() {
                    match symbol_data.0 {
                        IdentifierKind::FUNCTION(func_symbol_data) => {
                            assert!(kind.clone() == FunctionKind::FUNC);
                            func_symbol_data
                                .0
                                .as_ref()
                                .borrow_mut()
                                .set_data(params_vec, return_type);
                        }
                        IdentifierKind::VARIABLE(variable_symbol_data) => {
                            assert!(kind.clone() == FunctionKind::LAMBDA);
                            let symbol_data = UserDefinedTypeData::LAMBDA(LambdaTypeData::new(
                                params_vec,
                                return_type,
                            ));
                            let lambda_type_obj = Type::new_with_lambda(
                                None,
                                &SymbolData::new(symbol_data, ok_identifier.start_line_number()),
                            );
                            variable_symbol_data
                                .0
                                .as_ref()
                                .borrow_mut()
                                .set_data_type(&lambda_type_obj);
                        }
                        _ => unreachable!(
                            "function name should be resolved to `SymbolData<FunctionData>` or `SymbolData<VariableData>`"
                        ),
                    }
                }
            }
        }
    }

    pub fn resolve_struct(&mut self, struct_decl: &StructDeclarationNode) {
        let core_struct_decl = struct_decl.core_ref();
        let mut fields_map: FxHashMap<String, Type> = FxHashMap::default();
        let struct_body = &core_struct_decl.block;
        for stmt in &struct_body.0.as_ref().borrow().stmts {
            let stmt = match stmt.core_ref() {
                CoreStatemenIndentWrapperNode::CORRECTLY_INDENTED(stmt) => stmt.clone(),
                CoreStatemenIndentWrapperNode::INCORRECTLY_INDENTED(stmt) => {
                    stmt.core_ref().stmt.clone()
                }
                _ => continue,
            };
            match stmt.core_ref() {
                CoreStatementNode::STRUCT_STATEMENT(struct_stmt) => {
                    let core_struct_stmt = struct_stmt.core_ref();
                    let name = &core_struct_stmt.name_type_spec.core_ref().name;
                    if let CoreIdentifierNode::OK(ok_identifier) = name.core_ref() {
                        let field_name = ok_identifier.token_value(&self.code);
                        let type_obj = self.type_obj_from_expression(
                            &core_struct_stmt.name_type_spec.core_ref().data_type
                        );
                        match fields_map.get(&field_name) {
                            Some(type_obj) => {
                                let err_message = format!("field with name `{}` already exists with type `{}`", field_name, type_obj);
                                self.log_error(
                                    ok_identifier.range(),
                                    ok_identifier.start_line_number(),
                                    err_message,
                                );
                            },
                            None => {
                                fields_map.insert(field_name, type_obj);
                            }
                        }
                    }
                },
                _ => unreachable!("statements other than `StructStatementNode` are not allowed in struct declaration block"),
            }
        }
        if let CoreIdentifierNode::OK(ok_identifier) = core_struct_decl.name.core_ref() {
            if let Some(symbol_data) = ok_identifier.user_defined_type_symbol_data(
                "struct name should be resolved to `SymbolData<UserDefinedTypeData>`",
            ) {
                symbol_data
                    .0
                    .as_ref()
                    .borrow_mut()
                    .struct_data_mut(STRUCT_NAME_NOT_BINDED_WITH_STRUCT_VARIANT_SYMBOL_DATA_MSG)
                    .set_fields(fields_map);
            }
        }
    }

    pub fn resolve_lambda_type(&mut self, lambda_type_decl: &OkLambdaTypeDeclarationNode) {
        let core_lambda_type_decl = lambda_type_decl.core_ref();
        let mut params_vec: Vec<(Rc<String>, Type)> = vec![];
        let params = &core_lambda_type_decl.params;
        let return_type = &core_lambda_type_decl.return_type;
        let return_type: Type = match return_type {
            Some(return_type_expr) => {
                let type_obj = self.type_obj_from_expression(return_type_expr);
                type_obj
            }
            None => Type::new_with_void(),
        };
        if let Some(params) = params {
            let params_iter = params.iter();
            // let mut params_map: FxHashMap<Rc<String>, ()> = FxHashMap::default();
            let mut params_map: Set<Rc<String>> = Set::default();
            for param in params_iter {
                let core_param = param.core_ref();
                let name = &core_param.name;
                if let CoreIdentifierNode::OK(ok_identifier) = name.core_ref() {
                    let variable_name = Rc::new(ok_identifier.token_value(&self.code));
                    if let Some(_) = params_map.get(&variable_name) {
                        let err_message =
                            format!("argument with name `{}` already exist", variable_name);
                        self.log_error(
                            ok_identifier.range(),
                            ok_identifier.start_line_number(),
                            err_message,
                        );
                        continue;
                    }
                    let type_obj = self.type_obj_from_expression(&core_param.data_type);
                    params_map.insert(variable_name.clone(), ());
                    params_vec.push((variable_name, type_obj));
                }
            }
        }
        if let CoreIdentifierNode::OK(ok_identifier) = core_lambda_type_decl.name.core_ref() {
            if let Some(symbol_data) = ok_identifier.user_defined_type_symbol_data(
                "lambda type name should be resolved to `SymbolData<UserDefinedTypeData>`",
            ) {
                symbol_data
                    .0
                    .as_ref()
                    .borrow_mut()
                    .lambda_data_mut(LAMBDA_NAME_NOT_BINDED_WITH_LAMBDA_VARIANT_SYMBOL_DATA_MSG)
                    .set_params_and_return_type(params_vec, return_type)
            }
        }
    }

    pub fn log_error(
        &mut self,
        error_range: TextRange,
        start_line_number: usize,
        err_message: String,
    ) {
        let start_err_index: usize = error_range.start().into();
        let end_err_index: usize = error_range.end().into();
        let err = JarvilError::form_error(
            start_err_index,
            end_err_index,
            start_line_number,
            &self.code,
            err_message,
            JarvilErrorKind::SEMANTIC_ERROR,
        );
        self.errors.push(err);
    }
}
impl Visitor for Resolver {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match self.mode {
            ResolverMode::DECLARE => match node {
                ASTNode::VARIABLE_DECLARATION(variable_decl) => {
                    self.declare_variable(variable_decl);
                    return None;
                }
                ASTNode::OK_FUNCTION_DECLARATION(func_decl) => {
                    self.declare_function(func_decl);
                    return None;
                }
                ASTNode::STRUCT_DECLARATION(struct_decl) => {
                    self.declare_struct(struct_decl);
                    return None;
                }
                ASTNode::OK_LAMBDA_TYPE_DECLARATION(lambda_type_decl) => {
                    self.declare_lambda_type(lambda_type_decl);
                    return None;
                }
                ASTNode::ATOM_START(atom_start) => {
                    match atom_start.core_ref() {
                        CoreAtomStartNode::IDENTIFIER(identifier) => {
                            if let CoreIdentifierNode::OK(ok_identifier) = identifier.core_ref() {
                                if let Some(name) = self.try_resolving_variable(ok_identifier) {
                                    let err_message =
                                        format!("variable `{}` is not declared in the scope", name);
                                    self.log_error(
                                        ok_identifier.range(),
                                        ok_identifier.start_line_number(),
                                        err_message,
                                    )
                                }
                            }
                        }
                        CoreAtomStartNode::CALL(func_call) => {
                            let core_func_call = func_call.core_ref();
                            if let CoreIdentifierNode::OK(ok_identifier) =
                                core_func_call.function_name.core_ref()
                            {
                                let lambda_name = Rc::new(ok_identifier.token_value(&self.code));
                                if let Some(symbol_data) =
                                    self.namespace.lookup_in_variables_namespace(&lambda_name)
                                {
                                    ok_identifier.bind_variable_decl(&symbol_data.0, symbol_data.1);
                                }
                            }
                            if let Some(params) = &core_func_call.params {
                                self.walk_params(params)
                            }
                        }
                        _ => {}
                    }
                    return None;
                }
                _ => return Some(()),
            },
            ResolverMode::RESOLVE => match node {
                ASTNode::OK_FUNCTION_DECLARATION(func_decl) => {
                    self.resolve_function(func_decl);
                    return None;
                }
                ASTNode::STRUCT_DECLARATION(struct_decl) => {
                    self.resolve_struct(struct_decl);
                    return None;
                }
                ASTNode::OK_LAMBDA_TYPE_DECLARATION(lambda_type_decl) => {
                    self.resolve_lambda_type(lambda_type_decl);
                    return None;
                }
                ASTNode::ATOM_START(atom_start) => {
                    match atom_start.core_ref() {
                        CoreAtomStartNode::CALL(func_call) => {
                            let core_func_call = func_call.core_ref();
                            if let CoreIdentifierNode::OK(ok_identifier) =
                                core_func_call.function_name.core_ref()
                            {
                                if !ok_identifier.is_resolved() {
                                    if let Some(name) = self.try_resolving_function(ok_identifier) {
                                        let err_message = format!(
                                            "function `{}` is not declared in the scope",
                                            name
                                        );
                                        self.log_error(
                                            ok_identifier.range(),
                                            ok_identifier.start_line_number(),
                                            err_message,
                                        )
                                    }
                                    // TODO - check in type namespace for constructor
                                }
                            }
                            if let Some(params) = &core_func_call.params {
                                self.walk_params(params)
                            }
                        }
                        CoreAtomStartNode::CLASS_METHOD_CALL(class_method_call) => {
                            let core_class_method_call = class_method_call.core_ref();
                            if let CoreIdentifierNode::OK(ok_identifier) =
                                core_class_method_call.class_name.core_ref()
                            {
                                if let Some(name) =
                                    self.try_resolving_user_defined_type(ok_identifier)
                                {
                                    let err_message =
                                        format!("type `{}` is not declared in the scope", name);
                                    self.log_error(
                                        ok_identifier.range(),
                                        ok_identifier.start_line_number(),
                                        err_message,
                                    )
                                }
                            }
                            if let Some(params) = &core_class_method_call.params {
                                self.walk_params(params);
                            }
                        }
                        _ => {}
                    }
                    return None;
                }
                _ => return Some(()),
            },
        }
    }
}
