// This module contains the old implementation of `resolver.rs`. The name resolution done here diverges heavily from python
// which made the code-generation quite complex. This resolver is replaced with the new python-compatible resolver present in `resolver.rs`.

use crate::ast::ast::{
    CallableBodyNode, CallablePrototypeNode, CoreCallableBodyNode, FunctionDeclarationNode,
    LambdaDeclarationNode,
};
use crate::constants::common::EIGHT_BIT_MAX_VALUE;
use crate::error::diagnostics::MoreThanMaxLimitParamsPassedError;
use crate::error::helper::IdentifierKind as IdentKind;
use crate::{
    ast::{
        ast::{
            ASTNode, BlockNode, CallableKind, CoreAtomStartNode, CoreIdentifierNode,
            CoreStatemenIndentWrapperNode, CoreStatementNode, Node, OkIdentifierNode,
            OkLambdaTypeDeclarationNode, StructDeclarationNode, TypeExpressionNode,
            TypeResolveKind, VariableDeclarationNode,
        },
        walk::Visitor,
    },
    code::Code,
    error::{
        constants::{
            LAMBDA_NAME_NOT_BINDED_WITH_LAMBDA_VARIANT_SYMBOL_DATA_MSG, SCOPE_NOT_SET_TO_BLOCK_MSG,
            STRUCT_NAME_NOT_BINDED_WITH_STRUCT_VARIANT_SYMBOL_DATA_MSG,
        },
        diagnostics::{Diagnostics, IdentifierAlreadyDeclaredError, IdentifierNotDeclaredError},
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

pub enum ResolverMode {
    DECLARE, // first pass - visit the declaration nodes and fills up the scope with placeholder values
    RESOLVE, // second pass - resolve each identifier in appropiate namespaces
}

pub struct Resolver {
    namespace: Namespace,
    pub code: Code,
    errors: Vec<Diagnostics>,
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

    pub fn open_block(&mut self) {
        self.namespace.open_scope();
    }

    pub fn close_block(&mut self) {
        self.namespace.close_scope();
    }

    pub fn open_func(&mut self) {
        self.namespace.open_scope();
    }

    pub fn close_func(&mut self) {
        self.namespace.close_scope();
    }

    pub fn resolve_ast(mut self, ast: &BlockNode) -> (Namespace, Vec<Diagnostics>) {
        let code_block = ast.0.as_ref().borrow();
        for stmt in &code_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        self.mode = ResolverMode::RESOLVE;
        for stmt in &code_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        (self.namespace, self.errors)
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
            Some((symbol_data, depth)) => {
                bind_fn(identifier, &symbol_data, depth);
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

    pub fn try_declare_and_bind<
        T,
        U: Fn(&Namespace, &Rc<String>, TextRange) -> Result<SymbolData<T>, TextRange>,
        V: Fn(&OkIdentifierNode, &SymbolData<T>),
    >(
        &mut self,
        identifier: &OkIdentifierNode,
        declare_fn: U,
        bind_fn: V,
    ) -> Option<(Rc<String>, TextRange)> {
        let name = Rc::new(identifier.token_value(&self.code));
        let symbol_data = declare_fn(&self.namespace, &name, identifier.range());
        match symbol_data {
            Ok(symbol_data) => {
                bind_fn(identifier, &symbol_data);
                None
            }
            Err(previous_decl_range) => Some((name, previous_decl_range)),
        }
    }

    pub fn try_declare_and_bind_variable(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<(Rc<String>, TextRange)> {
        let declare_fn = |namespace: &Namespace, name: &Rc<String>, decl_range: TextRange| {
            namespace.declare_variable(name, decl_range)
        };
        let bind_fn = |identifier: &OkIdentifierNode, symbol_data: &SymbolData<VariableData>| {
            identifier.bind_variable_decl(symbol_data, 0)
        };
        let name = Rc::new(identifier.token_value(&self.code));
        let symbol_data = declare_fn(&self.namespace, &name, identifier.range());
        match symbol_data {
            Ok(symbol_data) => {
                bind_fn(identifier, &symbol_data);
                return None;
            }
            Err(previous_decl_range) => return Some((name, previous_decl_range)),
        }
    }

    pub fn try_declare_and_bind_function(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<(Rc<String>, TextRange)> {
        let declare_fn = |namespace: &Namespace, name: &Rc<String>, decl_range: TextRange| {
            namespace.declare_function(name, decl_range)
        };
        let bind_fn = |identifier: &OkIdentifierNode, symbol_data: &SymbolData<FunctionData>| {
            identifier.bind_function_decl(symbol_data, 0)
        };
        self.try_declare_and_bind(identifier, declare_fn, bind_fn)
    }

    pub fn try_declare_and_bind_struct_type(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<(Rc<String>, TextRange)> {
        let declare_fn = |namespace: &Namespace, name: &Rc<String>, decl_range: TextRange| {
            namespace.declare_struct_type(name, decl_range)
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
    ) -> Option<(Rc<String>, TextRange)> {
        let declare_fn = |namespace: &Namespace, name: &Rc<String>, decl_range: TextRange| {
            namespace.declare_lambda_type(name, decl_range)
        };
        let bind_fn = |identifier: &OkIdentifierNode,
                       symbol_data: &SymbolData<UserDefinedTypeData>| {
            identifier.bind_user_defined_type_decl(symbol_data, 0)
        };
        self.try_declare_and_bind(identifier, declare_fn, bind_fn)
    }

    pub fn declare_variable(&mut self, variable_decl: &VariableDeclarationNode) {
        let core_variable_decl = variable_decl.core_ref();
        self.walk_r_variable_declaration(&core_variable_decl.r_node);
        if let CoreIdentifierNode::OK(ok_identifier) = core_variable_decl.name.core_ref() {
            if let Some((name, previous_decl_range)) =
                self.try_declare_and_bind_variable(ok_identifier)
            {
                let err = IdentifierAlreadyDeclaredError::new(
                    IdentKind::VARIABLE,
                    name.to_string(),
                    previous_decl_range,
                    ok_identifier.range(),
                );
                self.errors
                    .push(Diagnostics::IdentifierAlreadyDeclared(err));
            }
        }
    }

    pub fn declare_callable_prototype(&mut self, callable_prototype: &CallablePrototypeNode) {
        let core_callable_prototype = callable_prototype.core_ref();
        let params = &core_callable_prototype.params;
        if let Some(params) = params {
            let params_iter = params.iter();
            for param in params_iter {
                let core_param = param.core_ref();
                let param_name = &core_param.name;
                if let CoreIdentifierNode::OK(ok_identifier) = param_name.core_ref() {
                    if let Some((name, previous_decl_range)) =
                        self.try_declare_and_bind_variable(ok_identifier)
                    {
                        let err = IdentifierAlreadyDeclaredError::new(
                            IdentKind::VARIABLE,
                            name.to_string(),
                            previous_decl_range,
                            ok_identifier.range(),
                        );
                        self.errors
                            .push(Diagnostics::IdentifierAlreadyDeclared(err));
                    }
                }
            }
        }
    }

    pub fn declare_callable_body(&mut self, callable_body: &CallableBodyNode) {
        let core_callable_body = callable_body.0.as_ref();
        match core_callable_body {
            CoreCallableBodyNode::OK(ok_callable_body) => {
                let core_ok_callable_body = ok_callable_body.core_ref();
                self.open_func();
                self.declare_callable_prototype(&core_ok_callable_body.prototype);
                let func_body = &core_ok_callable_body.block;
                for stmt in &func_body.0.as_ref().borrow().stmts {
                    self.walk_stmt_indent_wrapper(stmt);
                }
                func_body.set_scope(&self.namespace);
                self.close_func();
            }
            CoreCallableBodyNode::MISSING_TOKENS(_) => return,
        }
    }

    pub fn declare_function(&mut self, func_decl: &FunctionDeclarationNode) {
        let core_func_decl = func_decl.core_ref();
        let func_name = &core_func_decl.name;
        let kind = &core_func_decl.kind;
        let body = &core_func_decl.body;
        self.walk_callable_body(body);
        if let CoreIdentifierNode::OK(ok_identifier) = func_name.core_ref() {
            match kind {
                CallableKind::FUNC => {
                    if let Some((name, previous_decl_range)) =
                        self.try_declare_and_bind_function(ok_identifier)
                    {
                        let err = IdentifierAlreadyDeclaredError::new(
                            IdentKind::FUNCTION,
                            name.to_string(),
                            previous_decl_range,
                            ok_identifier.range(),
                        );
                        self.errors
                            .push(Diagnostics::IdentifierAlreadyDeclared(err));
                    }
                }
                CallableKind::METHOD => unimplemented!(),
                CallableKind::CLASSMETHOD => unimplemented!(),
                CallableKind::CONSTRUCTOR => unimplemented!(),
            }
        }
    }

    pub fn declare_struct(&mut self, struct_decl: &StructDeclarationNode) {
        let core_struct_decl = struct_decl.core_ref();
        if let CoreIdentifierNode::OK(ok_identifier) = core_struct_decl.name.core_ref() {
            if let Some((name, previous_decl_range)) =
                self.try_declare_and_bind_struct_type(ok_identifier)
            {
                let err = IdentifierAlreadyDeclaredError::new(
                    IdentKind::TYPE,
                    name.to_string(),
                    previous_decl_range,
                    ok_identifier.range(),
                );
                self.errors
                    .push(Diagnostics::IdentifierAlreadyDeclared(err));
            }
        }
    }

    pub fn declare_lambda_type(&mut self, lambda_type_decl: &OkLambdaTypeDeclarationNode) {
        let core_lambda_type_decl = lambda_type_decl.core_ref();
        if let CoreIdentifierNode::OK(ok_identifier) = core_lambda_type_decl.name.core_ref() {
            if let Some((name, previous_decl_range)) =
                self.try_declare_and_bind_lambda_type(ok_identifier)
            {
                let err = IdentifierAlreadyDeclaredError::new(
                    IdentKind::TYPE,
                    name.to_string(),
                    previous_decl_range,
                    ok_identifier.range(),
                );
                self.errors
                    .push(Diagnostics::IdentifierAlreadyDeclared(err));
            }
        }
    }

    pub fn type_obj_from_expression(&mut self, type_expr: &TypeExpressionNode) -> Type {
        match type_expr.type_obj_before_resolved(&self.namespace, &self.code) {
            TypeResolveKind::RESOLVED(type_obj) => type_obj,
            TypeResolveKind::UNRESOLVED(identifier) => {
                let err = IdentifierNotDeclaredError::new(IdentKind::TYPE, identifier.range());
                self.errors.push(Diagnostics::IdentifierNotDeclared(err));
                return Type::new_with_unknown();
            }
            TypeResolveKind::INVALID => Type::new_with_unknown(),
        }
    }

    pub fn resolve_callable_prototype(
        &mut self,
        callable_prototype: &CallablePrototypeNode,
    ) -> (Vec<(Rc<String>, Type)>, Vec<Type>, Type) {
        let core_callable_prototype = callable_prototype.core_ref();
        let params = &core_callable_prototype.params;
        let return_type = &core_callable_prototype.return_type;
        let rparen = &core_callable_prototype.rparen;
        let mut params_vec: Vec<(Rc<String>, Type)> = vec![];
        let mut param_types_vec: Vec<Type> = vec![];
        let return_type: Type = match return_type {
            Some(return_type_expr) => {
                let type_obj = self.type_obj_from_expression(return_type_expr);
                type_obj
            }
            None => Type::new_with_void(),
        };
        let mut params_count: usize = 0;
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
                        params_vec.push((variable_name, type_obj.clone()));
                        param_types_vec.push(type_obj);
                        params_count += 1;
                    }
                }
            }
        }
        if params_count > EIGHT_BIT_MAX_VALUE {
            let err = MoreThanMaxLimitParamsPassedError::new(
                params_count,
                EIGHT_BIT_MAX_VALUE,
                rparen.range(),
            );
            self.errors
                .push(Diagnostics::MoreThanMaxLimitParamsPassed(err));
        }
        (params_vec, param_types_vec, return_type)
    }

    pub fn resolve_callable_body(
        &mut self,
        callable_body: &CallableBodyNode,
    ) -> (Vec<(Rc<String>, Type)>, Vec<Type>, Type) {
        let core_callable_body = callable_body.core_ref();
        match core_callable_body {
            CoreCallableBodyNode::OK(ok_callable_body) => {
                let core_ok_callable_body = ok_callable_body.core_ref();
                let func_body = &core_ok_callable_body.block;
                let (params_vec, param_types_vec, return_type) =
                    self.resolve_callable_prototype(&core_ok_callable_body.prototype);
                self.walk_block(func_body);
                (params_vec, param_types_vec, return_type)
            }
            CoreCallableBodyNode::MISSING_TOKENS(_) => {
                return (Vec::new(), Vec::new(), Type::new_with_unknown())
            }
        }
    }

    pub fn resolve_lambda(&mut self, lambda_decl: &LambdaDeclarationNode) {
        let core_lambda_decl = lambda_decl.core_ref();
        let lambda_variable_name = &core_lambda_decl.name;
        let (_, param_types_vec, return_type) = self.resolve_callable_body(&core_lambda_decl.body);
        if let CoreIdentifierNode::OK(ok_identifier) = lambda_variable_name.core_ref() {
            if let Some(variable_symbol_data) = ok_identifier.variable_symbol_data(
                "variable name should be resolved to `SymbolData<VariableData>`",
            ) {
                let symbol_data =
                    UserDefinedTypeData::LAMBDA(LambdaTypeData::new(param_types_vec, return_type));
                let lambda_type_obj = Type::new_with_lambda(
                    None,
                    &SymbolData::new(symbol_data, ok_identifier.range()),
                );
                variable_symbol_data
                    .0
                    .as_ref()
                    .borrow_mut()
                    .set_data_type(&lambda_type_obj);
            }
        }
    }

    pub fn resolve_function(&mut self, func_decl: &FunctionDeclarationNode) {
        let core_func_decl = func_decl.core_ref();
        let func_name = &core_func_decl.name;
        let (params_vec, _, return_type) = self.resolve_callable_body(&core_func_decl.body);
        let kind = &core_func_decl.kind;
        if let CoreIdentifierNode::OK(ok_identifier) = func_name.core_ref() {
            if let Some(symbol_data) = ok_identifier.symbol_data() {
                match symbol_data.0 {
                    IdentifierKind::FUNCTION(func_symbol_data) => {
                        assert!(kind.clone() == CallableKind::FUNC);
                        func_symbol_data
                            .0
                            .as_ref()
                            .borrow_mut()
                            .set_data(params_vec, return_type);
                    }
                    _ => unreachable!(
                        "function name should be resolved to `SymbolData<FunctionData>`"
                    ),
                }
            }
        }
    }

    pub fn resolve_struct(&mut self, struct_decl: &StructDeclarationNode) {
        let core_struct_decl = struct_decl.core_ref();
        let mut fields_map: FxHashMap<String, (Type, TextRange)> = FxHashMap::default();
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
                            Some((_, previous_decl_range)) => {
                                let err = IdentifierAlreadyDeclaredError::new(
                                    IdentKind::FIELD,
                                    field_name,
                                    *previous_decl_range,
                                    ok_identifier.range()
                                );
                                self.errors.push(Diagnostics::IdentifierAlreadyDeclared(err));
                            },
                            None => {
                                fields_map.insert(field_name, (type_obj, ok_identifier.range()));
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
        let mut types_vec: Vec<Type> = vec![];
        let type_tuple = &core_lambda_type_decl.type_tuple;
        let return_type = &core_lambda_type_decl.return_type;
        let rparen = &core_lambda_type_decl.rparen;
        let return_type: Type = match return_type {
            Some(return_type_expr) => {
                let type_obj = self.type_obj_from_expression(return_type_expr);
                type_obj
            }
            None => Type::new_with_void(),
        };
        let mut types_count = 0;
        if let Some(type_tuple) = type_tuple {
            let type_tuple_iter = type_tuple.iter();
            for data_type in type_tuple_iter {
                types_vec.push(self.type_obj_from_expression(&data_type));
                types_count += 1;
            }
        }
        if types_count > EIGHT_BIT_MAX_VALUE {
            let err = MoreThanMaxLimitParamsPassedError::new(
                types_count,
                EIGHT_BIT_MAX_VALUE,
                rparen.range(),
            );
            self.errors
                .push(Diagnostics::MoreThanMaxLimitParamsPassed(err));
        }
        if let CoreIdentifierNode::OK(ok_identifier) = lambda_type_decl.core_ref().name.core_ref() {
            if let Some(symbol_data) = ok_identifier.user_defined_type_symbol_data(
                "lambda type name should be resolved to `SymbolData<UserDefinedTypeData>`",
            ) {
                symbol_data
                    .0
                    .as_ref()
                    .borrow_mut()
                    .lambda_data_mut(LAMBDA_NAME_NOT_BINDED_WITH_LAMBDA_VARIANT_SYMBOL_DATA_MSG)
                    .set_params_and_return_type(types_vec, return_type)
            }
        }
    }
}

impl Visitor for Resolver {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match self.mode {
            ResolverMode::DECLARE => match node {
                ASTNode::BLOCK(block) => {
                    self.open_block();
                    let core_block = block.0.as_ref().borrow();
                    for stmt in &core_block.stmts {
                        self.walk_stmt_indent_wrapper(stmt);
                    }
                    self.close_block();
                    return None;
                }
                ASTNode::VARIABLE_DECLARATION(variable_decl) => {
                    self.declare_variable(variable_decl);
                    return None;
                }
                ASTNode::FUNCTION_DECLARATION(func_decl) => {
                    self.declare_function(func_decl);
                    return None;
                }
                ASTNode::CALLABLE_BODY(callable_body) => {
                    self.declare_callable_body(callable_body);
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
                                if let Some(_) = self.try_resolving_variable(ok_identifier) {
                                    let err = IdentifierNotDeclaredError::new(
                                        IdentKind::VARIABLE,
                                        ok_identifier.range(),
                                    );
                                    self.errors.push(Diagnostics::IdentifierNotDeclared(err));
                                }
                            }
                        }
                        CoreAtomStartNode::CALL(func_call) => {
                            // Also lookup in Type namespace for struct type constructor call
                            let core_func_call = func_call.core_ref();
                            if let CoreIdentifierNode::OK(ok_identifier) =
                                core_func_call.function_name.core_ref()
                            {
                                let lambda_name = Rc::new(ok_identifier.token_value(&self.code));
                                if let Some((symbol_data, depth)) =
                                    self.namespace.lookup_in_variables_namespace(&lambda_name)
                                {
                                    ok_identifier.bind_variable_decl(&symbol_data, depth);
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
            // add nodes for variable decl and assignment to first traverse the right hand side so that
            // errors like let x = x + 1 is catched
            ResolverMode::RESOLVE => match node {
                ASTNode::BLOCK(block) => {
                    self.namespace = block.scope().expect(SCOPE_NOT_SET_TO_BLOCK_MSG);
                    let core_block = block.0.as_ref().borrow();
                    for stmt in &core_block.stmts {
                        self.walk_stmt_indent_wrapper(stmt);
                    }
                    self.namespace.close_scope();
                    return None;
                }
                ASTNode::FUNCTION_DECLARATION(func_decl) => {
                    self.resolve_function(func_decl);
                    return None;
                }
                ASTNode::LAMBDA_DECLARATION(lambda_decl) => {
                    self.resolve_lambda(lambda_decl);
                    return None;
                }
                ASTNode::CALLABLE_BODY(callable_body) => {
                    self.resolve_callable_body(callable_body);
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
                                    if let Some(_) = self.try_resolving_function(ok_identifier) {
                                        let err = IdentifierNotDeclaredError::new(
                                            IdentKind::FUNCTION,
                                            ok_identifier.range(),
                                        );
                                        self.errors.push(Diagnostics::IdentifierNotDeclared(err));
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
                                if let Some(_) = self.try_resolving_user_defined_type(ok_identifier)
                                {
                                    let err = IdentifierNotDeclaredError::new(
                                        IdentKind::TYPE,
                                        ok_identifier.range(),
                                    );
                                    self.errors.push(Diagnostics::IdentifierNotDeclared(err));
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