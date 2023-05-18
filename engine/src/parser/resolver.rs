use crate::ast::ast::{
    CallableBodyNode, CallablePrototypeNode, CoreCallableBodyNode, CoreRVariableDeclarationNode,
    CoreSelfKeywordNode, CoreTypeExpressionNode, FunctionWrapperNode, OkSelfKeywordNode,
};
use crate::constants::common::EIGHT_BIT_MAX_VALUE;
use crate::error::diagnostics::{
    ConstructorNotFoundInsideStructDeclarationError, IdentifierFoundInNonLocalsError,
    IdentifierNotFoundInAnyNamespaceError, MoreThanMaxLimitParamsPassedError,
    NonHashableTypeInIndexError, NonVoidConstructorReturnTypeError, SelfNotFoundError,
    VariableReferencedBeforeAssignmentError,
};
use crate::error::helper::IdentifierKind as IdentKind;
use crate::scope::core::VariableLookupResult;
use crate::types::core::CoreType;
use crate::{
    ast::{
        ast::{
            ASTNode, BlockNode, CoreAtomStartNode, CoreIdentifierNode,
            CoreStatemenIndentWrapperNode, CoreStatementNode, Node, OkIdentifierNode,
            OkLambdaTypeDeclarationNode, StructDeclarationNode, TypeExpressionNode,
            TypeResolveKind, VariableDeclarationNode,
        },
        walk::Visitor,
    },
    code::Code,
    error::{
        constants::{
            LAMBDA_NAME_NOT_BINDED_WITH_LAMBDA_VARIANT_SYMBOL_DATA_MSG,
            STRUCT_NAME_NOT_BINDED_WITH_STRUCT_VARIANT_SYMBOL_DATA_MSG,
        },
        diagnostics::{Diagnostics, IdentifierAlreadyDeclaredError, IdentifierNotDeclaredError},
    },
    scope::{
        core::{Namespace, SymbolData},
        function::FunctionData,
        user_defined_types::{LambdaTypeData, UserDefinedTypeData},
        variables::VariableData,
    },
    types::core::Type,
};
use rustc_hash::FxHashMap;
use std::{rc::Rc, vec};
use text_size::TextRange;

pub enum ResolveResult {
    OK(usize),       // depth
    Err(Rc<String>), // name of the identifier
}

pub struct ClassContext {
    is_containing_self: Vec<bool>,
}

pub struct Context {
    class_context: ClassContext,
}

pub struct Resolver {
    namespace: Namespace,
    pub code: Code,
    errors: Vec<Diagnostics>,
    context: Context,
}

impl Resolver {
    pub fn new(code: &Code) -> Self {
        Resolver {
            namespace: Namespace::new(),
            code: code.clone(),
            errors: vec![],
            context: Context {
                class_context: ClassContext {
                    is_containing_self: vec![],
                },
            },
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

    pub fn set_curr_class_context_is_containing_self(&mut self, value: bool) {
        let len = self.context.class_context.is_containing_self.len();
        self.context.class_context.is_containing_self[len - 1] = value;
    }

    pub fn get_curr_class_context_is_containing_self(&self) -> bool {
        let len = self.context.class_context.is_containing_self.len();
        self.context.class_context.is_containing_self[len - 1]
    }

    pub fn resolve_ast(mut self, ast: &BlockNode) -> (Namespace, Vec<Diagnostics>) {
        let code_block = ast.0.as_ref().borrow();
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
    ) -> ResolveResult {
        let name = Rc::new(identifier.token_value(&self.code));
        match lookup_fn(&self.namespace, &name) {
            Some((symbol_data, depth)) => {
                bind_fn(identifier, &symbol_data, depth);
                ResolveResult::OK(depth)
            }
            None => ResolveResult::Err(name),
        }
    }

    pub fn try_resolving_variable(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> VariableLookupResult {
        let name = Rc::new(identifier.token_value(&self.code));
        match self
            .namespace
            .lookup_in_variables_namespace_with_is_init(&name)
        {
            VariableLookupResult::OK((symbol_data, depth)) => {
                identifier.bind_variable_decl(&symbol_data, depth);
                return VariableLookupResult::OK((symbol_data, depth));
            }
            VariableLookupResult::NOT_INITIALIZED(decl_range) => {
                return VariableLookupResult::NOT_INITIALIZED(decl_range)
            }
            VariableLookupResult::Err => return VariableLookupResult::Err,
        }
    }

    pub fn try_resolving_self_keyword(
        &mut self,
        self_keyword: &OkSelfKeywordNode,
    ) -> Option<(SymbolData<VariableData>, usize)> {
        let name = Rc::new(self_keyword.token_value(&self.code));
        assert!(name == Rc::new("self".to_string()));
        match self.namespace.lookup_in_variables_namespace(&name) {
            Some((symbol_data, depth)) => {
                self_keyword.bind_decl(&symbol_data, depth);
                return Some((symbol_data, depth));
            }
            None => return None,
        }
    }

    pub fn try_resolving_function(&mut self, identifier: &OkIdentifierNode) -> ResolveResult {
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
    ) -> ResolveResult {
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

    pub fn check_if_type_is_hashmap_with_hashable_index<T: Fn(&mut Resolver, TextRange)>(
        type_obj: &Type,
        type_expr: &TypeExpressionNode,
        is_log_error: Option<(T, &mut Resolver)>,
    ) -> Type {
        match type_obj.0.as_ref() {
            CoreType::HASHMAP(hashmap) => {
                let index_span = match type_expr.core_ref() {
                    CoreTypeExpressionNode::HASHMAP(hashmap_type_expr) => {
                        hashmap_type_expr.core_ref().key_type.range()
                    }
                    _ => unreachable!(),
                };
                if hashmap.key_type.is_hashable() {
                    return type_obj.clone();
                } else {
                    if let Some((log_error_fn, resolver)) = is_log_error {
                        log_error_fn(resolver, index_span)
                    }
                    return Type::new_with_unknown();
                }
            }
            _ => return type_obj.clone(),
        }
    }

    pub fn type_obj_from_expression(&mut self, type_expr: &TypeExpressionNode) -> Type {
        match type_expr.type_obj_before_resolved(&self.namespace, &self.code) {
            TypeResolveKind::RESOLVED(type_obj) => {
                let log_error_fn = |resolver: &mut Resolver, index_span: TextRange| {
                    let err = NonHashableTypeInIndexError::new(index_span);
                    resolver
                        .errors
                        .push(Diagnostics::NonHashableTypeInIndex(err));
                };
                return Self::check_if_type_is_hashmap_with_hashable_index(
                    &type_obj,
                    type_expr,
                    Some((log_error_fn, self)),
                );
            }
            TypeResolveKind::UNRESOLVED(identifier) => {
                let err = IdentifierNotDeclaredError::new(IdentKind::TYPE, identifier.range());
                self.errors.push(Diagnostics::IdentifierNotDeclared(err));
                return Type::new_with_unknown();
            }
            TypeResolveKind::INVALID => Type::new_with_unknown(),
        }
    }

    pub fn declare_callable_prototype(
        &mut self,
        callable_prototype: &CallablePrototypeNode,
    ) -> (Vec<(Rc<String>, Type)>, Vec<Type>, Type, Option<TextRange>) {
        let core_callable_prototype = callable_prototype.core_ref();
        let params = &core_callable_prototype.params;
        let return_type = &core_callable_prototype.return_type;
        let rparen = &core_callable_prototype.rparen;
        let mut params_vec: Vec<(Rc<String>, Type)> = vec![];
        let mut param_types_vec: Vec<Type> = vec![];
        let mut return_type_range: Option<TextRange> = None;
        let return_type: Type = match return_type {
            Some(return_type_expr) => {
                return_type_range = Some(return_type_expr.range());
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
                let param_name = &core_param.name;
                if let CoreIdentifierNode::OK(ok_identifier) = param_name.core_ref() {
                    let param_name = Rc::new(ok_identifier.token_value(&self.code));
                    let param_type = self.type_obj_from_expression(&core_param.data_type);
                    let symbol_data = self.namespace.declare_variable_with_type(
                        &param_name,
                        &param_type,
                        ok_identifier.range(),
                        true,
                    );
                    match symbol_data {
                        Ok(symbol_data) => {
                            ok_identifier.bind_variable_decl(&symbol_data, 0);
                            params_vec.push((param_name, param_type.clone()));
                            param_types_vec.push(param_type);
                            params_count += 1;
                        }
                        Err(previous_decl_range) => {
                            let err = IdentifierAlreadyDeclaredError::new(
                                IdentKind::VARIABLE,
                                param_name.to_string(),
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
        if params_count > EIGHT_BIT_MAX_VALUE {
            let err = MoreThanMaxLimitParamsPassedError::new(
                params_count,
                EIGHT_BIT_MAX_VALUE,
                rparen.range(),
            );
            self.errors
                .push(Diagnostics::MoreThanMaxLimitParamsPassed(err));
        }
        (params_vec, param_types_vec, return_type, return_type_range)
    }

    pub fn visit_callable_body(
        &mut self,
        callable_body_decl: &CallableBodyNode,
    ) -> (Vec<(Rc<String>, Type)>, Vec<Type>, Type, Option<TextRange>) {
        let core_callable_body_decl = callable_body_decl.core_ref();
        match core_callable_body_decl {
            CoreCallableBodyNode::OK(ok_callable_body) => {
                let core_ok_callable_body = ok_callable_body.core_ref();
                self.open_func();
                let (params_vec, param_types_vec, return_type, return_type_range) =
                    self.declare_callable_prototype(&core_ok_callable_body.prototype);
                let callable_body = &core_ok_callable_body.block;
                for stmt in &callable_body.0.as_ref().borrow().stmts {
                    self.walk_stmt_indent_wrapper(stmt);
                }
                callable_body.set_scope(&self.namespace);
                self.close_func();
                (params_vec, param_types_vec, return_type, return_type_range)
            }
            CoreCallableBodyNode::MISSING_TOKENS(_) => {
                return (Vec::new(), Vec::new(), Type::new_with_unknown(), None)
            }
        }
    }

    pub fn declare_variable(&mut self, variable_decl: &VariableDeclarationNode) {
        let core_variable_decl = variable_decl.core_ref();
        if let CoreIdentifierNode::OK(ok_identifier) = core_variable_decl.name.core_ref() {
            let name = Rc::new(ok_identifier.token_value(&self.code));
            if self.namespace.is_variable_in_non_locals(&name) {
                let err = IdentifierFoundInNonLocalsError::new(
                    IdentKind::VARIABLE,
                    ok_identifier.range(),
                );
                self.errors
                    .push(Diagnostics::IdentifierFoundInNonLocals(err));
            } else {
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
        // Except `CoreRAssignmentNode::LAMBDA`, type of the variable is set in the `type_checker.rs`. For `CoreRAssignmentNode::LAMBDA`,
        // it's type is set to the variable symbol_data here itself.
        match core_variable_decl.r_node.core_ref() {
            CoreRVariableDeclarationNode::LAMBDA(lambda_r_assign) => {
                // For case of lambda variable, it is allowed to be referenced inside the body to
                // enable recursive definitions
                if let CoreIdentifierNode::OK(ok_identifier) = core_variable_decl.name.core_ref() {
                    if let Some(symbol_data) = ok_identifier.variable_symbol_data(
                        "variable name should be resolved to `SymbolData<VariableData>`",
                    ) {
                        symbol_data.0.as_ref().borrow_mut().set_is_init(true);
                    }
                };
                let core_lambda_r_assign = &lambda_r_assign.core_ref();
                let (_, params_vec, return_type, _) =
                    self.visit_callable_body(&core_lambda_r_assign.body);
                let lambda_type_obj = match core_lambda_r_assign.body.core_ref() {
                    CoreCallableBodyNode::OK(ok_callable_body) => {
                        let symbol_data = UserDefinedTypeData::LAMBDA(LambdaTypeData::new(
                            params_vec,
                            return_type,
                        ));
                        Type::new_with_lambda(
                            None,
                            &SymbolData::new(
                                symbol_data,
                                ok_callable_body
                                    .core_ref()
                                    .prototype
                                    .core_ref()
                                    .lparen
                                    .range(),
                            ),
                        )
                    }
                    CoreCallableBodyNode::MISSING_TOKENS(_) => Type::new_with_unknown(),
                };
                if let CoreIdentifierNode::OK(ok_identifier) = core_variable_decl.name.core_ref() {
                    if let Some(symbol_data) = ok_identifier.variable_symbol_data(
                        "variable name should be resolved to `SymbolData<VariableData>`",
                    ) {
                        symbol_data
                            .0
                            .as_ref()
                            .borrow_mut()
                            .set_data_type(&lambda_type_obj);
                    }
                };
            }
            CoreRVariableDeclarationNode::EXPRESSION(expr_r_assign) => {
                self.walk_expr_stmt(expr_r_assign);
            }
            CoreRVariableDeclarationNode::MISSING_TOKENS(missing_token) => {
                self.walk_missing_tokens(missing_token);
            }
        }
        if let CoreIdentifierNode::OK(ok_identifier) = core_variable_decl.name.core_ref() {
            if let Some(symbol_data) = ok_identifier.variable_symbol_data(
                "variable name should be resolved to `SymbolData<VariableData>`",
            ) {
                symbol_data.0.as_ref().borrow_mut().set_is_init(true);
            }
        };
    }

    pub fn declare_function(&mut self, func_wrapper: &FunctionWrapperNode) {
        let core_func_decl = func_wrapper.core_ref().func_decl.core_ref();
        let func_name = &core_func_decl.name;
        let body = &core_func_decl.body;
        if let CoreIdentifierNode::OK(ok_identifier) = func_name.core_ref() {
            let name = Rc::new(ok_identifier.token_value(&self.code));
            if self.namespace.is_function_in_non_locals(&name) {
                let err = IdentifierFoundInNonLocalsError::new(
                    IdentKind::FUNCTION,
                    ok_identifier.range(),
                );
                self.errors
                    .push(Diagnostics::IdentifierFoundInNonLocals(err));
            } else {
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
        }
        let (params_vec, _, return_type, _) = self.visit_callable_body(body);
        if let CoreIdentifierNode::OK(ok_identifier) = func_name.core_ref() {
            if let Some(symbol_data) = ok_identifier.function_symbol_data(
                "function name should be resolved to `SymbolData<FunctionData>`",
            ) {
                symbol_data
                    .0
                    .as_ref()
                    .borrow_mut()
                    .set_data(params_vec, return_type);
            }
        }
    }

    pub fn declare_struct_type(&mut self, struct_decl: &StructDeclarationNode) {
        self.context.class_context.is_containing_self.push(false);
        let core_struct_decl = struct_decl.core_ref();
        let struct_type_obj = match core_struct_decl.name.core_ref() {
            CoreIdentifierNode::OK(ok_identifier) => {
                let temp_struct_type_obj =
                    match self.try_declare_and_bind_struct_type(ok_identifier) {
                        Some((name, previous_decl_range)) => {
                            let err = IdentifierAlreadyDeclaredError::new(
                                IdentKind::TYPE,
                                name.to_string(),
                                previous_decl_range,
                                ok_identifier.range(),
                            );
                            self.errors
                                .push(Diagnostics::IdentifierAlreadyDeclared(err));
                            Type::new_with_unknown()
                        }
                        None => {
                            match ok_identifier.user_defined_type_symbol_data(
                            "struct name should be resolved to `SymbolData<UserDefinedTypeData>`"
                        ) {
                            Some(symbol_data) => {
                                let name = Rc::new(ok_identifier.token_value(&self.code));
                                Type::new_with_struct(name.to_string(), &symbol_data)
                            }
                            None => unreachable!()
                        }
                        }
                    };
                temp_struct_type_obj
            }
            _ => Type::new_with_unknown(),
        };
        self.open_block();
        let result = self.namespace.declare_variable_with_type(
            &Rc::new(String::from("self")),
            &struct_type_obj,
            core_struct_decl.name.range(),
            true,
        );
        assert!(result.is_ok());

        let mut fields_map: FxHashMap<Rc<String>, (Type, TextRange)> = FxHashMap::default();
        let mut constructor: Option<(FunctionData, TextRange)> = None;
        let mut methods: FxHashMap<Rc<String>, (FunctionData, TextRange)> = FxHashMap::default();
        let mut class_methods: FxHashMap<Rc<String>, (FunctionData, TextRange)> =
            FxHashMap::default();

        fn is_already_a_method(
            methods: &FxHashMap<Rc<String>, (FunctionData, TextRange)>,
            class_methods: &FxHashMap<Rc<String>, (FunctionData, TextRange)>,
            name: &String,
        ) -> Option<TextRange> {
            match methods.get(name) {
                Some((_, previous_decl_range)) => return Some(previous_decl_range.clone()),
                None => match class_methods.get(name) {
                    Some((_, previous_decl_range)) => return Some(previous_decl_range.clone()),
                    None => return None,
                },
            }
        }

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
                CoreStatementNode::STRUCT_PROPERTY_DECLARATION(struct_property_decl) => {
                    let core_struct_stmt = struct_property_decl.core_ref();
                    let name = &core_struct_stmt.name_type_spec.core_ref().name;
                    if let CoreIdentifierNode::OK(ok_identifier) = name.core_ref() {
                        let field_name = ok_identifier.token_value(&self.code);
                        let type_obj = self.type_obj_from_expression(
                            &core_struct_stmt.name_type_spec.core_ref().data_type,
                        );
                        match fields_map.get(&field_name) {
                            Some((_, previous_decl_range)) => {
                                let err = IdentifierAlreadyDeclaredError::new(
                                    IdentKind::FIELD,
                                    field_name,
                                    *previous_decl_range,
                                    ok_identifier.range(),
                                );
                                self.errors
                                    .push(Diagnostics::IdentifierAlreadyDeclared(err));
                            }
                            None => {
                                fields_map
                                    .insert(Rc::new(field_name), (type_obj, ok_identifier.range()));
                            }
                        }
                    }
                }
                CoreStatementNode::BOUNDED_METHOD_WRAPPER(bounded_method_wrappewr) => {
                    self.set_curr_class_context_is_containing_self(false);
                    let core_func_decl = bounded_method_wrappewr.core_ref().func_decl.core_ref();
                    let (params_vec, _, return_type, return_type_range) =
                        self.visit_callable_body(&core_func_decl.body);
                    if let CoreIdentifierNode::OK(ok_bounded_method_name) =
                        core_func_decl.name.core_ref()
                    {
                        let func_meta_data = FunctionData::new(params_vec, return_type);
                        let method_name_str = ok_bounded_method_name.token_value(&self.code);
                        if method_name_str.eq("__init__") {
                            match constructor {
                                Some((_, previous_decl_range)) => {
                                    let err = IdentifierAlreadyDeclaredError::new(
                                        IdentKind::CONSTRUCTOR,
                                        method_name_str,
                                        previous_decl_range,
                                        ok_bounded_method_name.range(),
                                    );
                                    self.errors
                                        .push(Diagnostics::IdentifierAlreadyDeclared(err));
                                }
                                None => {
                                    if let Some(return_type_range) = return_type_range {
                                        assert!(!func_meta_data.return_type.is_void());
                                        let err = NonVoidConstructorReturnTypeError::new(
                                            return_type_range,
                                        );
                                        self.errors
                                            .push(Diagnostics::NonVoidConstructorReturnType(err))
                                    }
                                    constructor =
                                        Some((func_meta_data, ok_bounded_method_name.range()));
                                }
                            }
                        } else {
                            match is_already_a_method(&methods, &class_methods, &method_name_str) {
                                Some(previous_decl_range) => {
                                    let err = IdentifierAlreadyDeclaredError::new(
                                        IdentKind::METHOD,
                                        method_name_str,
                                        previous_decl_range,
                                        ok_bounded_method_name.range(),
                                    );
                                    self.errors
                                        .push(Diagnostics::IdentifierAlreadyDeclared(err));
                                }
                                None => {
                                    let is_containing_self =
                                        self.get_curr_class_context_is_containing_self();
                                    if is_containing_self {
                                        methods.insert(
                                            Rc::new(method_name_str),
                                            (func_meta_data, ok_bounded_method_name.range()),
                                        );
                                    } else {
                                        class_methods.insert(
                                            Rc::new(method_name_str),
                                            (func_meta_data, ok_bounded_method_name.range()),
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
        self.close_block();
        if let CoreIdentifierNode::OK(ok_identifier) = core_struct_decl.name.core_ref() {
            if constructor.is_none() {
                let err =
                    ConstructorNotFoundInsideStructDeclarationError::new(ok_identifier.range());
                self.errors
                    .push(Diagnostics::ConstructorNotFoundInsideStructDeclaration(err));
            }
            if let Some(symbol_data) = ok_identifier.user_defined_type_symbol_data(
                "struct name should be resolved to `SymbolData<UserDefinedTypeData>`",
            ) {
                symbol_data
                    .0
                    .as_ref()
                    .borrow_mut()
                    .struct_data_mut(STRUCT_NAME_NOT_BINDED_WITH_STRUCT_VARIANT_SYMBOL_DATA_MSG)
                    .set_meta_data(fields_map, constructor, methods, class_methods);
            }
        }
        self.context.class_context.is_containing_self.pop();
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
        if let CoreIdentifierNode::OK(ok_identifier) = core_lambda_type_decl.name.core_ref() {
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
        match node {
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
            ASTNode::FUNCTION_WRAPPER(func_wrapper) => {
                self.declare_function(func_wrapper);
                return None;
            }
            ASTNode::STRUCT_DECLARATION(struct_decl) => {
                self.declare_struct_type(struct_decl);
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
                            let name = Rc::new(ok_identifier.token_value(&self.code));
                            match self.try_resolving_variable(ok_identifier) {
                                VariableLookupResult::OK((_, depth)) => {
                                    if depth > 0 {
                                        self.namespace.set_to_variable_non_locals(&name);
                                    }
                                }
                                VariableLookupResult::NOT_INITIALIZED(decl_range) => {
                                    let err = VariableReferencedBeforeAssignmentError::new(
                                        name.to_string(),
                                        decl_range,
                                        ok_identifier.range(),
                                    );
                                    self.errors
                                        .push(Diagnostics::VariableReferencedBeforeAssignment(err));
                                }
                                VariableLookupResult::Err => {
                                    let err = IdentifierNotDeclaredError::new(
                                        IdentKind::VARIABLE,
                                        ok_identifier.range(),
                                    );
                                    self.errors.push(Diagnostics::IdentifierNotDeclared(err));
                                }
                            }
                        }
                    }
                    CoreAtomStartNode::SELF_KEYWORD(self_keyword) => {
                        if let CoreSelfKeywordNode::OK(ok_self_keyword) = self_keyword.core_ref() {
                            match self.try_resolving_self_keyword(ok_self_keyword) {
                                Some(_) => {
                                    self.set_curr_class_context_is_containing_self(true);
                                }
                                None => {
                                    let err = SelfNotFoundError::new(ok_self_keyword.range());
                                    self.errors.push(Diagnostics::SelfNotFound(err));
                                }
                            }
                        }
                    }
                    CoreAtomStartNode::CALL(func_call) => {
                        let core_func_call = func_call.core_ref();
                        if let CoreIdentifierNode::OK(ok_identifier) =
                            core_func_call.function_name.core_ref()
                        {
                            // order of namespace search: function => type => variable
                            let name = Rc::new(ok_identifier.token_value(&self.code));
                            match self.namespace.lookup_in_functions_namespace(&name) {
                                Some((symbol_data, depth)) => {
                                    ok_identifier.bind_function_decl(&symbol_data, depth);
                                    if depth > 0 {
                                        self.namespace.set_to_function_non_locals(&name);
                                    }
                                }
                                None => match self.namespace.lookup_in_types_namespace(&name) {
                                    Some((symbol_data, depth)) => {
                                        ok_identifier
                                            .bind_user_defined_type_decl(&symbol_data, depth);
                                    }
                                    None => match self.try_resolving_variable(ok_identifier) {
                                        VariableLookupResult::OK((_, depth)) => {
                                            if depth > 0 {
                                                self.namespace.set_to_variable_non_locals(&name);
                                            }
                                        }
                                        VariableLookupResult::NOT_INITIALIZED(decl_range) => {
                                            let err = VariableReferencedBeforeAssignmentError::new(
                                                name.to_string(),
                                                decl_range,
                                                ok_identifier.range(),
                                            );
                                            self.errors.push(
                                                Diagnostics::VariableReferencedBeforeAssignment(
                                                    err,
                                                ),
                                            );
                                        }
                                        VariableLookupResult::Err => {
                                            let err = IdentifierNotFoundInAnyNamespaceError::new(
                                                ok_identifier.range(),
                                            );
                                            self.errors.push(
                                                Diagnostics::IdentifierNotFoundInAnyNamespace(err),
                                            );
                                        }
                                    },
                                },
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
                            if let ResolveResult::Err(_) =
                                self.try_resolving_user_defined_type(ok_identifier)
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
                }
                return None;
            }
            _ => return Some(()),
        }
    }
}
