use crate::ast::ast::{
    BoundedMethodKind, CallableBodyNode, CallablePrototypeNode, CoreAtomNode, CoreCallableBodyNode,
    CoreRVariableDeclarationNode, CoreSelfKeywordNode, CoreTypeExpressionNode, FunctionWrapperNode,
    OkAssignmentNode, OkSelfKeywordNode,
};
use crate::constants::common::EIGHT_BIT_MAX_VALUE;
use crate::error::diagnostics::{
    BuiltinFunctionNameOverlapError, ConstructorNotFoundInsideStructDeclarationError,
    FieldsNotInitializedInConstructorError, IdentifierFoundInNonLocalsError,
    IdentifierNotFoundInAnyNamespaceError, MainFunctionNotFoundError, MainFunctionWrongTypeError,
    MoreThanMaxLimitParamsPassedError, NonHashableTypeInIndexError,
    NonVoidConstructorReturnTypeError, SelfNotFoundError, SingleSubTypeFoundInTupleError,
    VariableReferencedBeforeAssignmentError,
};
use crate::error::helper::IdentifierKind as IdentKind;
use crate::scope::builtin::{is_name_in_builtin_func, print_meta_data, range_meta_data};
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
    code::JarvilCode,
    error::{
        constants::STRUCT_NAME_NOT_BINDED_WITH_STRUCT_VARIANT_SYMBOL_DATA_MSG,
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
use rustc_hash::{FxHashMap, FxHashSet};
use std::{rc::Rc, vec};
use text_size::TextRange;

pub enum ResolveResult {
    OK(usize),   // depth
    Err(String), // name of the identifier
}

pub enum ErrorLoggingTypeKind {
    HASHMAP,
    TUPLE,
}

pub struct ClassContext {
    is_containing_self: bool,
    is_traversing_constructor: bool,
    constructor_initialized_fields: FxHashSet<String>,
}

pub struct BlockContext {
    variable_non_locals: FxHashSet<String>,
    function_non_locals: FxHashMap<String, bool>,
}

pub struct Context {
    class_context_stack: Vec<ClassContext>,
    block_context_stack: Vec<BlockContext>,
}

pub struct Resolver {
    namespace: Namespace,
    scope_index: usize,
    pub code: JarvilCode,
    errors: Vec<Diagnostics>,
    context: Context,
    indent_level: usize,
    ident_binding_table: FxHashMap<OkIdentifierNode, usize>, // binding mapping between `OkIdentifierNode` and `scope_index`
    self_binding_table: FxHashMap<OkSelfKeywordNode, usize>, // binding mapping between `OkSelfKeywordNode` and `scope_index`
}

impl Resolver {
    pub fn new(code: &JarvilCode) -> Self {
        Resolver {
            namespace: Namespace::new(), // `is_global` will be `true` for this namespace
            scope_index: 0,
            code: code.clone(),
            errors: vec![],
            context: Context {
                class_context_stack: vec![],
                block_context_stack: vec![BlockContext {
                    variable_non_locals: FxHashSet::default(),
                    function_non_locals: FxHashMap::default(),
                }],
            },
            indent_level: 0,
            ident_binding_table: FxHashMap::default(),
            self_binding_table: FxHashMap::default(),
        }
    }

    pub fn resolve_ast(mut self, ast: &BlockNode) -> (Namespace, Vec<Diagnostics>) {
        let code_block = &*ast.0.as_ref().borrow();
        // setting builtin functions to global scope
        self.namespace.functions.force_insert(
            self.scope_index,
            "print".to_string(),
            print_meta_data(),
            TextRange::default(),
            false,
        );
        self.namespace.functions.force_insert(
            self.scope_index,
            "range".to_string(),
            range_meta_data(),
            TextRange::default(),
            false,
        );
        for stmt in &code_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        match self
            .namespace
            .functions
            .get(self.scope_index, "main")
        {
            Some(symbol_data) => {
                let func_meta_data = &*symbol_data.0.as_ref().borrow();
                let params = &func_meta_data.params;
                let return_type = &func_meta_data.return_type;
                if params.as_ref().len() > 0 || !return_type.is_void() {
                    let span = symbol_data.1;
                    let err = MainFunctionWrongTypeError::new(span);
                    self.errors.push(Diagnostics::MainFunctionWrongType(err));
                }
            }
            None => {
                let err = MainFunctionNotFoundError::new();
                self.errors.push(Diagnostics::MainFunctionNotFound(err));
            }
        }
        (self.namespace, self.errors)
    }

    pub fn open_block(&mut self) {
        let new_scope_index = self.namespace.open_scope(self.scope_index);
        self.scope_index = new_scope_index;
        self.indent_level = self.indent_level + 1;
        self.context.block_context_stack.push(BlockContext {
            variable_non_locals: FxHashSet::default(),
            function_non_locals: FxHashMap::default(),
        });
    }

    pub fn close_block(&mut self, block: &BlockNode) {
        let parent_scope_index = match self.namespace.parent_scope_index(self.scope_index) {
            Some(parent_scope_index) => parent_scope_index,
            None => unreachable!(),
        };
        self.scope_index = parent_scope_index;
        self.indent_level = self.indent_level - 1;
        let non_locals = match self.context.block_context_stack.pop() {
            Some(block_context) => block_context,
            None => unreachable!(),
        };
        block.set_non_locals(
            non_locals.variable_non_locals,
            non_locals.function_non_locals,
        );
    }

    pub fn set_curr_class_context_is_containing_self(&mut self, value: bool) {
        let len = self.context.class_context_stack.len();
        self.context.class_context_stack[len - 1].is_containing_self = value;
    }

    pub fn get_curr_class_context_is_containing_self(&self) -> bool {
        let len = self.context.class_context_stack.len();
        self.context.class_context_stack[len - 1].is_containing_self
    }

    pub fn set_curr_class_context_constructor_initialized_fields(&mut self, field_name: String) {
        let len = self.context.class_context_stack.len();
        self.context.class_context_stack[len - 1]
            .constructor_initialized_fields
            .insert(field_name);
    }

    pub fn is_field_in_class_context_constructor_initialized_fields(
        &self,
        field_name: &str,
    ) -> bool {
        let len = self.context.class_context_stack.len();
        match self.context.class_context_stack[len - 1]
            .constructor_initialized_fields
            .get(field_name)
        {
            Some(_) => true,
            None => false,
        }
    }

    pub fn set_curr_class_context_is_traversing_constructor(&mut self, value: bool) {
        let len = self.context.class_context_stack.len();
        self.context.class_context_stack[len - 1].is_traversing_constructor = value;
    }

    pub fn get_curr_class_context_is_traversing_constructor(&self) -> bool {
        let len = self.context.class_context_stack.len();
        if len == 0 {
            return false;
        }
        self.context.class_context_stack[len - 1].is_traversing_constructor
    }

    pub fn set_to_variable_non_locals(&mut self, name: String) {
        // variables are never resolved to global declarations as they are not allowed in Jarvil
        let len = self.context.block_context_stack.len();
        self.context.block_context_stack[len - 1]
            .variable_non_locals
            .insert(name);
    }

    pub fn is_variable_in_non_locals(&self, name: &str) -> bool {
        let len = self.context.block_context_stack.len();
        self.context.block_context_stack[len - 1]
            .variable_non_locals
            .get(name)
            .is_some()
    }

    pub fn set_to_function_non_locals(&mut self, name: String, is_global: bool) {
        let len = self.context.block_context_stack.len();
        self.context.block_context_stack[len - 1]
            .function_non_locals
            .insert(name, is_global);
    }

    pub fn is_function_in_non_locals(&self, name: &str) -> bool {
        let len = self.context.block_context_stack.len();
        self.context.block_context_stack[len - 1]
            .function_non_locals
            .get(name)
            .is_some()
    }

    pub fn try_resolving<
        T,
        U: Fn(&Namespace, usize, &str) -> Option<(SymbolData<T>, usize, usize, bool)>,
        V: Fn(&OkIdentifierNode, &SymbolData<T>, usize),
    >(
        &mut self,
        identifier: &OkIdentifierNode,
        lookup_fn: U,
        bind_fn: V,
    ) -> ResolveResult {
        let name = identifier.token_value(&self.code);
        match lookup_fn(&self.namespace, self.scope_index, &name) {
            Some((symbol_data, _, depth, is_global)) => {
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
        let name = identifier.token_value(&self.code);
        match self
            .namespace
            .lookup_in_variables_namespace_with_is_init(self.scope_index, &name)
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
        let name = self_keyword.token_value(&self.code);
        assert!(name == "self".to_string());
        match self
            .namespace
            .lookup_in_variables_namespace(self.scope_index, &name)
        {
            Some((symbol_data, _, depth, _)) => {
                self_keyword.bind_decl(&symbol_data, depth);
                return Some((symbol_data, depth));
            }
            None => return None,
        }
    }

    pub fn try_resolving_function(&mut self, identifier: &OkIdentifierNode) -> ResolveResult {
        let lookup_fn = |namespace: &Namespace, scope_index: usize, key: &str| {
            namespace.lookup_in_functions_namespace(scope_index, key)
        };
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
        let lookup_fn = |namespace: &Namespace, scope_index: usize, key: &str| {
            namespace.lookup_in_types_namespace(scope_index, key)
        };
        let bind_fn = |identifier: &OkIdentifierNode,
                       symbol_data: &SymbolData<UserDefinedTypeData>,
                       depth: usize| {
            identifier.bind_user_defined_type_decl(symbol_data, depth)
        };
        self.try_resolving(identifier, lookup_fn, bind_fn)
    }

    pub fn try_declare_and_bind<
        T,
        U: Fn(&mut Namespace, usize, String, TextRange) -> Result<SymbolData<T>, (String, TextRange)>,
        V: Fn(&OkIdentifierNode, &SymbolData<T>),
    >(
        &mut self,
        identifier: &OkIdentifierNode,
        declare_fn: U,
        bind_fn: V,
    ) -> Option<(String, TextRange)> {
        let name = identifier.token_value(&self.code);
        let symbol_data = declare_fn(
            &mut self.namespace,
            self.scope_index,
            name,
            identifier.range(),
        );
        match symbol_data {
            Ok(symbol_data) => {
                bind_fn(identifier, &symbol_data);
                None
            }
            Err((name, previous_decl_range)) => Some((name, previous_decl_range)),
        }
    }

    pub fn try_declare_and_bind_variable(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<(String, TextRange)> {
        let declare_fn =
            |namespace: &mut Namespace, scope_index: usize, name: String, decl_range: TextRange| {
                namespace.declare_variable(scope_index, name, decl_range)
            };
        let bind_fn = |identifier: &OkIdentifierNode, symbol_data: &SymbolData<VariableData>| {
            identifier.bind_variable_decl(symbol_data, 0)
        };
        self.try_declare_and_bind(identifier, declare_fn, bind_fn)
    }

    pub fn try_declare_and_bind_function(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<(String, TextRange)> {
        let declare_fn =
            |namespace: &mut Namespace, scope_index: usize, name: String, decl_range: TextRange| {
                namespace.declare_function(scope_index, name, decl_range)
            };
        let bind_fn = |identifier: &OkIdentifierNode, symbol_data: &SymbolData<FunctionData>| {
            identifier.bind_function_decl(symbol_data, 0)
        };
        self.try_declare_and_bind(identifier, declare_fn, bind_fn)
    }

    pub fn try_declare_and_bind_struct_type(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<(String, TextRange)> {
        let declare_fn =
            |namespace: &mut Namespace, scope_index: usize, name: String, decl_range: TextRange| {
                namespace.declare_struct_type(scope_index, name, decl_range)
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
    ) -> Option<(String, TextRange)> {
        let declare_fn =
            |namespace: &mut Namespace, scope_index: usize, name: String, decl_range: TextRange| {
                namespace.declare_lambda_type(scope_index, name, decl_range)
            };
        let bind_fn = |identifier: &OkIdentifierNode,
                       symbol_data: &SymbolData<UserDefinedTypeData>| {
            identifier.bind_user_defined_type_decl(symbol_data, 0)
        };
        self.try_declare_and_bind(identifier, declare_fn, bind_fn)
    }

    pub fn pre_type_checking<T: Fn(&mut Resolver, TextRange, ErrorLoggingTypeKind)>(
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
                if !hashmap.key_type.is_hashable() {
                    if let Some((log_error_fn, resolver)) = is_log_error {
                        log_error_fn(resolver, index_span, ErrorLoggingTypeKind::HASHMAP)
                    }
                    return Type::new_with_unknown();
                }
                return type_obj.clone();
            }
            CoreType::TUPLE(tuple) => {
                if tuple.sub_types.len() <= 1 {
                    if let Some((log_error_fn, resolver)) = is_log_error {
                        log_error_fn(resolver, type_expr.range(), ErrorLoggingTypeKind::TUPLE)
                    }
                    return Type::new_with_unknown();
                }
                return type_obj.clone();
            }
            _ => return type_obj.clone(),
        }
    }

    pub fn type_obj_from_expression(&mut self, type_expr: &TypeExpressionNode) -> Type {
        match type_expr.type_obj_before_resolved(&self.namespace, self.scope_index, &self.code) {
            TypeResolveKind::RESOLVED(type_obj) => {
                let log_error_fn =
                    |resolver: &mut Resolver, span: TextRange, type_kind: ErrorLoggingTypeKind| {
                        match type_kind {
                            ErrorLoggingTypeKind::HASHMAP => {
                                let err = NonHashableTypeInIndexError::new(span);
                                resolver
                                    .errors
                                    .push(Diagnostics::NonHashableTypeInIndex(err));
                            }
                            ErrorLoggingTypeKind::TUPLE => {
                                let err = SingleSubTypeFoundInTupleError::new(span);
                                resolver
                                    .errors
                                    .push(Diagnostics::SingleSubTypeFoundInTuple(err));
                            }
                        }
                    };
                // This is type-checking prior to type-checking phase to
                // catch some early errors related to name resolution and
                // structure of the type.
                return Self::pre_type_checking(&type_obj, type_expr, Some((log_error_fn, self)));
            }
            TypeResolveKind::UNRESOLVED(identifier) => {
                for unresolved_identifier in identifier {
                    let err = IdentifierNotDeclaredError::new(
                        IdentKind::TYPE,
                        unresolved_identifier.range(),
                    );
                    self.errors.push(Diagnostics::IdentifierNotDeclared(err));
                }
                return Type::new_with_unknown();
            }
            TypeResolveKind::INVALID => Type::new_with_unknown(),
        }
    }

    pub fn declare_callable_prototype(
        &mut self,
        callable_prototype: &CallablePrototypeNode,
    ) -> (Vec<Type>, Type, Option<TextRange>) {
        let core_callable_prototype = callable_prototype.core_ref();
        let params = &core_callable_prototype.params;
        let return_type = &core_callable_prototype.return_type;
        let rparen = &core_callable_prototype.rparen;
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
                    let param_name = ok_identifier.token_value(&self.code);
                    let param_type = self.type_obj_from_expression(&core_param.data_type);
                    let symbol_data = self.namespace.declare_variable_with_type(
                        self.scope_index,
                        param_name,
                        &param_type,
                        ok_identifier.range(),
                        true,
                    );
                    match symbol_data {
                        Ok(symbol_data) => {
                            ok_identifier.bind_variable_decl(&symbol_data, 0);
                            param_types_vec.push(param_type);
                            params_count += 1;
                        }
                        Err((param_name, previous_decl_range)) => {
                            let err = IdentifierAlreadyDeclaredError::new(
                                IdentKind::VARIABLE,
                                param_name,
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
        (param_types_vec, return_type, return_type_range)
    }

    pub fn visit_callable_body(
        &mut self,
        callable_body_decl: &CallableBodyNode,
    ) -> (Vec<Type>, Type, Option<TextRange>) {
        let core_callable_body_decl = callable_body_decl.core_ref();
        match core_callable_body_decl {
            CoreCallableBodyNode::OK(ok_callable_body) => {
                let core_ok_callable_body = ok_callable_body.core_ref();
                let callable_body = &core_ok_callable_body.block;
                self.open_block();
                let (param_types_vec, return_type, return_type_range) =
                    self.declare_callable_prototype(&core_ok_callable_body.prototype);
                for stmt in &callable_body.0.as_ref().borrow().stmts {
                    self.walk_stmt_indent_wrapper(stmt);
                }
                self.close_block(callable_body);
                (param_types_vec, return_type, return_type_range)
            }
            CoreCallableBodyNode::MISSING_TOKENS(_) => {
                return (Vec::new(), Type::new_with_unknown(), None)
            }
        }
    }

    pub fn declare_variable(&mut self, variable_decl: &VariableDeclarationNode) {
        let core_variable_decl = variable_decl.core_ref();
        if let CoreIdentifierNode::OK(ok_identifier) = core_variable_decl.name.core_ref() {
            let name = ok_identifier.token_value(&self.code);
            if self.is_variable_in_non_locals(&name) {
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
                        name,
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
                let (params_vec, return_type, _) =
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
                                true,
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
            let name = ok_identifier.token_value(&self.code);
            if self.is_function_in_non_locals(&name) {
                let err = IdentifierFoundInNonLocalsError::new(
                    IdentKind::FUNCTION,
                    ok_identifier.range(),
                );
                self.errors
                    .push(Diagnostics::IdentifierFoundInNonLocals(err));
            } else if self.indent_level == 0 && is_name_in_builtin_func(&name) {
                let err = BuiltinFunctionNameOverlapError::new(ok_identifier.range());
                self.errors
                    .push(Diagnostics::BuiltinFunctionNameOverlap(err));
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
        let (param_types_vec, return_type, _) = self.visit_callable_body(body);
        if let CoreIdentifierNode::OK(ok_identifier) = func_name.core_ref() {
            if let Some(symbol_data) = ok_identifier.function_symbol_data(
                "function name should be resolved to `SymbolData<FunctionData>`",
            ) {
                symbol_data
                    .0
                    .as_ref()
                    .borrow_mut()
                    .set_data(param_types_vec, return_type);
            }
        }
    }

    pub fn declare_struct_type(&mut self, struct_decl: &StructDeclarationNode) {
        self.context.class_context_stack.push(ClassContext {
            is_containing_self: false,
            is_traversing_constructor: false,
            constructor_initialized_fields: FxHashSet::default(),
        });
        let core_struct_decl = struct_decl.core_ref();
        let struct_body = &core_struct_decl.block;
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
                                let name = ok_identifier.token_value(&self.code);
                                Type::new_with_struct(name, &symbol_data)
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
            self.scope_index,
            "self".to_string(),
            &struct_type_obj,
            core_struct_decl.name.range(),
            true,
        );
        assert!(result.is_ok());

        let mut fields_map: FxHashMap<String, (Type, TextRange)> = FxHashMap::default();
        let mut constructor: Option<(FunctionData, TextRange)> = None;
        let mut methods: FxHashMap<String, (FunctionData, TextRange)> = FxHashMap::default();
        let mut class_methods: FxHashMap<String, (FunctionData, TextRange)> = FxHashMap::default();

        fn is_already_a_method(
            methods: &FxHashMap<String, (FunctionData, TextRange)>,
            class_methods: &FxHashMap<String, (FunctionData, TextRange)>,
            name: &str,
        ) -> Option<TextRange> {
            match methods.get(name) {
                Some((_, previous_decl_range)) => return Some(previous_decl_range.clone()),
                None => match class_methods.get(name) {
                    Some((_, previous_decl_range)) => return Some(previous_decl_range.clone()),
                    None => return None,
                },
            }
        }
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
                                fields_map.insert(field_name, (type_obj, ok_identifier.range()));
                            }
                        }
                    }
                }
                CoreStatementNode::BOUNDED_METHOD_WRAPPER(bounded_method_wrappewr) => {
                    self.set_curr_class_context_is_containing_self(false);
                    let core_func_decl = &bounded_method_wrappewr
                        .0
                        .as_ref()
                        .borrow()
                        .func_decl
                        .core_ref()
                        .clone();
                    if let CoreIdentifierNode::OK(ok_bounded_method_name) =
                        core_func_decl.name.core_ref()
                    {
                        let method_name_str = ok_bounded_method_name.token_value(&self.code);
                        if method_name_str.eq("__init__") && constructor.is_none() {
                            self.set_curr_class_context_is_traversing_constructor(true);
                        }
                    }
                    let (param_types_vec, return_type, return_type_range) =
                        self.visit_callable_body(&core_func_decl.body);
                    if let CoreIdentifierNode::OK(ok_bounded_method_name) =
                        core_func_decl.name.core_ref()
                    {
                        let func_meta_data = FunctionData::new(param_types_vec, return_type);
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
                                    bounded_method_wrappewr
                                        .set_bounded_kind(BoundedMethodKind::CONSTRUCTOR);
                                    self.set_curr_class_context_is_traversing_constructor(false);
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
                                            method_name_str,
                                            (func_meta_data, ok_bounded_method_name.range()),
                                        );
                                        bounded_method_wrappewr
                                            .set_bounded_kind(BoundedMethodKind::METHOD);
                                    } else {
                                        class_methods.insert(
                                            method_name_str,
                                            (func_meta_data, ok_bounded_method_name.range()),
                                        );
                                        bounded_method_wrappewr
                                            .set_bounded_kind(BoundedMethodKind::CLASS_METHOD);
                                    }
                                }
                            }
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
        self.close_block(struct_body);
        // TODO - check here whether any fields are missing in constructor_initialized_fields
        if let CoreIdentifierNode::OK(ok_identifier) = core_struct_decl.name.core_ref() {
            match constructor {
                Some((_, construct_span)) => {
                    let mut missing_fields_from_constructor: Vec<&str> = vec![];
                    for (field_name, _) in fields_map.iter() {
                        if !self
                            .is_field_in_class_context_constructor_initialized_fields(field_name)
                        {
                            missing_fields_from_constructor.push(field_name);
                        }
                    }
                    if missing_fields_from_constructor.len() > 0 {
                        let len = missing_fields_from_constructor.len();
                        let mut err_msg_str = format!("`{}`", missing_fields_from_constructor[0]);
                        if len > 1 {
                            for i in 1..(len - 1) {
                                err_msg_str.push_str(&format!(
                                    ", `{}`",
                                    missing_fields_from_constructor[i]
                                ));
                            }
                            err_msg_str.push_str(&format!(
                                " and `{}`",
                                missing_fields_from_constructor[len - 1]
                            ));
                        }
                        let err = FieldsNotInitializedInConstructorError::new(
                            err_msg_str,
                            construct_span,
                        );
                        self.errors
                            .push(Diagnostics::FieldsNotInitializedInConstructor(err));
                    }
                }
                None => {
                    let err =
                        ConstructorNotFoundInsideStructDeclarationError::new(ok_identifier.range());
                    self.errors
                        .push(Diagnostics::ConstructorNotFoundInsideStructDeclaration(err));
                }
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
        self.context.class_context_stack.pop();
    }

    pub fn declare_lambda_type(&mut self, lambda_type_decl: &OkLambdaTypeDeclarationNode) {
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
        if let CoreIdentifierNode::OK(ok_identifier) = core_lambda_type_decl.name.core_ref() {
            let name = ok_identifier.token_value(&self.code);
            let symbol_data = self.namespace.declare_lambda_type_with_meta_data(
                self.scope_index,
                name,
                types_vec,
                return_type,
                ok_identifier.range(),
            );
            match symbol_data {
                Ok(symbol_data) => {
                    ok_identifier.bind_user_defined_type_decl(&symbol_data, 0);
                }
                Err((name, previous_decl_range)) => {
                    let err = IdentifierAlreadyDeclaredError::new(
                        IdentKind::TYPE,
                        name,
                        previous_decl_range,
                        ok_identifier.range(),
                    );
                    self.errors
                        .push(Diagnostics::IdentifierAlreadyDeclared(err));
                }
            }
        }
    }

    pub fn visit_ok_assignment(&mut self, ok_assignment: &OkAssignmentNode) {
        let core_ok_assignment = ok_assignment.core_ref();
        let l_atom = &core_ok_assignment.l_atom;
        let r_assign = &core_ok_assignment.r_assign;
        self.walk_atom(l_atom);
        self.walk_r_assignment(r_assign);
        if self.get_curr_class_context_is_traversing_constructor() {
            if let CoreAtomNode::PROPERTRY_ACCESS(property_access) = l_atom.core_ref() {
                let core_property_access = property_access.core_ref();
                let property_name = &core_property_access.propertry;
                if let CoreIdentifierNode::OK(property_name) = property_name.core_ref() {
                    let atom = &core_property_access.atom;
                    if let CoreAtomNode::ATOM_START(atom_start) = atom.core_ref() {
                        if let CoreAtomStartNode::SELF_KEYWORD(_) = atom_start.core_ref() {
                            let property_name_str = property_name.token_value(&self.code);
                            self.set_curr_class_context_constructor_initialized_fields(
                                property_name_str,
                            );
                        }
                    }
                }
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
                self.close_block(block);
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
            ASTNode::OK_ASSIGNMENT(ok_assignment) => {
                self.visit_ok_assignment(ok_assignment);
                return None;
            }
            ASTNode::ATOM_START(atom_start) => {
                match atom_start.core_ref() {
                    CoreAtomStartNode::IDENTIFIER(identifier) => {
                        if let CoreIdentifierNode::OK(ok_identifier) = identifier.core_ref() {
                            let name = ok_identifier.token_value(&self.code);
                            match self.try_resolving_variable(ok_identifier) {
                                VariableLookupResult::OK((_, depth)) => {
                                    if depth > 0 {
                                        self.set_to_variable_non_locals(name);
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
                            let name = ok_identifier.token_value(&self.code);
                            match self
                                .namespace
                                .lookup_in_functions_namespace(self.scope_index, &name)
                            {
                                Some((symbol_data, _, depth, is_global)) => {
                                    ok_identifier.bind_function_decl(&symbol_data, depth);
                                    // function is resolved to nonlocal scope and should be non-builtin
                                    if depth > 0 && symbol_data.2 {
                                        self.set_to_function_non_locals(name, is_global);
                                    }
                                }
                                None => match self
                                    .namespace
                                    .lookup_in_types_namespace(self.scope_index, &name)
                                {
                                    Some((symbol_data, _, depth, _)) => {
                                        ok_identifier
                                            .bind_user_defined_type_decl(&symbol_data, depth);
                                    }
                                    None => match self.try_resolving_variable(ok_identifier) {
                                        VariableLookupResult::OK((_, depth)) => {
                                            if depth > 0 {
                                                self.set_to_variable_non_locals(name);
                                            }
                                        }
                                        VariableLookupResult::NOT_INITIALIZED(decl_range) => {
                                            let err = VariableReferencedBeforeAssignmentError::new(
                                                name,
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
