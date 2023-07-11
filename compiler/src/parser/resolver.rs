use crate::ast::ast::{
    BoundedMethodKind, CallableBodyNode, CallablePrototypeNode, CoreAssignmentNode, CoreAtomNode,
    CoreRVariableDeclarationNode, CoreSelfKeywordNode, CoreTypeExpressionNode, FunctionWrapperNode,
    InterfaceDeclarationNode, LambdaTypeDeclarationNode, OkSelfKeywordNode,
};
use crate::constants::common::EIGHT_BIT_MAX_VALUE;
use crate::error::diagnostics::{
    BuiltinFunctionNameOverlapError, ConstructorNotFoundInsideStructDeclarationError,
    FieldsNotInitializedInConstructorError, IdentifierFoundInNonLocalsError,
    IdentifierNotFoundInAnyNamespaceError, MainFunctionNotFoundError, MainFunctionWrongTypeError,
    MismatchedConstructorReturnTypeError, MoreThanMaxLimitParamsPassedError,
    NonHashableTypeInIndexError, NonStructConstructorReturnTypeError, SelfNotFoundError,
    VariableReferencedBeforeAssignmentError, VoidConstructorReturnTypeError,
};
use crate::error::helper::IdentifierKind as IdentKind;
use crate::scope::builtin::{is_name_in_builtin_func, print_meta_data, range_meta_data};
use crate::scope::core::{GlobalSymbolDataRegistry, VariableLookupResult};
use crate::scope::function::{CallableKind, CallablePrototypeData};
use crate::scope::handler::{NamespaceHandler, SymbolDataEntry};
use crate::types::core::CoreType;
use crate::{
    ast::{
        ast::{
            ASTNode, BlockNode, CoreAtomStartNode, CoreIdentifierNode,
            CoreStatemenIndentWrapperNode, CoreStatementNode, Node, OkIdentifierNode,
            StructDeclarationNode, TypeExpressionNode, TypeResolveKind, VariableDeclarationNode,
        },
        walk::Visitor,
    },
    code::JarvilCode,
    error::diagnostics::{Diagnostics, IdentifierAlreadyDeclaredError, IdentifierNotDeclaredError},
    scope::{
        core::{Namespace, SymbolData},
        function::CallableData,
        variables::VariableData,
    },
    types::core::Type,
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::vec;
use text_size::TextRange;

pub enum ResolveResult {
    Ok(usize),   // depth
    Err(String), // name of the identifier
}

pub struct ClassContext {
    is_containing_self: bool,
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
    scope_index: usize,
    pub code: JarvilCode,
    errors: Vec<Diagnostics>,
    context: Context,
    indent_level: usize,
    pub namespace_handler: NamespaceHandler,
}

impl Resolver {
    pub fn new(code: JarvilCode) -> Self {
        Resolver {
            scope_index: 0,
            code,
            errors: vec![],
            context: Context {
                class_context_stack: vec![],
                block_context_stack: vec![BlockContext {
                    variable_non_locals: FxHashSet::default(),
                    function_non_locals: FxHashMap::default(),
                }],
            },
            indent_level: 0,
            namespace_handler: NamespaceHandler::new(),
        }
    }

    pub fn resolve_ast(
        mut self,
        ast: &BlockNode,
    ) -> (NamespaceHandler, Vec<Diagnostics>, JarvilCode) {
        let code_block = &*ast.0.as_ref();
        // setting builtin functions to global scope
        self.namespace_handler.namespace.functions.force_insert(
            self.scope_index,
            "print".to_string(),
            print_meta_data(),
            TextRange::default(),
            false,
            &mut self
                .namespace_handler
                .symbol_data_registry
                .functions_registry,
        );
        self.namespace_handler.namespace.functions.force_insert(
            self.scope_index,
            "range".to_string(),
            range_meta_data(),
            TextRange::default(),
            false,
            &mut self
                .namespace_handler
                .symbol_data_registry
                .functions_registry,
        );
        for stmt in &*code_block.stmts.as_ref() {
            self.walk_stmt_indent_wrapper(stmt);
        }
        match self
            .namespace_handler
            .namespace
            .functions
            .get(self.scope_index, "main")
        {
            Some(symbol_data_index) => {
                let symbol_data = self
                    .namespace_handler
                    .symbol_data_registry
                    .get_functions_symbol_data_ref_at_index(*symbol_data_index);
                let func_meta_data = &*symbol_data.0 .0.as_ref().borrow();
                let params = &func_meta_data.prototype.params;
                let return_type = &func_meta_data.prototype.return_type;
                if params.len() > 0 || !return_type.is_void() {
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
        (self.namespace_handler, self.errors, self.code)
    }

    pub fn open_block(&mut self) {
        let new_scope_index = self
            .namespace_handler
            .namespace
            .open_scope(self.scope_index);
        self.scope_index = new_scope_index;
        self.indent_level = self.indent_level + 1;
        self.context.block_context_stack.push(BlockContext {
            variable_non_locals: FxHashSet::default(),
            function_non_locals: FxHashMap::default(),
        });
    }

    pub fn close_block(&mut self, block: &BlockNode) {
        let parent_scope_index = match self
            .namespace_handler
            .namespace
            .parent_scope_index(self.scope_index)
        {
            Some(parent_scope_index) => parent_scope_index,
            None => unreachable!(),
        };
        self.scope_index = parent_scope_index;
        self.indent_level = self.indent_level - 1;
        let non_locals = match self.context.block_context_stack.pop() {
            Some(block_context) => block_context,
            None => unreachable!(),
        };
        self.namespace_handler.set_non_locals(
            block,
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

    pub fn bind_decl_to_identifier(
        &mut self,
        node: &OkIdentifierNode,
        symbol_data: SymbolDataEntry,
    ) {
        self.namespace_handler
            .identifier_binding_table
            .insert(node.clone(), symbol_data);
    }

    pub fn bind_decl_to_self_keyword(
        &mut self,
        node: &OkSelfKeywordNode,
        symbol_data_index: usize,
    ) {
        self.namespace_handler
            .self_keyword_binding_table
            .insert(node.clone(), symbol_data_index);
    }

    pub fn try_resolving<
        U: Fn(&Namespace, usize, &str) -> Option<(SymbolDataEntry, usize, usize, bool)>,
    >(
        &mut self,
        identifier: &OkIdentifierNode,
        lookup_fn: U,
    ) -> ResolveResult {
        let name = identifier.token_value(&self.code);
        match lookup_fn(&self.namespace_handler.namespace, self.scope_index, &name) {
            Some((symbol_data, _, depth, _)) => {
                self.bind_decl_to_identifier(identifier, symbol_data);
                ResolveResult::Ok(depth)
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
            .namespace_handler
            .namespace
            .lookup_in_variables_namespace_with_is_init(
                self.scope_index,
                &name,
                &self.namespace_handler.symbol_data_registry,
            ) {
            VariableLookupResult::Ok((symbol_data, resolved_scope_index, depth)) => {
                self.bind_decl_to_identifier(
                    identifier,
                    SymbolDataEntry::Variable(symbol_data.clone()),
                );
                return VariableLookupResult::Ok((symbol_data, resolved_scope_index, depth));
            }
            VariableLookupResult::NotInitialized(decl_range) => {
                return VariableLookupResult::NotInitialized(decl_range)
            }
            VariableLookupResult::Err => return VariableLookupResult::Err,
        }
    }

    pub fn try_resolving_self_keyword(
        &mut self,
        self_keyword: &OkSelfKeywordNode,
    ) -> Option<(usize, usize)> {
        let name = self_keyword.token_value(&self.code);
        assert!(name == "self".to_string());
        match self
            .namespace_handler
            .namespace
            .lookup_in_variables_namespace(self.scope_index, &name)
        {
            Some((symbol_data_index, _, depth, _)) => {
                self.bind_decl_to_self_keyword(self_keyword, symbol_data_index);
                return Some((symbol_data_index, depth));
            }
            None => return None,
        }
    }

    pub fn try_resolving_user_defined_type(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> ResolveResult {
        let lookup_fn = |namespace: &Namespace, scope_index: usize, key: &str| match namespace
            .lookup_in_types_namespace(scope_index, key)
        {
            Some(result) => {
                return Some((
                    SymbolDataEntry::Type(result.0),
                    result.1,
                    result.2,
                    result.3,
                ))
            }
            None => return None,
        };
        self.try_resolving(identifier, lookup_fn)
    }

    pub fn try_declare_and_bind<
        U: Fn(
            &mut Namespace,
            usize,
            String,
            TextRange,
            &mut GlobalSymbolDataRegistry,
        ) -> Result<SymbolDataEntry, (String, TextRange)>,
    >(
        &mut self,
        identifier: &OkIdentifierNode,
        declare_fn: U,
    ) -> Option<(String, TextRange)> {
        let name = identifier.token_value(&self.code);
        let symbol_data = declare_fn(
            &mut self.namespace_handler.namespace,
            self.scope_index,
            name,
            identifier.range(),
            &mut self.namespace_handler.symbol_data_registry,
        );
        match symbol_data {
            Ok(symbol_data) => {
                self.bind_decl_to_identifier(identifier, symbol_data);
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
            |namespace: &mut Namespace,
             scope_index: usize,
             name: String,
             decl_range: TextRange,
             symbol_data_registry: &mut GlobalSymbolDataRegistry| {
                namespace.declare_variable(scope_index, name, decl_range, symbol_data_registry)
            };
        self.try_declare_and_bind(identifier, declare_fn)
    }

    pub fn try_declare_and_bind_function(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<(String, TextRange)> {
        let declare_fn =
            |namespace: &mut Namespace,
             scope_index: usize,
             name: String,
             decl_range: TextRange,
             symbol_data_registry: &mut GlobalSymbolDataRegistry| {
                namespace.declare_function(scope_index, name, decl_range, symbol_data_registry)
            };
        self.try_declare_and_bind(identifier, declare_fn)
    }

    pub fn try_declare_and_bind_struct_type(
        &mut self,
        identifier: &OkIdentifierNode,
    ) -> Option<(String, TextRange)> {
        let declare_fn =
            |namespace: &mut Namespace,
             scope_index: usize,
             name: String,
             decl_range: TextRange,
             symbol_data_registry: &mut GlobalSymbolDataRegistry| {
                namespace.declare_struct_type(scope_index, name, decl_range, symbol_data_registry)
            };
        self.try_declare_and_bind(identifier, declare_fn)
    }

    pub fn pre_type_checking<T: Fn(&mut Resolver, TextRange)>(
        type_obj: &Type,
        type_expr: &TypeExpressionNode,
        is_log_error: Option<(T, &mut Resolver)>,
    ) -> Type {
        match type_obj.0.as_ref() {
            CoreType::HashMap(hashmap) => {
                let index_span = match type_expr.core_ref() {
                    CoreTypeExpressionNode::HashMap(hashmap_type_expr) => {
                        hashmap_type_expr.core_ref().key_type.range()
                    }
                    _ => unreachable!(),
                };
                if !hashmap.key_type.is_hashable() {
                    if let Some((log_error_fn, resolver)) = is_log_error {
                        log_error_fn(resolver, index_span)
                    }
                    return Type::new_with_unknown();
                }
                return type_obj.clone();
            }
            _ => return type_obj.clone(),
        }
    }

    pub fn type_obj_from_expression(&mut self, type_expr: &TypeExpressionNode) -> Type {
        match type_expr.type_obj_before_resolved(self, self.scope_index) {
            TypeResolveKind::Resolved(type_obj) => {
                let log_error_fn = |resolver: &mut Resolver, span: TextRange| {
                    let err = NonHashableTypeInIndexError::new(span);
                    resolver
                        .errors
                        .push(Diagnostics::NonHashableTypeInIndex(err));
                };
                // This is type-checking prior to type-checking phase to
                // catch some early errors related to name resolution and
                // structure of the type.
                return Self::pre_type_checking(&type_obj, type_expr, Some((log_error_fn, self)));
            }
            TypeResolveKind::Unresolved(identifier) => {
                for unresolved_identifier in identifier {
                    let err = IdentifierNotDeclaredError::new(
                        IdentKind::Type,
                        unresolved_identifier.range(),
                    );
                    self.errors.push(Diagnostics::IdentifierNotDeclared(err));
                }
                return Type::new_with_unknown();
            }
            TypeResolveKind::Invalid => Type::new_with_unknown(),
        }
    }

    fn is_already_a_method(
        methods: &FxHashMap<String, (CallableData, TextRange)>,
        class_methods: &FxHashMap<String, (CallableData, TextRange)>,
        name: &str,
    ) -> Option<TextRange> {
        match methods.get(name) {
            Some((_, previous_decl_range)) => return Some(*previous_decl_range),
            None => match class_methods.get(name) {
                Some((_, previous_decl_range)) => return Some(*previous_decl_range),
                None => return None,
            },
        }
    }

    pub fn declare_callable_prototype(
        &mut self,
        callable_prototype: &CallablePrototypeNode,
    ) -> (Vec<Type>, Type, Option<TextRange>, bool) {
        // (params_vec, return_type, return_type_span, is_concretization_required)
        let core_callable_prototype = callable_prototype.core_ref();
        let params = &core_callable_prototype.params;
        let return_type = &core_callable_prototype.return_type;
        let rparen = &core_callable_prototype.rparen;
        let mut param_types_vec: Vec<Type> = vec![];
        let mut return_type_range: Option<TextRange> = None;
        let mut is_concretization_required = false;
        let return_type: Type = match return_type {
            Some((_, return_type_expr)) => {
                return_type_range = Some(return_type_expr.range());
                let type_obj = self.type_obj_from_expression(return_type_expr);
                type_obj
            }
            None => Type::new_with_void(),
        };
        if return_type.has_generics() {
            is_concretization_required = true;
        }
        let mut params_count: usize = 0;
        if let Some(params) = params {
            let params_iter = params.iter();
            for param in params_iter {
                let core_param = param.core_ref();
                let param_name = &core_param.name;
                if let CoreIdentifierNode::Ok(ok_identifier) = param_name.core_ref() {
                    let param_name = ok_identifier.token_value(&self.code);
                    let param_type = self.type_obj_from_expression(&core_param.data_type);
                    let result = self.namespace_handler.namespace.declare_variable_with_type(
                        self.scope_index,
                        param_name,
                        &param_type,
                        ok_identifier.range(),
                        true,
                        &mut self.namespace_handler.symbol_data_registry,
                    );
                    match result {
                        Ok(symbol_data) => {
                            self.bind_decl_to_identifier(ok_identifier, symbol_data);
                            if param_type.has_generics() {
                                is_concretization_required = true;
                            }
                            param_types_vec.push(param_type);
                            params_count += 1;
                        }
                        Err((param_name, previous_decl_range)) => {
                            let err = IdentifierAlreadyDeclaredError::new(
                                IdentKind::Variable,
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
        (
            param_types_vec,
            return_type,
            return_type_range,
            is_concretization_required,
        )
    }

    pub fn visit_callable_body(
        &mut self,
        callable_body: &CallableBodyNode,
    ) -> (Vec<Type>, Type, Option<TextRange>, bool) {
        let core_callable_body = callable_body.core_ref();
        let callable_body = &core_callable_body.block;
        self.open_block();
        let (param_types_vec, return_type, return_type_range, is_concretization_required) =
            self.declare_callable_prototype(&core_callable_body.prototype);
        for stmt in &*callable_body.0.as_ref().stmts.as_ref() {
            self.walk_stmt_indent_wrapper(stmt);
        }
        self.close_block(callable_body);
        (
            param_types_vec,
            return_type,
            return_type_range,
            is_concretization_required,
        )
    }

    pub fn visit_constructor_body(
        &mut self,
        callable_body: &CallableBodyNode,
    ) -> (Vec<Type>, Type, Option<TextRange>, FxHashSet<String>, bool) {
        let core_callable_body = callable_body.core_ref();
        let mut initialized_fields: FxHashSet<String> = FxHashSet::default();
        let callable_body = &core_callable_body.block;
        self.open_block();
        let (param_types_vec, return_type, return_type_range, is_concretization_required) =
            self.declare_callable_prototype(&core_callable_body.prototype);
        for stmt in &*callable_body.0.as_ref().stmts.as_ref() {
            let stmt = match stmt.core_ref() {
                CoreStatemenIndentWrapperNode::CorrectlyIndented(stmt) => stmt,
                CoreStatemenIndentWrapperNode::IncorrectlyIndented(stmt) => {
                    let core_stmt = stmt.core_ref();
                    &core_stmt.stmt
                }
                _ => continue,
            };
            self.walk_stmt(&stmt);
            if let CoreStatementNode::Assignment(assignment) = stmt.core_ref() {
                if let CoreAssignmentNode::Ok(ok_assignment) = assignment.core_ref() {
                    let l_atom = &ok_assignment.core_ref().l_atom;
                    if let CoreAtomNode::PropertyAccess(property_access) = l_atom.core_ref() {
                        let core_property_access = property_access.core_ref();
                        let property_name = &core_property_access.propertry;
                        if let CoreIdentifierNode::Ok(property_name) = property_name.core_ref() {
                            let atom = &core_property_access.atom;
                            if let CoreAtomNode::AtomStart(atom_start) = atom.core_ref() {
                                if let CoreAtomStartNode::SelfKeyword(_) = atom_start.core_ref() {
                                    // l_atom of the form `self.<property_name>`
                                    let property_name_str = property_name.token_value(&self.code);
                                    initialized_fields.insert(property_name_str);
                                }
                            }
                        }
                    }
                }
            }
        }
        self.close_block(callable_body);
        (
            param_types_vec,
            return_type,
            return_type_range,
            initialized_fields,
            is_concretization_required,
        )
    }

    pub fn declare_variable(&mut self, variable_decl: &VariableDeclarationNode) {
        let core_variable_decl = variable_decl.core_ref();
        if let CoreIdentifierNode::Ok(ok_identifier) = core_variable_decl.name.core_ref() {
            let name = ok_identifier.token_value(&self.code);
            if self.is_variable_in_non_locals(&name) {
                let err = IdentifierFoundInNonLocalsError::new(
                    IdentKind::Variable,
                    ok_identifier.range(),
                );
                self.errors
                    .push(Diagnostics::IdentifierFoundInNonLocals(err));
            } else {
                if let Some((name, previous_decl_range)) =
                    self.try_declare_and_bind_variable(ok_identifier)
                {
                    let err = IdentifierAlreadyDeclaredError::new(
                        IdentKind::Variable,
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
            CoreRVariableDeclarationNode::Lambda(lambda_r_assign) => {
                // For case of lambda variable, it is allowed to be referenced inside the body to
                // enable recursive definitions
                if let CoreIdentifierNode::Ok(ok_identifier) = core_variable_decl.name.core_ref() {
                    if let Some(symbol_data) = self
                        .namespace_handler
                        .get_variable_symbol_data_ref(ok_identifier)
                    {
                        symbol_data.0 .0.as_ref().borrow_mut().set_is_init(true);
                    }
                };
                let core_lambda_r_assign = &lambda_r_assign.core_ref();
                let (params_vec, return_type, _, is_concretization_required) =
                    self.visit_callable_body(&core_lambda_r_assign.body);
                let lambda_type_obj = Type::new_with_lambda_unnamed(CallablePrototypeData::new(
                    params_vec,
                    return_type,
                    is_concretization_required,
                ));
                if let CoreIdentifierNode::Ok(ok_identifier) = core_variable_decl.name.core_ref() {
                    if let Some(symbol_data) = self
                        .namespace_handler
                        .get_variable_symbol_data_ref(ok_identifier)
                    {
                        symbol_data
                            .0
                             .0
                            .as_ref()
                            .borrow_mut()
                            .set_data_type(&lambda_type_obj);
                    }
                };
            }
            CoreRVariableDeclarationNode::Expression(expr_r_assign) => {
                self.walk_expr_stmt(expr_r_assign);
            }
        }
        if let CoreIdentifierNode::Ok(ok_identifier) = core_variable_decl.name.core_ref() {
            if let Some(symbol_data) = self
                .namespace_handler
                .get_variable_symbol_data_ref(ok_identifier)
            {
                symbol_data.0 .0.as_ref().borrow_mut().set_is_init(true);
            }
        };
    }

    pub fn declare_function(&mut self, func_wrapper: &FunctionWrapperNode) {
        let core_func_decl = func_wrapper.core_ref().func_decl.core_ref();
        let func_name = &core_func_decl.name;
        let body = &core_func_decl.body;
        if let CoreIdentifierNode::Ok(ok_identifier) = func_name.core_ref() {
            let name = ok_identifier.token_value(&self.code);
            if self.is_function_in_non_locals(&name) {
                let err = IdentifierFoundInNonLocalsError::new(
                    IdentKind::Function,
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
                        IdentKind::Function,
                        name.to_string(),
                        previous_decl_range,
                        ok_identifier.range(),
                    );
                    self.errors
                        .push(Diagnostics::IdentifierAlreadyDeclared(err));
                }
            }
        }
        let (param_types_vec, return_type, _, is_concretization_required) =
            self.visit_callable_body(body);
        if let CoreIdentifierNode::Ok(ok_identifier) = func_name.core_ref() {
            if let Some(symbol_data) = self
                .namespace_handler
                .get_function_symbol_data_ref(ok_identifier)
            {
                symbol_data.0 .0.as_ref().borrow_mut().set_data(
                    param_types_vec,
                    return_type,
                    CallableKind::Function,
                    is_concretization_required,
                    None,
                );
            }
        }
    }

    pub fn declare_struct_type(&mut self, struct_decl: &StructDeclarationNode) {
        self.context.class_context_stack.push(ClassContext {
            is_containing_self: false,
        });
        let core_struct_decl = struct_decl.core_ref();
        let struct_body = &core_struct_decl.block;
        let (struct_type_obj, struct_name) = match core_struct_decl.name.core_ref() {
            CoreIdentifierNode::Ok(ok_identifier) => {
                let temp_struct_type_obj =
                    match self.try_declare_and_bind_struct_type(ok_identifier) {
                        Some((name, previous_decl_range)) => {
                            let err = IdentifierAlreadyDeclaredError::new(
                                IdentKind::Type,
                                name.to_string(),
                                previous_decl_range,
                                ok_identifier.range(),
                            );
                            self.errors
                                .push(Diagnostics::IdentifierAlreadyDeclared(err));
                            Type::new_with_unknown()
                        }
                        None => {
                            match self
                                .namespace_handler
                                .get_type_symbol_data_ref(ok_identifier)
                            {
                                Some(symbol_data) => {
                                    let name = ok_identifier.token_value(&self.code);
                                    Type::new_with_struct(name, &symbol_data, None, false)
                                }
                                None => unreachable!(),
                            }
                        }
                    };
                (
                    temp_struct_type_obj,
                    Some(ok_identifier.token_value(&self.code)),
                )
            }
            _ => (Type::new_with_unknown(), None),
        };
        self.open_block();
        let result = self.namespace_handler.namespace.declare_variable_with_type(
            self.scope_index,
            "self".to_string(),
            &struct_type_obj,
            core_struct_decl.name.range(),
            true,
            &mut self.namespace_handler.symbol_data_registry,
        );
        assert!(result.is_ok());

        let mut fields_map: FxHashMap<String, (Type, TextRange)> = FxHashMap::default();
        let mut constructor: Option<(CallableData, TextRange)> = None;
        let mut methods: FxHashMap<String, (CallableData, TextRange)> = FxHashMap::default();
        let mut class_methods: FxHashMap<String, (CallableData, TextRange)> = FxHashMap::default();
        let mut initialized_fields: FxHashSet<String> = FxHashSet::default();
        for stmt in &*struct_body.0.as_ref().stmts.as_ref() {
            let stmt = match stmt.core_ref() {
                CoreStatemenIndentWrapperNode::CorrectlyIndented(stmt) => stmt,
                CoreStatemenIndentWrapperNode::IncorrectlyIndented(stmt) => &stmt.core_ref().stmt,
                _ => continue,
            };
            match stmt.core_ref() {
                CoreStatementNode::StructPropertyDeclaration(struct_property_decl) => {
                    let core_struct_stmt = struct_property_decl.core_ref();
                    let name = &core_struct_stmt.name_type_spec.core_ref().name;
                    if let CoreIdentifierNode::Ok(ok_identifier) = name.core_ref() {
                        let field_name = ok_identifier.token_value(&self.code);
                        let type_obj = self.type_obj_from_expression(
                            &core_struct_stmt.name_type_spec.core_ref().data_type,
                        );
                        match fields_map.get(&field_name) {
                            Some((_, previous_decl_range)) => {
                                let err = IdentifierAlreadyDeclaredError::new(
                                    IdentKind::Field,
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
                CoreStatementNode::BoundedMethodWrapper(bounded_method_wrapper) => {
                    self.set_curr_class_context_is_containing_self(false);
                    let core_func_decl = bounded_method_wrapper.0.as_ref().func_decl.core_ref();
                    let mut is_constructor = false;
                    if let CoreIdentifierNode::Ok(ok_bounded_method_name) =
                        core_func_decl.name.core_ref()
                    {
                        let method_name_str = ok_bounded_method_name.token_value(&self.code);
                        if method_name_str.eq("__init__") && constructor.is_none() {
                            is_constructor = true;
                        }
                    }
                    let (
                        param_types_vec,
                        return_type,
                        return_type_range,
                        is_concretization_required,
                    ) = if is_constructor {
                        let (
                            param_types_vec,
                            return_type,
                            return_type_range,
                            temp_initialized_fields,
                            is_concretization_required,
                        ) = self.visit_constructor_body(&core_func_decl.body);
                        initialized_fields = temp_initialized_fields;
                        (
                            param_types_vec,
                            return_type,
                            return_type_range,
                            is_concretization_required,
                        )
                    } else {
                        self.visit_callable_body(&core_func_decl.body)
                    };
                    if let CoreIdentifierNode::Ok(ok_bounded_method_name) =
                        core_func_decl.name.core_ref()
                    {
                        let func_meta_data = CallableData::new(
                            param_types_vec,
                            return_type.clone(),
                            CallableKind::Method,
                            is_concretization_required,
                            None,
                        );
                        let method_name_str = ok_bounded_method_name.token_value(&self.code);
                        if method_name_str.eq("__init__") {
                            match constructor {
                                Some((_, previous_decl_range)) => {
                                    let err = IdentifierAlreadyDeclaredError::new(
                                        IdentKind::Constructor,
                                        method_name_str,
                                        previous_decl_range,
                                        ok_bounded_method_name.range(),
                                    );
                                    self.errors
                                        .push(Diagnostics::IdentifierAlreadyDeclared(err));
                                }
                                None => {
                                    match return_type_range {
                                        Some(return_type_range) => match return_type.0.as_ref() {
                                            CoreType::Struct(struct_data) => {
                                                if let Some(struct_name) = &struct_name {
                                                    if !struct_data.name.eq(struct_name) {
                                                        let err = MismatchedConstructorReturnTypeError::new(
                                                                struct_name.to_string(),
                                                                return_type_range
                                                        );
                                                        self.errors.push(Diagnostics::MismatchedConstructorReturnType(err));
                                                    }
                                                }
                                            }
                                            CoreType::Void => unreachable!(),
                                            _ => {
                                                let err = NonStructConstructorReturnTypeError::new(
                                                    return_type_range,
                                                );
                                                self.errors.push(
                                                    Diagnostics::NonStructConstructorReturnType(
                                                        err,
                                                    ),
                                                );
                                            }
                                        },
                                        None => {
                                            let err = VoidConstructorReturnTypeError::new(
                                                ok_bounded_method_name.range(),
                                            );
                                            self.errors
                                                .push(Diagnostics::VoidConstructorReturnType(err));
                                        }
                                    }
                                    constructor =
                                        Some((func_meta_data, ok_bounded_method_name.range()));
                                    self.namespace_handler.set_bounded_kind(
                                        bounded_method_wrapper,
                                        BoundedMethodKind::Constructor,
                                    );
                                }
                            }
                        } else {
                            match Resolver::is_already_a_method(
                                &methods,
                                &class_methods,
                                &method_name_str,
                            ) {
                                Some(previous_decl_range) => {
                                    let err = IdentifierAlreadyDeclaredError::new(
                                        IdentKind::Method,
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
                                        self.namespace_handler.set_bounded_kind(
                                            bounded_method_wrapper,
                                            BoundedMethodKind::Method,
                                        );
                                    } else {
                                        class_methods.insert(
                                            method_name_str,
                                            (func_meta_data, ok_bounded_method_name.range()),
                                        );
                                        self.namespace_handler.set_bounded_kind(
                                            bounded_method_wrapper,
                                            BoundedMethodKind::ClassMethod,
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
        self.close_block(struct_body);
        if let CoreIdentifierNode::Ok(ok_identifier) = core_struct_decl.name.core_ref() {
            match constructor {
                Some((_, construct_span)) => {
                    let mut missing_fields_from_constructor: Vec<&str> = vec![];
                    for (field_name, _) in fields_map.iter() {
                        if initialized_fields.get(field_name).is_none() {
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
            if let Some(symbol_data) = self
                .namespace_handler
                .get_type_symbol_data_ref(ok_identifier)
            {
                symbol_data
                    .0
                     .0
                    .as_ref()
                    .borrow_mut()
                    .get_struct_data_mut_ref()
                    .set_meta_data(fields_map, constructor, methods, class_methods, None);
            }
        }
        self.context.class_context_stack.pop();
    }

    pub fn declare_interface(&mut self, interface_decl: &InterfaceDeclarationNode) {
        todo!()
    }

    pub fn declare_lambda_type(&mut self, lambda_type_decl: &LambdaTypeDeclarationNode) {
        let core_lambda_type_decl = lambda_type_decl.core_ref();
        let mut types_vec: Vec<Type> = vec![];
        let mut is_concretization_required = false;
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
        if return_type.has_generics() {
            is_concretization_required = true;
        }
        let mut types_count = 0;
        if let Some(type_tuple) = type_tuple {
            let type_tuple_iter = type_tuple.iter();
            for data_type in type_tuple_iter {
                let ty = self.type_obj_from_expression(&data_type);
                if ty.has_generics() {
                    is_concretization_required = true;
                }
                types_vec.push(ty);
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
        if let CoreIdentifierNode::Ok(ok_identifier) = core_lambda_type_decl.name.core_ref() {
            let name = ok_identifier.token_value(&self.code);
            let result = self
                .namespace_handler
                .namespace
                .declare_lambda_type_with_meta_data(
                    self.scope_index,
                    name,
                    types_vec,
                    return_type,
                    is_concretization_required,
                    ok_identifier.range(),
                    &mut self.namespace_handler.symbol_data_registry,
                );
            match result {
                Ok(symbol_data) => {
                    self.bind_decl_to_identifier(ok_identifier, symbol_data);
                }
                Err((name, previous_decl_range)) => {
                    let err = IdentifierAlreadyDeclaredError::new(
                        IdentKind::Type,
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
}

impl Visitor for Resolver {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match node {
            ASTNode::Block(block) => {
                self.open_block();
                let core_block = block.0.as_ref();
                for stmt in &*core_block.stmts.as_ref() {
                    self.walk_stmt_indent_wrapper(stmt);
                }
                self.close_block(block);
                return None;
            }
            ASTNode::VariableDeclaration(variable_decl) => {
                self.declare_variable(variable_decl);
                return None;
            }
            ASTNode::FunctionWrapper(func_wrapper) => {
                self.declare_function(func_wrapper);
                return None;
            }
            ASTNode::StructDeclaration(struct_decl) => {
                self.declare_struct_type(struct_decl);
                return None;
            }
            ASTNode::InterfaceDeclaration(interface_decl) => {
                self.declare_interface(interface_decl);
                return None;
            }
            ASTNode::LambdaTypeDeclaration(lambda_type_decl) => {
                self.declare_lambda_type(lambda_type_decl);
                return None;
            }
            ASTNode::AtomStart(atom_start) => {
                match atom_start.core_ref() {
                    CoreAtomStartNode::Identifier(identifier) => {
                        if let CoreIdentifierNode::Ok(ok_identifier) = identifier.core_ref() {
                            let name = ok_identifier.token_value(&self.code);
                            match self.try_resolving_variable(ok_identifier) {
                                VariableLookupResult::Ok((_, _, depth)) => {
                                    if depth > 0 {
                                        self.set_to_variable_non_locals(name);
                                    }
                                }
                                VariableLookupResult::NotInitialized(decl_range) => {
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
                                        IdentKind::Variable,
                                        ok_identifier.range(),
                                    );
                                    self.errors.push(Diagnostics::IdentifierNotDeclared(err));
                                }
                            }
                        }
                    }
                    CoreAtomStartNode::SelfKeyword(self_keyword) => {
                        if let CoreSelfKeywordNode::Ok(ok_self_keyword) = self_keyword.core_ref() {
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
                    CoreAtomStartNode::Call(func_call) => {
                        let core_func_call = func_call.core_ref();
                        if let CoreIdentifierNode::Ok(ok_identifier) =
                            core_func_call.function_name.core_ref()
                        {
                            // order of namespace search: function => type => variable
                            let name = ok_identifier.token_value(&self.code);
                            match self
                                .namespace_handler
                                .namespace
                                .lookup_in_functions_namespace(self.scope_index, &name)
                            {
                                Some((symbol_data_index, _, depth, is_global)) => {
                                    // function is resolved to nonlocal scope and should be non-builtin
                                    let symbol_data = self
                                        .namespace_handler
                                        .symbol_data_registry
                                        .get_functions_symbol_data_ref_at_index(symbol_data_index);
                                    if depth > 0 && symbol_data.2 {
                                        self.set_to_function_non_locals(name, is_global);
                                    }
                                    self.bind_decl_to_identifier(
                                        ok_identifier,
                                        SymbolDataEntry::Function(symbol_data_index),
                                    );
                                }
                                None => match self
                                    .namespace_handler
                                    .namespace
                                    .lookup_in_types_namespace(self.scope_index, &name)
                                {
                                    Some((symbol_data, _, _, _)) => {
                                        self.bind_decl_to_identifier(
                                            ok_identifier,
                                            SymbolDataEntry::Type(symbol_data),
                                        );
                                    }
                                    None => match self.try_resolving_variable(ok_identifier) {
                                        VariableLookupResult::Ok((_, _, depth)) => {
                                            if depth > 0 {
                                                self.set_to_variable_non_locals(name);
                                            }
                                        }
                                        VariableLookupResult::NotInitialized(decl_range) => {
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
                    CoreAtomStartNode::ClassMethodCall(class_method_call) => {
                        let core_class_method_call = class_method_call.core_ref();
                        if let CoreIdentifierNode::Ok(ok_identifier) =
                            core_class_method_call.class_name.core_ref()
                        {
                            if let ResolveResult::Err(_) =
                                self.try_resolving_user_defined_type(ok_identifier)
                            {
                                let err = IdentifierNotDeclaredError::new(
                                    IdentKind::Type,
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
