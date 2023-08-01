use crate::ast::ast::{
    BoundedMethodKind, CallableBodyNode, CallablePrototypeNode, CoreAssignmentNode, CoreAtomNode,
    CoreIdentifierInDeclNode, CoreIdentifierInUseNode, CoreRVariableDeclarationNode,
    CoreSelfKeywordNode, CoreTypeExpressionNode, FunctionWrapperNode, InterfaceDeclarationNode,
    LambdaTypeDeclarationNode, OkIdentifierInDeclNode, OkIdentifierInUseNode, OkSelfKeywordNode,
    UnresolvedIdentifier,
};
use crate::error::diagnostics::{
    BuiltinFunctionNameOverlapError, ConstructorNotFoundInsideStructDeclarationError,
    FieldsNotInitializedInConstructorError, GenericTypeResolvedToOutsideScopeError,
    GenericTypesDeclarationInsideConstructorFoundError, IdentifierFoundInNonLocalsError,
    IdentifierNotFoundInAnyNamespaceError, IdentifierUsedBeforeInitializedError,
    InterfaceAlreadyExistInBoundsDeclarationError, MainFunctionNotFoundError,
    MainFunctionWrongTypeError, NonHashableTypeInIndexError, NonVoidConstructorReturnTypeError,
    SelfNotFoundError,
};
use crate::error::helper::IdentifierKind as IdentKind;
use crate::scope::builtin::{is_name_in_builtin_func, print_meta_data, range_meta_data};
use crate::scope::concrete::core::ConcreteTypesRegistryKey;
use crate::scope::core::{
    AbstractSymbolData, FunctionSymbolData, GenericTypeParams, InterfaceSymbolData, LookupData,
    LookupResult, UserDefinedTypeSymbolData, VariableSymbolData,
};
use crate::scope::function::{CallableKind, CallablePrototypeData};
use crate::scope::handler::{ConcreteSymbolDataEntry, NamespaceHandler, SymbolDataEntry};
use crate::scope::interfaces::{InterfaceBounds, InterfaceObject};
use crate::scope::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::types::core::CoreType;
use crate::{
    ast::{
        ast::{
            ASTNode, BlockNode, CoreAtomStartNode, CoreStatemenIndentWrapperNode,
            CoreStatementNode, Node, StructDeclarationNode, TypeExpressionNode, TypeResolveKind,
            VariableDeclarationNode,
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

#[derive(Debug)]
pub enum ResolveResult<T: AbstractSymbolData> {
    Ok(
        LookupData<T>,
        Option<ConcreteTypesRegistryKey>,
        bool,
        String,
    ),
    InvalidGenericTypeArgsProvided,
    NotInitialized(TextRange, String),
    Unresolved,
}

#[derive(Debug, Clone, Copy)]
pub enum BlockKind {
    Function,
    Lambda,
    LambdaType,
    Struct,
    Interface,
    Method,
    Conditional,
    Loop,
}

impl BlockKind {
    fn is_generics_shielding_block(&self) -> bool {
        match self {
            BlockKind::Function
            | BlockKind::Method
            | BlockKind::LambdaType
            | BlockKind::Struct
            | BlockKind::Interface
            | BlockKind::Lambda => true,
            BlockKind::Conditional | BlockKind::Loop => false,
        }
    }

    fn is_method(&self) -> bool {
        match self {
            BlockKind::Method => true,
            _ => false,
        }
    }
}

pub struct ClassContext {
    is_containing_self: bool,
}

pub struct BlockContext {
    variable_non_locals: FxHashSet<String>,
    function_non_locals: FxHashMap<String, bool>,
    block_kind: BlockKind,
    scope_index: usize,
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
                    block_kind: BlockKind::Function,
                    scope_index: 0,
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
        );
        self.namespace_handler.namespace.functions.force_insert(
            self.scope_index,
            "range".to_string(),
            range_meta_data(),
            TextRange::default(),
            false,
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
            Some(symbol_data) => {
                let func_meta_data = &*symbol_data.get_core_ref();
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

    pub fn open_block(&mut self, block_kind: BlockKind) {
        let new_scope_index = self
            .namespace_handler
            .namespace
            .open_scope(self.scope_index);
        self.scope_index = new_scope_index;
        self.indent_level = self.indent_level + 1;
        self.context.block_context_stack.push(BlockContext {
            variable_non_locals: FxHashSet::default(),
            function_non_locals: FxHashMap::default(),
            block_kind,
            scope_index: new_scope_index,
        });
    }

    pub fn close_block(&mut self, block: Option<&BlockNode>) {
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
        if let Some(block) = block {
            self.namespace_handler.set_non_locals(
                block,
                non_locals.variable_non_locals,
                non_locals.function_non_locals,
            );
        }
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

    pub fn get_enclosing_generics_declarative_scope_index(&self) -> (usize, Option<usize>) {
        // (enclosing_scope, enclosing_class_scope `if enclosing scope is method`)
        let mut index = self.context.block_context_stack.len() - 1;
        while index >= 0 {
            let block_context = &self.context.block_context_stack[index];
            if block_context.block_kind.is_generics_shielding_block() {
                if block_context.block_kind.is_method() {
                    return (
                        block_context.scope_index,
                        Some(self.context.block_context_stack[index - 1].scope_index),
                    );
                } else {
                    return (block_context.scope_index, None);
                }
            }
            index = index - 1;
        }
        unreachable!()
    }

    pub fn bind_decl_to_identifier_in_decl(
        &mut self,
        node: &OkIdentifierInDeclNode,
        symbol_data: SymbolDataEntry,
    ) {
        self.namespace_handler
            .identifier_in_decl_binding_table
            .insert(node.clone(), symbol_data);
    }

    pub fn bind_decl_to_identifier_in_use<T: AbstractSymbolData>(
        &mut self,
        node: &OkIdentifierInUseNode,
        symbol_data: &T,
        is_concrete_types_none_allowed: bool,
    ) -> (Option<ConcreteTypesRegistryKey>, bool) {
        // (index to the registry, has_generics)

        // TODO - check with symbol_data that whether the identifier even expects a generic type args.
        // if not raise error
        let (mut concrete_types, mut has_generics) =
            self.extract_angle_bracket_content_from_identifier_in_use(node);
        //if concrete_types.is_some() && !symbol_data.is_generics_allowed() {
        // TODO - raise error `no generic type arguments expected`
        //    concrete_types = None;
        //    has_generics = false;
        //}
        let result =
            symbol_data.check_generic_type_args(&concrete_types, is_concrete_types_none_allowed);
        let index = symbol_data.register_concrete_types(concrete_types, has_generics);
        let concrete_symbol_data = ConcreteSymbolDataEntry::new(symbol_data.get_entry(), index);
        self.namespace_handler
            .identifier_in_use_binding_table
            .insert(node.clone(), concrete_symbol_data);
        return (index, has_generics);
    }

    pub fn bind_decl_to_self_keyword(
        &mut self,
        node: &OkSelfKeywordNode,
        symbol_data: SymbolData<VariableData>,
    ) {
        self.namespace_handler
            .self_keyword_binding_table
            .insert(node.clone(), symbol_data);
    }

    pub fn try_resolving<
        T: AbstractSymbolData,
        U: Fn(&Namespace, usize, &str) -> LookupResult<T>,
    >(
        &mut self,
        identifier: &OkIdentifierInUseNode,
        lookup_fn: U,
        ident_kind: IdentKind,
        log_error: bool,
        is_concrete_types_none_allowed: bool,
    ) -> ResolveResult<T> {
        let name = identifier.token_value(&self.code);
        match lookup_fn(&self.namespace_handler.namespace, self.scope_index, &name) {
            LookupResult::Ok(lookup_data) => {
                let (key, has_generics) = self.bind_decl_to_identifier_in_use(
                    identifier,
                    &lookup_data.symbol_data,
                    is_concrete_types_none_allowed,
                );
                ResolveResult::Ok(lookup_data, key, has_generics, name)
            }
            LookupResult::NotInitialized(decl_range) => {
                if log_error {
                    let err = IdentifierUsedBeforeInitializedError::new(
                        &name,
                        ident_kind,
                        decl_range,
                        identifier.range(),
                    );
                    self.errors
                        .push(Diagnostics::IdentifierUsedBeforeInitialized(err));
                }
                ResolveResult::NotInitialized(decl_range, name)
            }
            LookupResult::Unresolved => {
                if log_error {
                    let err = IdentifierNotDeclaredError::new(ident_kind, identifier.range());
                    self.errors.push(Diagnostics::IdentifierNotDeclared(err));
                }
                ResolveResult::Unresolved
            }
        }
    }

    pub fn try_resolving_variable(
        &mut self,
        identifier: &OkIdentifierInUseNode,
        log_error: bool,
    ) -> ResolveResult<VariableSymbolData> {
        let lookup_fn = |namespace: &Namespace, scope_index: usize, key: &str| {
            namespace.lookup_in_variables_namespace(scope_index, key)
        };
        self.try_resolving(identifier, lookup_fn, IdentKind::Variable, log_error, false)
    }

    pub fn try_resolving_function(
        &mut self,
        identifier: &OkIdentifierInUseNode,
        log_error: bool,
    ) -> ResolveResult<FunctionSymbolData> {
        let lookup_fn = |namespace: &Namespace, scope_index: usize, key: &str| {
            namespace.lookup_in_functions_namespace(scope_index, key)
        };
        self.try_resolving(identifier, lookup_fn, IdentKind::Function, log_error, true)
    }

    pub fn try_resolving_user_defined_type(
        &mut self,
        identifier: &OkIdentifierInUseNode,
        log_error: bool,
        is_concrete_types_none_allowed: bool,
    ) -> ResolveResult<UserDefinedTypeSymbolData> {
        let lookup_fn = |namespace: &Namespace, scope_index: usize, key: &str| {
            namespace.lookup_in_types_namespace(scope_index, key)
        };
        self.try_resolving(
            identifier,
            lookup_fn,
            IdentKind::UserDefinedType,
            log_error,
            is_concrete_types_none_allowed,
        )
    }

    pub fn try_resolving_interface(
        &mut self,
        identifier: &OkIdentifierInUseNode,
        log_error: bool,
    ) -> ResolveResult<InterfaceSymbolData> {
        let lookup_fn = |namespace: &Namespace, scope_index: usize, key: &str| {
            namespace.lookup_in_interfaces_namespace(scope_index, key)
        };
        self.try_resolving(
            identifier,
            lookup_fn,
            IdentKind::Interface,
            log_error,
            false,
        )
    }

    pub fn try_resolving_self_keyword(
        &mut self,
        self_keyword: &OkSelfKeywordNode,
    ) -> Option<(SymbolData<VariableData>, usize)> {
        let name = self_keyword.token_value(&self.code);
        assert!(name == "self".to_string());
        match self
            .namespace_handler
            .namespace
            .lookup_in_variables_namespace(self.scope_index, &name)
        {
            LookupResult::Ok(lookup_data) => {
                let symbol_data = lookup_data.symbol_data;
                let depth = lookup_data.depth;
                self.bind_decl_to_self_keyword(self_keyword, symbol_data.0.clone());
                return Some((symbol_data.0, depth));
            }
            LookupResult::NotInitialized(_) => unreachable!(),
            LookupResult::Unresolved => return None,
        }
    }

    pub fn try_declare_and_bind<
        T: AbstractSymbolData,
        U: Fn(&mut Namespace, usize, String, TextRange) -> Result<T, (String, TextRange)>,
    >(
        &mut self,
        identifier: &OkIdentifierInDeclNode,
        declare_fn: U,
    ) -> Result<T, (String, TextRange)> {
        let name = identifier.token_value(&self.code);
        let result = declare_fn(
            &mut self.namespace_handler.namespace,
            self.scope_index,
            name,
            identifier.core_ref().name.range(),
        );
        match result {
            Ok(symbol_data) => {
                self.bind_decl_to_identifier_in_decl(identifier, symbol_data.get_entry());
                Ok(symbol_data)
            }
            Err(err) => Err(err),
        }
    }

    pub fn try_declare_and_bind_variable(
        &mut self,
        identifier: &OkIdentifierInDeclNode,
    ) -> Result<VariableSymbolData, (String, TextRange)> {
        let declare_fn =
            |namespace: &mut Namespace, scope_index: usize, name: String, decl_range: TextRange| {
                namespace.declare_variable(scope_index, name, decl_range)
            };
        self.try_declare_and_bind(identifier, declare_fn)
    }

    pub fn try_declare_and_bind_function(
        &mut self,
        identifier: &OkIdentifierInDeclNode,
    ) -> Result<FunctionSymbolData, (String, TextRange)> {
        let declare_fn =
            |namespace: &mut Namespace, scope_index: usize, name: String, decl_range: TextRange| {
                namespace.declare_function(scope_index, name, decl_range)
            };
        self.try_declare_and_bind(identifier, declare_fn)
    }

    pub fn try_declare_and_bind_struct_type(
        &mut self,
        identifier: &OkIdentifierInDeclNode,
    ) -> Result<UserDefinedTypeSymbolData, (String, TextRange)> {
        let declare_fn =
            |namespace: &mut Namespace, scope_index: usize, name: String, decl_range: TextRange| {
                namespace.declare_struct_type(scope_index, name, decl_range)
            };
        self.try_declare_and_bind(identifier, declare_fn)
    }

    pub fn try_declare_and_bind_interface(
        &mut self,
        identifier: &OkIdentifierInDeclNode,
    ) -> Result<InterfaceSymbolData, (String, TextRange)> {
        let declare_fn =
            |namespace: &mut Namespace, scope_index: usize, name: String, decl_range: TextRange| {
                namespace.declare_interface(scope_index, name, decl_range)
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
            TypeResolveKind::Unresolved(unresolved) => {
                for unresolved_identifier in unresolved {
                    match unresolved_identifier {
                        UnresolvedIdentifier::Unresolved(identifier) => {
                            let err = IdentifierNotDeclaredError::new(
                                IdentKind::UserDefinedType,
                                identifier.range(),
                            );
                            self.errors.push(Diagnostics::IdentifierNotDeclared(err));
                        }
                        UnresolvedIdentifier::GenericResolvedToOutsideScope(
                            identifier,
                            decl_range,
                        ) => {
                            let err = GenericTypeResolvedToOutsideScopeError::new(
                                identifier.range(),
                                decl_range,
                            );
                            self.errors
                                .push(Diagnostics::GenericTypeResolvedToOutsideScope(err));
                        }
                        UnresolvedIdentifier::NotInitialized(identifier, decl_range) => {
                            let name = identifier.token_value(&self.code);
                            let err = IdentifierUsedBeforeInitializedError::new(
                                &name,
                                IdentKind::UserDefinedType,
                                decl_range,
                                identifier.range(),
                            );
                            self.errors
                                .push(Diagnostics::IdentifierUsedBeforeInitialized(err));
                        }
                    }
                }
                return Type::new_with_unknown();
            }
            TypeResolveKind::Invalid => Type::new_with_unknown(),
        }
    }

    fn interface_obj_from_expression(
        &mut self,
        interface_expr: &OkIdentifierInUseNode,
    ) -> Option<InterfaceObject> {
        match self.try_resolving_interface(interface_expr, true) {
            ResolveResult::Ok(lookup_data, index, _, name) => {
                Some(InterfaceObject::new(name, lookup_data.symbol_data.0, index))
            }
            _ => None,
        }
    }

    fn extract_angle_bracket_content_from_identifier_in_use(
        &mut self,
        ok_identifier_in_use: &OkIdentifierInUseNode,
    ) -> (Option<Vec<Type>>, bool) {
        match &ok_identifier_in_use.core_ref().generic_type_args {
            Some((_, generic_type_args, _)) => {
                let mut has_generics = false;
                let mut concrete_types: Vec<Type> = vec![];
                for generic_type_expr in generic_type_args.iter() {
                    let ty = self.type_obj_from_expression(&generic_type_expr);
                    if ty.has_generics() {
                        has_generics = true;
                    }
                    concrete_types.push(ty);
                }
                return (Some(concrete_types), has_generics);
            }
            None => return (None, false),
        }
    }

    fn declare_angle_bracket_content_from_identifier_in_decl(
        &mut self,
        ok_identifier_in_decl: &OkIdentifierInDeclNode,
        decl_place_category: GenericTypeDeclarationPlaceCategory,
    ) -> (Option<GenericTypeParams>, Option<Vec<Type>>) {
        match &ok_identifier_in_decl.core_ref().generic_type_decls {
            Some((_, generic_type_decls, _)) => {
                let mut generic_type_params_vec: Vec<(String, InterfaceBounds, TextRange)> = vec![];
                let mut concrete_types: Vec<Type> = vec![];
                for (index, generic_type_decl) in generic_type_decls.iter().enumerate() {
                    let core_generic_type_decl = generic_type_decl.core_ref();
                    if let CoreIdentifierInDeclNode::Ok(ok_identifier_in_decl) =
                        core_generic_type_decl.generic_type_name.core_ref()
                    {
                        assert!(ok_identifier_in_decl
                            .core_ref()
                            .generic_type_decls
                            .is_none());
                        let generic_ty_name = ok_identifier_in_decl.token_value(&self.code);
                        let mut interface_bounds = InterfaceBounds::default();
                        if let Some((_, interface_bounds_node)) =
                            &core_generic_type_decl.interface_bounds
                        {
                            for interface_expr in interface_bounds_node.iter() {
                                if let CoreIdentifierInUseNode::Ok(interface_expr) =
                                    interface_expr.core_ref()
                                {
                                    if let Some(interface_obj) =
                                        self.interface_obj_from_expression(&interface_expr)
                                    {
                                        if let Some(previous_decl_range) = interface_bounds
                                            .insert(interface_obj, interface_expr.range())
                                        {
                                            let name = &interface_expr.token_value(&self.code);
                                            let err =
                                                InterfaceAlreadyExistInBoundsDeclarationError::new(
                                                    name,
                                                    previous_decl_range,
                                                    interface_expr.range(),
                                                );
                                            self.errors.push(Diagnostics::InterfaceAlreadyExistInBoundsDeclaration(err));
                                        }
                                    }
                                }
                            }
                        }
                        match self
                            .namespace_handler
                            .namespace
                            .declare_generic_type_with_meta_data(
                                self.scope_index,
                                generic_ty_name.to_string(),
                                index,
                                decl_place_category,
                                &interface_bounds,
                                ok_identifier_in_decl.range(),
                            ) {
                            Ok(symbol_data) => {
                                self.bind_decl_to_identifier_in_decl(
                                    ok_identifier_in_decl,
                                    symbol_data.get_entry(),
                                );
                                generic_type_params_vec.push((
                                    generic_ty_name.to_string(),
                                    interface_bounds,
                                    ok_identifier_in_decl.range(),
                                ));
                                concrete_types
                                    .push(Type::new_with_generic(generic_ty_name, &symbol_data.0))
                            }
                            Err((param_name, previous_decl_range)) => {
                                let err = IdentifierAlreadyDeclaredError::new(
                                    IdentKind::UserDefinedType,
                                    param_name,
                                    previous_decl_range,
                                    ok_identifier_in_decl.range(),
                                );
                                self.errors
                                    .push(Diagnostics::IdentifierAlreadyDeclared(err));
                            }
                        }
                    }
                }
                return (
                    Some(GenericTypeParams(generic_type_params_vec)),
                    Some(concrete_types),
                );
            }
            None => return (None, None),
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
        optional_identifier_in_decl: Option<&OkIdentifierInDeclNode>,
    ) -> (
        Vec<Type>,
        Type,
        Option<TextRange>,
        Option<(Vec<usize>, bool)>,
        Option<GenericTypeParams>,
    ) {
        let generic_type_decls = match optional_identifier_in_decl {
            Some(ok_identifier) => {
                self.declare_angle_bracket_content_from_identifier_in_decl(
                    ok_identifier,
                    GenericTypeDeclarationPlaceCategory::InCallable,
                )
                .0
            }
            None => None,
        };
        // (params_vec, return_type, return_type_span, is_concretization_required)
        let core_callable_prototype = callable_prototype.core_ref();
        let params = &core_callable_prototype.params;
        let return_type = &core_callable_prototype.return_type;
        let mut param_types_vec: Vec<Type> = vec![];
        let mut return_type_range: Option<TextRange> = None;
        let mut is_concretization_required_for_return_type = false;
        let mut generics_containing_params_indexes = vec![];
        let return_type: Type = match return_type {
            Some((_, return_type_expr)) => {
                return_type_range = Some(return_type_expr.range());
                let type_obj = self.type_obj_from_expression(return_type_expr);
                type_obj
            }
            None => Type::new_with_void(),
        };
        if return_type.has_generics() {
            is_concretization_required_for_return_type = true;
        }
        if let Some(params) = params {
            let params_iter = params.iter();
            for param in params_iter {
                let core_param = param.core_ref();
                let param_name = &core_param.name;
                if let CoreIdentifierInDeclNode::Ok(ok_identifier) = param_name.core_ref() {
                    let param_name = ok_identifier.token_value(&self.code);
                    let param_type = self.type_obj_from_expression(&core_param.data_type);
                    let result = self.namespace_handler.namespace.declare_variable_with_type(
                        self.scope_index,
                        param_name,
                        &param_type,
                        ok_identifier.range(),
                        true,
                    );
                    match result {
                        Ok(symbol_data) => {
                            self.bind_decl_to_identifier_in_decl(
                                ok_identifier,
                                symbol_data.get_entry(),
                            );
                            if param_type.has_generics() {
                                generics_containing_params_indexes.push(param_types_vec.len());
                            }
                            param_types_vec.push(param_type);
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
        let is_concretization_required = if generics_containing_params_indexes.len() == 0
            && !is_concretization_required_for_return_type
        {
            None
        } else {
            Some((
                generics_containing_params_indexes,
                is_concretization_required_for_return_type,
            ))
        };
        (
            param_types_vec,
            return_type,
            return_type_range,
            is_concretization_required,
            generic_type_decls,
        )
    }

    pub fn visit_callable_body(
        &mut self,
        callable_body: &CallableBodyNode,
        optional_identifier_in_decl: Option<&OkIdentifierInDeclNode>,
        symbol_data: &Option<FunctionSymbolData>,
    ) -> (
        Vec<Type>,
        Type,
        Option<TextRange>,
        Option<(Vec<usize>, bool)>,
    ) {
        let core_callable_body = callable_body.core_ref();
        let callable_body = &core_callable_body.block;
        self.open_block(callable_body.core_ref().kind);
        let (
            param_types_vec,
            return_type,
            return_type_range,
            is_concretization_required,
            generic_type_decls,
        ) = self
            .declare_callable_prototype(&core_callable_body.prototype, optional_identifier_in_decl);
        if let Some(symbol_data) = symbol_data {
            symbol_data
                .0
                .get_core_mut_ref()
                .set_generics(generic_type_decls);
        }
        for stmt in &*callable_body.0.as_ref().stmts.as_ref() {
            self.walk_stmt_indent_wrapper(stmt);
        }
        self.close_block(Some(callable_body));
        (
            param_types_vec,
            return_type,
            return_type_range,
            is_concretization_required,
        )
    }

    pub fn visit_method_body(
        &mut self,
        callable_body: &CallableBodyNode,
        optional_identifier_in_decl: Option<&OkIdentifierInDeclNode>,
    ) -> (
        Vec<Type>,
        Type,
        Option<TextRange>,
        Option<(Vec<usize>, bool)>,
        Option<GenericTypeParams>,
    ) {
        let core_callable_body = callable_body.core_ref();
        let callable_body = &core_callable_body.block;
        self.open_block(callable_body.core_ref().kind);
        let (
            param_types_vec,
            return_type,
            return_type_range,
            is_concretization_required,
            generic_type_decls,
        ) = self
            .declare_callable_prototype(&core_callable_body.prototype, optional_identifier_in_decl);
        for stmt in &*callable_body.0.as_ref().stmts.as_ref() {
            self.walk_stmt_indent_wrapper(stmt);
        }
        self.close_block(Some(callable_body));
        (
            param_types_vec,
            return_type,
            return_type_range,
            is_concretization_required,
            generic_type_decls,
        )
    }

    pub fn visit_constructor_body(
        &mut self,
        callable_body: &CallableBodyNode,
    ) -> (
        Vec<Type>,
        Type,
        Option<TextRange>,
        FxHashSet<String>,
        Option<(Vec<usize>, bool)>,
    ) {
        let core_callable_body = callable_body.core_ref();
        let mut initialized_fields: FxHashSet<String> = FxHashSet::default();
        let callable_body = &core_callable_body.block;
        self.open_block(callable_body.core_ref().kind);
        let (param_types_vec, return_type, return_type_range, is_concretization_required, _) =
            self.declare_callable_prototype(&core_callable_body.prototype, None);
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
                        if let CoreIdentifierInUseNode::Ok(property_name) = property_name.core_ref()
                        {
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
        self.close_block(Some(callable_body));
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
        let mut symbol_data: Option<VariableSymbolData> = None;
        if let CoreIdentifierInDeclNode::Ok(ok_identifier) = core_variable_decl.name.core_ref() {
            let name = ok_identifier.token_value(&self.code);
            if self.is_variable_in_non_locals(&name) {
                let err = IdentifierFoundInNonLocalsError::new(
                    IdentKind::Variable,
                    ok_identifier.range(),
                );
                self.errors
                    .push(Diagnostics::IdentifierFoundInNonLocals(err));
            } else {
                match self.try_declare_and_bind_variable(ok_identifier) {
                    Ok(local_symbol_data) => {
                        symbol_data = Some(local_symbol_data);
                    }
                    Err((name, previous_decl_range)) => {
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
        }
        // Except `CoreRAssignmentNode::LAMBDA`, type of the variable is set in the `type_checker.rs`. For `CoreRAssignmentNode::LAMBDA`,
        // it's type is set to the variable symbol_data here itself.
        match core_variable_decl.r_node.core_ref() {
            CoreRVariableDeclarationNode::Lambda(lambda_r_assign) => {
                // For case of lambda variable, it is allowed to be referenced inside the body to
                // enable recursive definitions
                if let Some(symbol_data) = &symbol_data {
                    symbol_data.0.get_core_mut_ref().set_is_init(true);
                }
                let core_lambda_r_assign = &lambda_r_assign.core_ref();
                let (params_vec, return_type, _, is_concretization_required) =
                    self.visit_callable_body(&core_lambda_r_assign.body, None, &None);
                let lambda_type_obj = Type::new_with_lambda_unnamed(CallablePrototypeData::new(
                    params_vec,
                    return_type,
                    is_concretization_required,
                ));
                if let Some(symbol_data) = &symbol_data {
                    symbol_data
                        .0
                        .get_core_mut_ref()
                        .set_data_type(&lambda_type_obj);
                }
            }
            CoreRVariableDeclarationNode::Expression(expr_r_assign) => {
                self.walk_expr_stmt(expr_r_assign);
            }
        }
        if let Some(symbol_data) = &symbol_data {
            symbol_data.0.get_core_mut_ref().set_is_init(true);
        }
    }

    pub fn declare_function(&mut self, func_wrapper: &FunctionWrapperNode) {
        let core_func_decl = func_wrapper.core_ref().func_decl.core_ref();
        let func_name = &core_func_decl.name;
        let body = &core_func_decl.body;
        let mut optional_ok_identifier_node = None;
        let mut symbol_data: Option<FunctionSymbolData> = None;
        if let CoreIdentifierInDeclNode::Ok(ok_identifier) = func_name.core_ref() {
            optional_ok_identifier_node = Some(ok_identifier);
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
                match self.try_declare_and_bind_function(ok_identifier) {
                    Ok(local_symbol_data) => symbol_data = Some(local_symbol_data),
                    Err((name, previous_decl_range)) => {
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
        }
        let (param_types_vec, return_type, _, is_concretization_required) =
            self.visit_callable_body(body, optional_ok_identifier_node, &symbol_data);
        if let Some(symbol_data) = &symbol_data {
            symbol_data.0.get_core_mut_ref().set_meta_data(
                param_types_vec,
                return_type,
                CallableKind::Function,
                is_concretization_required,
            );
        }
    }

    pub fn declare_struct_type(&mut self, struct_decl: &StructDeclarationNode) {
        self.context.class_context_stack.push(ClassContext {
            is_containing_self: false,
        });
        let core_struct_decl = struct_decl.core_ref();
        let struct_body = &core_struct_decl.block;
        let implementing_interfaces_node = &core_struct_decl.implementing_interfaces;
        let mut optional_ok_identifier_node = None;
        let mut symbol_data: Option<UserDefinedTypeSymbolData> = None;
        if let CoreIdentifierInDeclNode::Ok(ok_identifier) = core_struct_decl.name.core_ref() {
            optional_ok_identifier_node = Some(ok_identifier);
            match self.try_declare_and_bind_struct_type(ok_identifier) {
                Ok(local_symbol_data) => {
                    symbol_data = Some(local_symbol_data);
                }
                Err((name, previous_decl_range)) => {
                    let err = IdentifierAlreadyDeclaredError::new(
                        IdentKind::UserDefinedType,
                        name.to_string(),
                        previous_decl_range,
                        ok_identifier.range(),
                    );
                    self.errors
                        .push(Diagnostics::IdentifierAlreadyDeclared(err));
                }
            }
        };
        self.open_block(struct_body.core_ref().kind);
        let (struct_generic_type_decls, struct_ty) = match optional_ok_identifier_node {
            Some(ok_identifier) => {
                let (struct_generic_type_decls, concrete_types) = self
                    .declare_angle_bracket_content_from_identifier_in_decl(
                        ok_identifier,
                        GenericTypeDeclarationPlaceCategory::InStruct,
                    );
                let struct_ty = match &symbol_data {
                    Some(symbol_data) => {
                        let has_generics = if struct_generic_type_decls.is_some() {
                            true
                        } else {
                            false
                        };
                        let name = ok_identifier.token_value(&self.code);
                        let index = symbol_data
                            .0
                            .register_concrete_types(concrete_types, has_generics);
                        Type::new_with_struct(name, &symbol_data.0, index, has_generics)
                    }
                    None => Type::new_with_unknown(),
                };
                (struct_generic_type_decls, struct_ty)
            }
            None => (None, Type::new_with_unknown()),
        };

        let result = self.namespace_handler.namespace.declare_variable_with_type(
            self.scope_index,
            "self".to_string(),
            &struct_ty,
            core_struct_decl.name.range(),
            true,
        );
        assert!(result.is_ok());

        let mut implementing_interfaces: Option<InterfaceBounds> = None;
        if let Some((_, interfaces_node)) = implementing_interfaces_node {
            let mut interfaces = InterfaceBounds::default();
            for interface_expr in interfaces_node.iter() {
                if let CoreIdentifierInUseNode::Ok(interface_expr) = interface_expr.core_ref() {
                    if let Some(interface_obj) = self.interface_obj_from_expression(&interface_expr)
                    {
                        if let Some(previous_decl_range) =
                            interfaces.insert(interface_obj, interface_expr.range())
                        {
                            let name = &interface_expr.token_value(&self.code);
                            let err = InterfaceAlreadyExistInBoundsDeclarationError::new(
                                name,
                                previous_decl_range,
                                interface_expr.range(),
                            );
                            self.errors
                                .push(Diagnostics::InterfaceAlreadyExistInBoundsDeclaration(err));
                        }
                    }
                }
            }
            if interfaces.len() > 0 {
                implementing_interfaces = Some(interfaces);
            }
        }
        if let Some(symbol_data) = &symbol_data {
            symbol_data
                .0
                .get_core_mut_ref()
                .get_struct_data_mut_ref()
                .set_generics_and_interfaces(struct_generic_type_decls, implementing_interfaces);
        }

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
                    if let CoreIdentifierInDeclNode::Ok(ok_identifier) = name.core_ref() {
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
                    let mut optional_ok_identifier_node = None;
                    if let CoreIdentifierInDeclNode::Ok(ok_bounded_method_name) =
                        core_func_decl.name.core_ref()
                    {
                        optional_ok_identifier_node = Some(ok_bounded_method_name);
                        let method_name_str = ok_bounded_method_name.token_value(&self.code);
                        if method_name_str.eq("__init__") && constructor.is_none() {
                            is_constructor = true;
                            if let Some((_, generic_type_decls, _)) =
                                &ok_bounded_method_name.core_ref().generic_type_decls
                            {
                                let err = GenericTypesDeclarationInsideConstructorFoundError::new(
                                    generic_type_decls.range(),
                                );
                                self.errors.push(
                                    Diagnostics::GenericTypesDeclarationInsideConstructorFound(err),
                                );
                            }
                        }
                    }
                    let (
                        param_types_vec,
                        return_type,
                        return_type_range,
                        is_concretization_required,
                        method_generic_type_decls,
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
                            None,
                        )
                    } else {
                        self.visit_method_body(&core_func_decl.body, optional_ok_identifier_node)
                    };
                    if let CoreIdentifierInDeclNode::Ok(ok_bounded_method_name) =
                        core_func_decl.name.core_ref()
                    {
                        let func_meta_data = CallableData::new(
                            param_types_vec,
                            return_type.clone(),
                            CallableKind::Method,
                            is_concretization_required,
                            method_generic_type_decls,
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
                                    if let Some(return_type_range) = return_type_range {
                                        let err = NonVoidConstructorReturnTypeError::new(
                                            return_type_range,
                                        );
                                        self.errors
                                            .push(Diagnostics::NonVoidConstructorReturnType(err));
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
        self.close_block(Some(struct_body));
        if let CoreIdentifierInDeclNode::Ok(ok_identifier) = core_struct_decl.name.core_ref() {
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
            if let Some(symbol_data) = &symbol_data {
                symbol_data
                    .0
                    .get_core_mut_ref()
                    .get_struct_data_mut_ref()
                    .set_meta_data(fields_map, constructor, methods, class_methods);
            }
        }
        self.context.class_context_stack.pop();
    }

    pub fn declare_lambda_type(&mut self, lambda_type_decl: &LambdaTypeDeclarationNode) {
        let core_lambda_type_decl = lambda_type_decl.core_ref();
        let mut types_vec: Vec<Type> = vec![];
        let type_tuple = &core_lambda_type_decl.type_tuple;
        let return_type = &core_lambda_type_decl.return_type;
        let mut generics_containing_params_indexes = vec![];
        let mut is_concretization_required_for_return_type = false;
        let mut optional_ok_identifier_node: Option<&OkIdentifierInDeclNode> = None;
        let mut generic_type_decls: Option<GenericTypeParams> = None;
        self.open_block(BlockKind::LambdaType);
        if let CoreIdentifierInDeclNode::Ok(ok_identifier) = core_lambda_type_decl.name.core_ref() {
            optional_ok_identifier_node = Some(ok_identifier);
            generic_type_decls = self
                .declare_angle_bracket_content_from_identifier_in_decl(
                    ok_identifier,
                    GenericTypeDeclarationPlaceCategory::InCallable,
                )
                .0;
        }

        let return_type: Type = match return_type {
            Some(return_type_expr) => {
                let type_obj = self.type_obj_from_expression(return_type_expr);
                type_obj
            }
            None => Type::new_with_void(),
        };
        if return_type.has_generics() {
            is_concretization_required_for_return_type = true;
        }
        if let Some(type_tuple) = type_tuple {
            let type_tuple_iter = type_tuple.iter();
            for data_type in type_tuple_iter {
                let ty = self.type_obj_from_expression(&data_type);
                if ty.has_generics() {
                    generics_containing_params_indexes.push(types_vec.len());
                }
                types_vec.push(ty);
            }
        }
        self.close_block(None);
        if let Some(ok_identifier) = optional_ok_identifier_node {
            let name = ok_identifier.token_value(&self.code);
            let result = self
                .namespace_handler
                .namespace
                .declare_lambda_type_with_meta_data(
                    self.scope_index,
                    name,
                    types_vec,
                    return_type,
                    if generics_containing_params_indexes.len() == 0
                        && !is_concretization_required_for_return_type
                    {
                        None
                    } else {
                        Some((
                            generics_containing_params_indexes,
                            is_concretization_required_for_return_type,
                        ))
                    },
                    generic_type_decls,
                    ok_identifier.core_ref().name.range(),
                );
            match result {
                Ok(symbol_data) => {
                    self.bind_decl_to_identifier_in_decl(ok_identifier, symbol_data.get_entry());
                }
                Err((name, previous_decl_range)) => {
                    let err = IdentifierAlreadyDeclaredError::new(
                        IdentKind::UserDefinedType,
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

    pub fn declare_interface(&mut self, interface_decl: &InterfaceDeclarationNode) {
        let core_interface_decl = interface_decl.core_ref();
        let name = &core_interface_decl.name;
        let mut optional_ok_identifier_in_decl = None;
        let mut symbol_data: Option<InterfaceSymbolData> = None;
        if let CoreIdentifierInDeclNode::Ok(ok_identifier_in_decl) = name.core_ref() {
            optional_ok_identifier_in_decl = Some(ok_identifier_in_decl);
            // setting the interface first in scope enables the generic type declaration to use this interface
            // having recursive referencing
            match self.try_declare_and_bind_interface(ok_identifier_in_decl) {
                Ok(local_symbol_data) => symbol_data = Some(local_symbol_data),
                Err((name, previous_decl_range)) => {
                    let err = IdentifierAlreadyDeclaredError::new(
                        IdentKind::Interface,
                        name,
                        previous_decl_range,
                        ok_identifier_in_decl.range(),
                    );
                    self.errors
                        .push(Diagnostics::IdentifierAlreadyDeclared(err));
                }
            }
        }
        let interface_body = &core_interface_decl.block;
        self.open_block(interface_body.core_ref().kind);
        let generic_type_decls = match optional_ok_identifier_in_decl {
            Some(ok_identifier) => {
                self.declare_angle_bracket_content_from_identifier_in_decl(
                    ok_identifier,
                    GenericTypeDeclarationPlaceCategory::InStruct,
                )
                .0
            }
            None => None,
        };
        if let Some(symbol_data) = &symbol_data {
            symbol_data
                .0
                .get_core_mut_ref()
                .set_generics(generic_type_decls);
        }
        let mut fields_map: FxHashMap<String, (Type, TextRange)> = FxHashMap::default();
        let mut methods: FxHashMap<String, (CallableData, TextRange)> = FxHashMap::default();
        // traverse the body
        // ensure that the method name is not `__init__` etc.
        self.close_block(Some(interface_body));
        if let Some(symbol_data) = &symbol_data {
            symbol_data
                .0
                .get_core_mut_ref()
                .set_meta_data(fields_map, methods);
        }
    }
}

impl Visitor for Resolver {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match node {
            ASTNode::Block(block) => {
                let core_block = block.0.as_ref();
                self.open_block(core_block.kind);
                for stmt in &*core_block.stmts.as_ref() {
                    self.walk_stmt_indent_wrapper(stmt);
                }
                self.close_block(Some(block));
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
                        if let CoreIdentifierInUseNode::Ok(ok_identifier) = identifier.core_ref() {
                            if let ResolveResult::Ok(lookup_data, _, _, name) =
                                self.try_resolving_variable(ok_identifier, true)
                            {
                                if lookup_data.depth > 0 {
                                    self.set_to_variable_non_locals(name);
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
                        if let CoreIdentifierInUseNode::Ok(ok_identifier) =
                            core_func_call.function_name.core_ref()
                        {
                            // order of namespace search: function => type => variable
                            match self.try_resolving_function(ok_identifier, false) {
                                ResolveResult::Ok(lookup_data, _, _, name) => {
                                    let symbol_data = lookup_data.symbol_data;
                                    let depth = lookup_data.depth;
                                    let is_global = lookup_data.is_global;
                                    if depth > 0 && symbol_data.0 .2 {
                                        self.set_to_function_non_locals(name, is_global);
                                    }
                                }
                                ResolveResult::NotInitialized(_, _) => unreachable!(),
                                ResolveResult::InvalidGenericTypeArgsProvided => {
                                    // TODO - raise error ``
                                }
                                ResolveResult::Unresolved => {
                                    match self.try_resolving_user_defined_type(
                                        ok_identifier,
                                        false,
                                        true,
                                    ) {
                                        ResolveResult::Ok(_, _, _, _) => {}
                                        ResolveResult::NotInitialized(decl_range, name) => {
                                            let err = IdentifierUsedBeforeInitializedError::new(
                                                &name,
                                                IdentKind::UserDefinedType,
                                                decl_range,
                                                ok_identifier.range(),
                                            );
                                            self.errors.push(
                                                Diagnostics::IdentifierUsedBeforeInitialized(err),
                                            );
                                        }
                                        ResolveResult::InvalidGenericTypeArgsProvided => {
                                            // TODO - raise error `invalid generic type args`
                                        }
                                        ResolveResult::Unresolved => {
                                            match self.try_resolving_variable(ok_identifier, false)
                                            {
                                                ResolveResult::Ok(lookup_data, _, _, name) => {
                                                    let depth = lookup_data.depth;
                                                    if depth > 0 {
                                                        self.set_to_variable_non_locals(name);
                                                    }
                                                }
                                                ResolveResult::NotInitialized(decl_range, name) => {
                                                    let err =
                                                        IdentifierUsedBeforeInitializedError::new(
                                                            &name,
                                                            IdentKind::Variable,
                                                            decl_range,
                                                            ok_identifier.range(),
                                                        );
                                                    self.errors.push(
                                                        Diagnostics::IdentifierUsedBeforeInitialized(
                                                            err,
                                                        ),
                                                    );
                                                }
                                                ResolveResult::Unresolved => {
                                                    let err =
                                                        IdentifierNotFoundInAnyNamespaceError::new(
                                                            ok_identifier.range(),
                                                        );
                                                    self.errors.push(
                                                        Diagnostics::IdentifierNotFoundInAnyNamespace(err),
                                                    );
                                                }
                                                ResolveResult::InvalidGenericTypeArgsProvided => {
                                                    // TODO - raise error `Variable cannot have generic type arguments`
                                                    todo!()
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        if let Some(params) = &core_func_call.params {
                            self.walk_params(params)
                        }
                    }
                    CoreAtomStartNode::ClassMethodCall(class_method_call) => {
                        let core_class_method_call = class_method_call.core_ref();
                        if let CoreIdentifierInUseNode::Ok(ok_identifier) =
                            core_class_method_call.class_name.core_ref()
                        {
                            self.try_resolving_user_defined_type(ok_identifier, true, false);
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
