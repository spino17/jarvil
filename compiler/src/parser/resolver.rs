use super::helper::err_for_generic_type_args;
use crate::ast::ast::{
    BoundedMethodKind, CallableBodyNode, CallablePrototypeNode, CoreAssignmentNode, CoreAtomNode,
    CoreIdentifierInDeclNode, CoreIdentifierInUseNode, CoreRVariableDeclarationNode,
    CoreSelfKeywordNode, FunctionWrapperNode, InterfaceDeclarationNode, LambdaTypeDeclarationNode,
    OkIdentifierInDeclNode, OkIdentifierInUseNode, OkSelfKeywordNode, UnresolvedIdentifier,
    UserDefinedTypeNode,
};
use crate::error::diagnostics::{
    BuiltinFunctionNameOverlapError, ConstructorNotFoundInsideStructDeclarationError,
    FieldsNotInitializedInConstructorError, GenericTypeResolvedToOutsideScopeError,
    GenericTypesDeclarationInsideConstructorFoundError, IdentifierFoundInNonLocalsError,
    IdentifierNotFoundInAnyNamespaceError, IdentifierUsedBeforeInitializedError,
    InferredLambdaVariableTypeMismatchedWithTypeFromAnnotationError,
    InitMethodNotAllowedInsideConstructorError, InterfaceAlreadyExistInBoundsDeclarationError,
    MainFunctionNotFoundError, MainFunctionWrongTypeError, NonVoidConstructorReturnTypeError,
    SelfNotFoundError,
};
use crate::error::helper::IdentifierKind as IdentKind;
use crate::scope::builtin::{is_name_in_builtin_func, print_meta_data, range_meta_data};
use crate::scope::concrete::ConcreteTypesTuple;
use crate::scope::core::{
    AbstractSymbolData, FunctionSymbolData, GenericTypeParams, InterfaceSymbolData, LookupData,
    LookupResult, MangledIdentifierName, UserDefinedTypeSymbolData, VariableSymbolData,
};
use crate::scope::errors::GenericTypeArgsCheckError;
use crate::scope::function::{CallableKind, CallablePrototypeData};
use crate::scope::handler::{ConcreteSymbolDataEntry, SemanticStateDatabase, SymbolDataEntry};
use crate::scope::interfaces::{InterfaceBounds, InterfaceObject};
use crate::scope::types::core::UserDefineTypeKind;
use crate::scope::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::types::core::AbstractType;
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
    Ok(LookupData<T>, Option<ConcreteTypesTuple>, String),
    InvalidGenericTypeArgsProvided(GenericTypeArgsCheckError),
    NotInitialized(TextRange, String),
    Unresolved,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

    pub fn has_callable_body(&self) -> bool {
        match self {
            BlockKind::Function | BlockKind::Method | BlockKind::Lambda => true,
            BlockKind::LambdaType
            | BlockKind::Struct
            | BlockKind::Interface
            | BlockKind::Conditional
            | BlockKind::Loop => false,
        }
    }
}

pub struct ClassContext {
    is_containing_self: bool,
}

pub struct BlockContext {
    variable_non_locals: FxHashSet<MangledIdentifierName>,
    function_non_locals: FxHashSet<MangledIdentifierName>,
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
    pub semantic_state_db: SemanticStateDatabase,
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
                    function_non_locals: FxHashSet::default(),
                    block_kind: BlockKind::Function,
                    scope_index: 0,
                }],
            },
            indent_level: 0,
            semantic_state_db: SemanticStateDatabase::new(),
        }
    }

    pub fn resolve_ast(
        mut self,
        ast: &BlockNode,
    ) -> (SemanticStateDatabase, Vec<Diagnostics>, JarvilCode) {
        let code_block = ast.0.as_ref();
        // setting builtin functions to global scope
        // TODO - shift them to standard library
        self.semantic_state_db.namespace.functions.force_insert(
            self.scope_index,
            "print".to_string(),
            print_meta_data(),
            TextRange::default(),
        );
        self.semantic_state_db.namespace.functions.force_insert(
            self.scope_index,
            "range".to_string(),
            range_meta_data(),
            TextRange::default(),
        );
        for stmt in code_block.stmts.as_ref() {
            self.walk_stmt_indent_wrapper(stmt);
        }
        match self
            .semantic_state_db
            .namespace
            .functions
            .get(self.scope_index, "main")
        {
            Some(symbol_data) => {
                let func_meta_data = &*symbol_data.get_core_ref();
                let params = &func_meta_data.prototype.params;
                let return_type = &func_meta_data.prototype.return_type;
                if !params.is_empty() || !return_type.is_void() {
                    let span = symbol_data.declaration_line_number();
                    let err = MainFunctionWrongTypeError::new(span);
                    self.errors.push(Diagnostics::MainFunctionWrongType(err));
                }
            }
            None => {
                let err = MainFunctionNotFoundError::new();
                self.errors.push(Diagnostics::MainFunctionNotFound(err));
            }
        }
        (self.semantic_state_db, self.errors, self.code)
    }

    pub fn open_block(&mut self, block_kind: BlockKind) {
        let new_scope_index = self
            .semantic_state_db
            .namespace
            .open_scope(self.scope_index, block_kind);
        self.scope_index = new_scope_index;
        self.indent_level += 1;
        self.context.block_context_stack.push(BlockContext {
            variable_non_locals: FxHashSet::default(),
            function_non_locals: FxHashSet::default(),
            block_kind,
            scope_index: new_scope_index,
        });
    }

    pub fn close_block(&mut self, block: Option<&BlockNode>) {
        let parent_scope_index = match self
            .semantic_state_db
            .namespace
            .parent_scope_index(self.scope_index)
        {
            Some(parent_scope_index) => parent_scope_index,
            None => unreachable!(),
        };
        self.scope_index = parent_scope_index;
        self.indent_level -= 1;
        let non_locals = match self.context.block_context_stack.pop() {
            Some(block_context) => block_context,
            None => unreachable!(),
        };
        if let Some(block) = block {
            self.semantic_state_db
                .set_non_locals(block, non_locals.variable_non_locals);
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

    pub fn set_to_variable_non_locals(
        &mut self,
        name: MangledIdentifierName,
        enclosing_func_scope_depth: Option<usize>,
    ) {
        // variables are never resolved to global declarations as they are not allowed in Jarvil
        let len = self.context.block_context_stack.len();
        // TODO - add name inside enclosing function scope depth
        self.context.block_context_stack[len - 1]
            .variable_non_locals
            .insert(name);
    }

    pub fn is_variable_in_non_locals(&self, name: &str) -> bool {
        let len = self.context.block_context_stack.len();
        for key in &self.context.block_context_stack[len - 1].variable_non_locals {
            if key.jarvil_identifer_name == name {
                return true;
            }
        }
        false
    }

    pub fn set_to_function_non_locals(&mut self, name: MangledIdentifierName) {
        let len = self.context.block_context_stack.len();
        self.context.block_context_stack[len - 1]
            .function_non_locals
            .insert(name);
    }

    pub fn is_function_in_non_locals(&self, name: &str) -> bool {
        let len = self.context.block_context_stack.len();
        for key in &self.context.block_context_stack[len - 1].function_non_locals {
            if key.jarvil_identifer_name == name {
                return true;
            }
        }
        false
    }

    pub fn get_enclosing_generics_declarative_scope_index(&self) -> (usize, Option<usize>) {
        // (enclosing_scope, enclosing_class_scope `if enclosing scope is method`)
        let mut index = self.context.block_context_stack.len() - 1;
        loop {
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
            index -= 1;
        }
    }

    pub fn bind_decl_to_identifier_in_decl(
        &mut self,
        node: &OkIdentifierInDeclNode,
        symbol_data: SymbolDataEntry,
    ) {
        self.semantic_state_db
            .identifier_in_decl_binding_table
            .insert(node.clone(), symbol_data);
    }

    pub fn bind_decl_to_identifier_in_use<T: AbstractSymbolData>(
        &mut self,
        node: &OkIdentifierInUseNode,
        symbol_data: &T,
        is_concrete_types_none_allowed: bool,
    ) -> Result<(Option<ConcreteTypesTuple>, bool), GenericTypeArgsCheckError> {
        // (index to the registry, has_generics)
        let (concrete_types, ty_ranges, has_generics) =
            self.extract_angle_bracket_content_from_identifier_in_use(node);
        symbol_data.check_generic_type_args(
            &concrete_types,
            &ty_ranges,
            is_concrete_types_none_allowed,
        )?;
        // let index = symbol_data.register_concrete_types(concrete_types);
        let concrete_symbol_data =
            ConcreteSymbolDataEntry::new(symbol_data.get_entry(), concrete_types.clone());
        self.semantic_state_db
            .identifier_in_use_binding_table
            .insert(node.clone(), concrete_symbol_data);
        Ok((concrete_types, has_generics))
    }

    pub fn bind_decl_to_self_keyword(
        &mut self,
        node: &OkSelfKeywordNode,
        symbol_data: SymbolData<VariableData>,
    ) {
        self.semantic_state_db
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
        match lookup_fn(&self.semantic_state_db.namespace, self.scope_index, &name) {
            LookupResult::Ok(lookup_data) => {
                match self.bind_decl_to_identifier_in_use(
                    identifier,
                    &lookup_data.symbol_data,
                    is_concrete_types_none_allowed,
                ) {
                    Ok((concrete_types, _)) => ResolveResult::Ok(lookup_data, concrete_types, name),
                    Err(err) => {
                        if log_error {
                            let err = err_for_generic_type_args(
                                &err,
                                identifier.core_ref().name.range(),
                                ident_kind,
                            );
                            self.errors.push(err);
                        }
                        ResolveResult::InvalidGenericTypeArgsProvided(err)
                    }
                }
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
        assert!(name == *"self");
        match self
            .semantic_state_db
            .namespace
            .lookup_in_variables_namespace(self.scope_index, &name)
        {
            LookupResult::Ok(lookup_data) => {
                let symbol_data = lookup_data.symbol_data;
                let depth = lookup_data.depth;
                self.bind_decl_to_self_keyword(self_keyword, symbol_data.0.clone());
                Some((symbol_data.0, depth))
            }
            LookupResult::NotInitialized(_) => unreachable!(),
            LookupResult::Unresolved => None,
        }
    }

    pub fn try_declare_and_bind<
        T: AbstractSymbolData,
        U: Fn(&mut Namespace, usize, String, TextRange, usize) -> Result<T, (String, TextRange)>,
    >(
        &mut self,
        identifier: &OkIdentifierInDeclNode,
        declare_fn: U,
        unique_id: usize,
    ) -> Result<T, (String, TextRange)> {
        let name = identifier.token_value(&self.code);
        let result = declare_fn(
            &mut self.semantic_state_db.namespace,
            self.scope_index,
            name,
            identifier.core_ref().name.range(),
            unique_id,
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
        let declare_fn = |namespace: &mut Namespace,
                          scope_index: usize,
                          name: String,
                          decl_range: TextRange,
                          unique_id: usize| {
            namespace.declare_variable(scope_index, name, decl_range, unique_id)
        };
        let unique_id = self
            .semantic_state_db
            .unique_key_generator
            .generate_unique_id_for_variable();
        self.try_declare_and_bind(identifier, declare_fn, unique_id)
    }

    pub fn try_declare_and_bind_function(
        &mut self,
        identifier: &OkIdentifierInDeclNode,
    ) -> Result<FunctionSymbolData, (String, TextRange)> {
        let declare_fn = |namespace: &mut Namespace,
                          scope_index: usize,
                          name: String,
                          decl_range: TextRange,
                          unique_id: usize| {
            namespace.declare_function(scope_index, name, decl_range, unique_id)
        };
        let unique_id = self
            .semantic_state_db
            .unique_key_generator
            .generate_unique_id_for_function();
        self.try_declare_and_bind(identifier, declare_fn, unique_id)
    }

    pub fn try_declare_and_bind_struct_type(
        &mut self,
        identifier: &OkIdentifierInDeclNode,
    ) -> Result<UserDefinedTypeSymbolData, (String, TextRange)> {
        let declare_fn = |namespace: &mut Namespace,
                          scope_index: usize,
                          name: String,
                          decl_range: TextRange,
                          unique_id: usize| {
            namespace.declare_struct_type(scope_index, name, decl_range, unique_id)
        };
        let unique_id = self
            .semantic_state_db
            .unique_key_generator
            .generate_unique_id_for_type();
        self.try_declare_and_bind(identifier, declare_fn, unique_id)
    }

    pub fn try_declare_and_bind_interface(
        &mut self,
        identifier: &OkIdentifierInDeclNode,
    ) -> Result<InterfaceSymbolData, (String, TextRange)> {
        let declare_fn = |namespace: &mut Namespace,
                          scope_index: usize,
                          name: String,
                          decl_range: TextRange,
                          unique_id: usize| {
            namespace.declare_interface(scope_index, name, decl_range, unique_id)
        };
        let unique_id = self
            .semantic_state_db
            .unique_key_generator
            .generate_unique_id_for_interface();
        self.try_declare_and_bind(identifier, declare_fn, unique_id)
    }

    pub fn type_obj_from_user_defined_type_expr<'a>(
        &mut self,
        user_defined_ty_expr: &'a UserDefinedTypeNode,
        scope_index: usize,
        has_generics: &mut bool,
    ) -> TypeResolveKind<'a> {
        if let CoreIdentifierInUseNode::Ok(ok_identifier) =
            user_defined_ty_expr.core_ref().name.core_ref()
        {
            let name = ok_identifier.token_value(&self.code);
            match self
                .semantic_state_db
                .namespace
                .lookup_in_types_namespace(scope_index, &name)
            {
                LookupResult::Ok(lookup_data) => {
                    let symbol_data = lookup_data.symbol_data;
                    let resolved_scope_index = lookup_data.resolved_scope_index;
                    let ty_kind = symbol_data.0.get_core_ref().get_kind();
                    let result = match ty_kind {
                        UserDefineTypeKind::Struct => {
                            match self.bind_decl_to_identifier_in_use(
                                ok_identifier,
                                &symbol_data,
                                false,
                            ) {
                                Ok((concrete_types, has_generics_inside_angle_bracket_types)) => {
                                    if has_generics_inside_angle_bracket_types {
                                        *has_generics = true;
                                    }
                                    TypeResolveKind::Resolved(Type::new_with_struct(
                                        &symbol_data.0,
                                        concrete_types,
                                    ))
                                }
                                Err(err) => TypeResolveKind::Unresolved(vec![
                                    UnresolvedIdentifier::InvalidGenericTypeArgsProvided(
                                        ok_identifier,
                                        err,
                                    ),
                                ]),
                            }
                        }
                        UserDefineTypeKind::Lambda => {
                            match self.bind_decl_to_identifier_in_use(
                                ok_identifier,
                                &symbol_data,
                                false,
                            ) {
                                Ok((concrete_types, has_generics_inside_angle_bracket_types)) => {
                                    if has_generics_inside_angle_bracket_types {
                                        *has_generics = true;
                                    }
                                    TypeResolveKind::Resolved(Type::new_with_lambda_named(
                                        &symbol_data.0,
                                        concrete_types,
                                    ))
                                }
                                Err(err) => TypeResolveKind::Unresolved(vec![
                                    UnresolvedIdentifier::InvalidGenericTypeArgsProvided(
                                        ok_identifier,
                                        err,
                                    ),
                                ]),
                            }
                        }
                        UserDefineTypeKind::Generic => {
                            // NOTE: generic types are only allowed to be resolved inside local scope (of function, method, struct etc.)
                            // This kind of check is similiar to how `Rust` programming language expects generic type resolution.
                            let (expected_scope_index, possible_expected_class_scope_index) =
                                self.get_enclosing_generics_declarative_scope_index();
                            let result = if resolved_scope_index != expected_scope_index {
                                match possible_expected_class_scope_index {
                                    Some(class_scope_index) => {
                                        if resolved_scope_index != class_scope_index {
                                            Err(())
                                        } else {
                                            Ok(())
                                        }
                                    }
                                    None => Err(()),
                                }
                            } else {
                                Ok(())
                            };
                            match result {
                                Ok(_) => {
                                    match self.bind_decl_to_identifier_in_use(
                                        ok_identifier,
                                        &symbol_data,
                                        false,
                                    ) {
                                        Ok((concrete_types, _)) => {
                                            assert!(concrete_types.is_none());
                                            *has_generics = true;
                                            TypeResolveKind::Resolved(Type::new_with_generic(
                                                &symbol_data.0,
                                            ))
                                        }
                                        Err(err) => TypeResolveKind::Unresolved(vec![
                                            UnresolvedIdentifier::InvalidGenericTypeArgsProvided(
                                                ok_identifier,
                                                err,
                                            ),
                                        ]),
                                    }
                                }
                                Err(_) => TypeResolveKind::Unresolved(vec![
                                    UnresolvedIdentifier::GenericResolvedToOutsideScope(
                                        ok_identifier,
                                        symbol_data.0.declaration_line_number(),
                                    ),
                                ]),
                            }
                        }
                    };
                    return result;
                }
                LookupResult::NotInitialized(decl_range) => {
                    return TypeResolveKind::Unresolved(vec![UnresolvedIdentifier::NotInitialized(
                        ok_identifier,
                        decl_range,
                    )])
                }
                LookupResult::Unresolved => {
                    return TypeResolveKind::Unresolved(vec![UnresolvedIdentifier::Unresolved(
                        ok_identifier,
                    )])
                }
            }
        }
        TypeResolveKind::Invalid
    }

    pub fn type_obj_from_expression(&mut self, type_expr: &TypeExpressionNode) -> (Type, bool) {
        let mut has_generics = false;
        let ty_obj =
            match type_expr.type_obj_before_resolved(self, self.scope_index, &mut has_generics) {
                TypeResolveKind::Resolved(type_obj) => type_obj,
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
                            UnresolvedIdentifier::InvalidGenericTypeArgsProvided(
                                identifier,
                                err,
                            ) => {
                                let err = err_for_generic_type_args(
                                    &err,
                                    identifier.core_ref().name.range(),
                                    IdentKind::UserDefinedType,
                                );
                                self.errors.push(err);
                            }
                        }
                    }
                    Type::new_with_unknown()
                }
                TypeResolveKind::Invalid => Type::new_with_unknown(),
            };
        self.semantic_state_db
            .set_type_expr_obj_mapping(type_expr, &ty_obj, has_generics);
        (ty_obj, has_generics)
    }

    pub fn type_obj_for_expression_contained_inside_declarations(
        &mut self,
        type_expr: &TypeExpressionNode,
    ) -> (Type, bool) {
        let (mut ty, ty_has_generics) = self.type_obj_from_expression(type_expr);
        if ty_has_generics {
            ty.set_concretization_required_flag();
        }
        (ty, ty_has_generics)
    }

    fn interface_obj_from_expression(
        &mut self,
        interface_expr: &OkIdentifierInUseNode,
    ) -> Option<InterfaceObject> {
        match self.try_resolving_interface(interface_expr, true) {
            ResolveResult::Ok(lookup_data, concrete_types, name) => Some(InterfaceObject::new(
                name,
                lookup_data.symbol_data.0,
                concrete_types,
            )),
            _ => None,
        }
    }

    fn extract_angle_bracket_content_from_identifier_in_use(
        &mut self,
        ok_identifier_in_use: &OkIdentifierInUseNode,
    ) -> (Option<ConcreteTypesTuple>, Option<Vec<TextRange>>, bool) {
        match &ok_identifier_in_use.core_ref().generic_type_args {
            Some((_, generic_type_args, _)) => {
                let mut has_generics = false;
                let mut concrete_types: Vec<Type> = vec![];
                let mut ty_ranges: Vec<TextRange> = vec![];
                for generic_type_expr in generic_type_args.iter() {
                    let (ty, ty_has_generics) = self.type_obj_from_expression(generic_type_expr);
                    if ty_has_generics {
                        has_generics = true;
                    }
                    concrete_types.push(ty);
                    ty_ranges.push(generic_type_expr.range())
                }
                (
                    Some(ConcreteTypesTuple::new(concrete_types)),
                    Some(ty_ranges),
                    has_generics,
                )
            }
            None => (None, None, false),
        }
    }

    fn declare_angle_bracket_content_from_identifier_in_decl(
        &mut self,
        ok_identifier_in_decl: &OkIdentifierInDeclNode,
        decl_place_category: GenericTypeDeclarationPlaceCategory,
    ) -> (Option<GenericTypeParams>, Option<ConcreteTypesTuple>) {
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
                                        self.interface_obj_from_expression(interface_expr)
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
                        let unique_id = self
                            .semantic_state_db
                            .unique_key_generator
                            .generate_unique_id_for_type();
                        match self
                            .semantic_state_db
                            .namespace
                            .declare_generic_type_with_meta_data(
                                self.scope_index,
                                generic_ty_name.to_string(),
                                index,
                                decl_place_category,
                                &interface_bounds,
                                ok_identifier_in_decl.range(),
                                unique_id,
                            ) {
                            Ok(symbol_data) => {
                                self.bind_decl_to_identifier_in_decl(
                                    ok_identifier_in_decl,
                                    symbol_data.get_entry(),
                                );
                                generic_type_params_vec.push((
                                    generic_ty_name,
                                    interface_bounds,
                                    ok_identifier_in_decl.range(),
                                ));
                                concrete_types.push(Type::new_with_generic(&symbol_data.0))
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
                (
                    Some(GenericTypeParams(generic_type_params_vec)),
                    Some(ConcreteTypesTuple::new(concrete_types)),
                )
            }
            None => (None, None),
        }
    }

    fn is_already_a_method(
        methods: &FxHashMap<String, (CallableData, TextRange)>,
        class_methods: &FxHashMap<String, (CallableData, TextRange)>,
        name: &str,
    ) -> Option<TextRange> {
        match methods.get(name) {
            Some((_, previous_decl_range)) => Some(*previous_decl_range),
            None => class_methods
                .get(name)
                .map(|(_, previous_decl_range)| *previous_decl_range),
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
                let (type_obj, ty_has_generics) =
                    self.type_obj_for_expression_contained_inside_declarations(return_type_expr);
                if ty_has_generics {
                    is_concretization_required_for_return_type = true;
                }
                type_obj
            }
            None => Type::new_with_void(),
        };
        if let Some(params) = params {
            let params_iter = params.iter();
            for param in params_iter {
                let core_param = param.core_ref();
                let param_name = &core_param.name;
                if let CoreIdentifierInDeclNode::Ok(ok_identifier) = param_name.core_ref() {
                    let param_name = ok_identifier.token_value(&self.code);
                    let (param_type, param_ty_has_generics) = self
                        .type_obj_for_expression_contained_inside_declarations(
                            &core_param.data_type,
                        );
                    let unique_id = self
                        .semantic_state_db
                        .unique_key_generator
                        .generate_unique_id_for_variable();
                    let result = self.semantic_state_db.namespace.declare_variable_with_type(
                        self.scope_index,
                        param_name,
                        &param_type,
                        ok_identifier.range(),
                        true,
                        unique_id,
                    );
                    match result {
                        Ok(symbol_data) => {
                            self.bind_decl_to_identifier_in_decl(
                                ok_identifier,
                                symbol_data.get_entry(),
                            );
                            if param_ty_has_generics {
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
        let is_concretization_required = if generics_containing_params_indexes.is_empty()
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
        for stmt in callable_body.0.as_ref().stmts.as_ref() {
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
        for stmt in callable_body.0.as_ref().stmts.as_ref() {
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
        for stmt in callable_body.0.as_ref().stmts.as_ref() {
            let stmt = match stmt.core_ref() {
                CoreStatemenIndentWrapperNode::CorrectlyIndented(stmt) => stmt,
                CoreStatemenIndentWrapperNode::IncorrectlyIndented(stmt) => {
                    let core_stmt = stmt.core_ref();
                    &core_stmt.stmt
                }
                _ => continue,
            };
            self.walk_stmt(stmt);
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

        let ty_from_optional_annotation = core_variable_decl
            .ty_annotation
            .as_ref()
            .map(|(_, ty_expr)| (self.type_obj_from_expression(ty_expr).0, ty_expr.range()));
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
                let lambda_ty = Type::new_with_lambda_unnamed(CallablePrototypeData::new(
                    params_vec,
                    return_type,
                    is_concretization_required,
                ));
                let final_variable_ty = match ty_from_optional_annotation {
                    Some((ty_from_optional_annotation, range)) => {
                        if !ty_from_optional_annotation.is_eq(&lambda_ty) {
                            let err = InferredLambdaVariableTypeMismatchedWithTypeFromAnnotationError::new(
                                ty_from_optional_annotation.to_string(),
                                lambda_ty.to_string(),
                                range
                            );
                            self.errors.push(Diagnostics::InferredLambdaVariableTypeMismatchedWithTypeFromAnnotation(err));
                        }
                        ty_from_optional_annotation
                    }
                    None => lambda_ty,
                };
                if let Some(symbol_data) = &symbol_data {
                    symbol_data
                        .0
                        .get_core_mut_ref()
                        .set_data_type(&final_variable_ty);
                }
            }
            CoreRVariableDeclarationNode::Expression(expr_r_assign) => {
                self.walk_expr_stmt(expr_r_assign);
                if let Some(symbol_data) = &symbol_data {
                    match ty_from_optional_annotation {
                        Some((ty, _)) => symbol_data
                            .0
                            .get_core_mut_ref()
                            .set_data_type_from_optional_annotation(ty),
                        None => symbol_data.0.get_core_mut_ref().set_is_init(true),
                    }
                }
            }
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
                        name,
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
                    Some(symbol_data) => Type::new_with_struct(&symbol_data.0, concrete_types),
                    None => Type::new_with_unknown(),
                };
                (struct_generic_type_decls, struct_ty)
            }
            None => (None, Type::new_with_unknown()),
        };
        let unique_id = self
            .semantic_state_db
            .unique_key_generator
            .generate_unique_id_for_variable();
        let result = self.semantic_state_db.namespace.declare_variable_with_type(
            self.scope_index,
            "self".to_string(),
            &struct_ty,
            core_struct_decl.name.range(),
            true,
            unique_id,
        );
        assert!(result.is_ok());

        let mut implementing_interfaces: Option<InterfaceBounds> = None;
        if let Some((_, interfaces_node)) = implementing_interfaces_node {
            let mut interfaces = InterfaceBounds::default();
            for interface_expr in interfaces_node.iter() {
                if let CoreIdentifierInUseNode::Ok(interface_expr) = interface_expr.core_ref() {
                    if let Some(interface_obj) = self.interface_obj_from_expression(interface_expr)
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
        for stmt in struct_body.0.as_ref().stmts.as_ref() {
            let stmt = match stmt.core_ref() {
                CoreStatemenIndentWrapperNode::CorrectlyIndented(stmt) => stmt,
                CoreStatemenIndentWrapperNode::IncorrectlyIndented(stmt) => &stmt.core_ref().stmt,
                _ => continue,
            };
            match stmt.core_ref() {
                CoreStatementNode::StructPropertyDeclaration(struct_property_decl) => {
                    let core_struct_property_decl = struct_property_decl.core_ref();
                    let name_type_spec = core_struct_property_decl.name_type_spec.core_ref();
                    let name = &name_type_spec.name;
                    if let CoreIdentifierInDeclNode::Ok(ok_identifier) = name.core_ref() {
                        let field_name = ok_identifier.token_value(&self.code);
                        let (type_obj, _) = self
                            .type_obj_for_expression_contained_inside_declarations(
                                &name_type_spec.data_type,
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
                    let core_func_decl = bounded_method_wrapper.core_ref().func_decl.core_ref();
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
                        let method_meta_data = CallableData::new(
                            param_types_vec,
                            return_type,
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
                                        Some((method_meta_data, ok_bounded_method_name.range()));
                                    self.semantic_state_db.set_bounded_kind(
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
                                            (method_meta_data, ok_bounded_method_name.range()),
                                        );
                                        self.semantic_state_db.set_bounded_kind(
                                            bounded_method_wrapper,
                                            BoundedMethodKind::Method,
                                        );
                                    } else {
                                        class_methods.insert(
                                            method_name_str,
                                            (method_meta_data, ok_bounded_method_name.range()),
                                        );
                                        self.semantic_state_db.set_bounded_kind(
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
                    if !missing_fields_from_constructor.is_empty() {
                        let err = FieldsNotInitializedInConstructorError::new(
                            missing_fields_from_constructor,
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
            Some((_, return_type_expr)) => {
                let (type_obj, ty_has_generics) =
                    self.type_obj_for_expression_contained_inside_declarations(return_type_expr);
                if ty_has_generics {
                    is_concretization_required_for_return_type = true;
                }
                type_obj
            }
            None => Type::new_with_void(),
        };
        if let Some(type_tuple) = type_tuple {
            let type_tuple_iter = type_tuple.iter();
            for data_type in type_tuple_iter {
                let (ty, ty_has_generics) =
                    self.type_obj_for_expression_contained_inside_declarations(data_type);
                if ty_has_generics {
                    generics_containing_params_indexes.push(types_vec.len());
                }
                types_vec.push(ty);
            }
        }
        self.close_block(None);
        if let Some(ok_identifier) = optional_ok_identifier_node {
            let name = ok_identifier.token_value(&self.code);
            let unique_id = self
                .semantic_state_db
                .unique_key_generator
                .generate_unique_id_for_type();
            let result = self
                .semantic_state_db
                .namespace
                .declare_lambda_type_with_meta_data(
                    self.scope_index,
                    name,
                    types_vec,
                    return_type,
                    if generics_containing_params_indexes.is_empty()
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
                    unique_id,
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
        for stmt in interface_body.0.as_ref().stmts.as_ref() {
            let stmt = match stmt.core_ref() {
                CoreStatemenIndentWrapperNode::CorrectlyIndented(stmt) => stmt,
                CoreStatemenIndentWrapperNode::IncorrectlyIndented(stmt) => &stmt.core_ref().stmt,
                _ => continue,
            };
            match stmt.core_ref() {
                CoreStatementNode::StructPropertyDeclaration(interface_property_declaration) => {
                    let core_interface_property_decl = interface_property_declaration.core_ref();
                    let name_type_spec = core_interface_property_decl.name_type_spec.core_ref();
                    let name = &name_type_spec.name;
                    if let CoreIdentifierInDeclNode::Ok(ok_identifier) = name.core_ref() {
                        let field_name = ok_identifier.token_value(&self.code);
                        let (type_obj, _) = self
                            .type_obj_for_expression_contained_inside_declarations(
                                &name_type_spec.data_type,
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
                CoreStatementNode::InterfaceMethodPrototypeWrapper(interface_method_wrapper) => {
                    let core_interface_method_wrapper = interface_method_wrapper.core_ref();
                    if let CoreIdentifierInDeclNode::Ok(ok_identifier) =
                        core_interface_method_wrapper.name.core_ref()
                    {
                        let method_name = ok_identifier.token_value(&self.code);
                        if method_name == "__init__" {
                            let err = InitMethodNotAllowedInsideConstructorError::new(
                                ok_identifier.core_ref().name.range(),
                            );
                            self.errors
                                .push(Diagnostics::InitMethodNotAllowedInsideConstructor(err));
                        } else {
                            let prototype = &core_interface_method_wrapper.prototype;
                            self.open_block(BlockKind::Method);
                            let (
                                param_types_vec,
                                return_type,
                                _,
                                is_concretization_required,
                                method_generic_type_decls,
                            ) = self.declare_callable_prototype(prototype, Some(ok_identifier));
                            self.close_block(None);
                            let method_meta_data = CallableData::new(
                                param_types_vec,
                                return_type,
                                CallableKind::Method,
                                is_concretization_required,
                                method_generic_type_decls,
                            );
                            methods.insert(method_name, (method_meta_data, ok_identifier.range()));
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
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
                for stmt in core_block.stmts.as_ref() {
                    self.walk_stmt_indent_wrapper(stmt);
                }
                self.close_block(Some(block));
                None
            }
            ASTNode::VariableDeclaration(variable_decl) => {
                self.declare_variable(variable_decl);
                None
            }
            ASTNode::FunctionWrapper(func_wrapper) => {
                self.declare_function(func_wrapper);
                None
            }
            ASTNode::StructDeclaration(struct_decl) => {
                self.declare_struct_type(struct_decl);
                None
            }
            ASTNode::InterfaceDeclaration(interface_decl) => {
                self.declare_interface(interface_decl);
                None
            }
            ASTNode::LambdaTypeDeclaration(lambda_type_decl) => {
                self.declare_lambda_type(lambda_type_decl);
                None
            }
            ASTNode::TypeExpression(type_expr) => {
                self.type_obj_from_expression(type_expr);
                None
            }
            ASTNode::AtomStart(atom_start) => {
                match atom_start.core_ref() {
                    CoreAtomStartNode::Identifier(identifier) => {
                        if let CoreIdentifierInUseNode::Ok(ok_identifier) = identifier.core_ref() {
                            if let ResolveResult::Ok(lookup_data, _, _) =
                                self.try_resolving_variable(ok_identifier, true)
                            {
                                if lookup_data.depth > 0 {
                                    self.set_to_variable_non_locals(
                                        lookup_data.symbol_data.get_mangled_name(),
                                        lookup_data.enclosing_func_scope_depth,
                                    );
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
                                ResolveResult::Ok(lookup_data, _, _) => {
                                    let symbol_data = lookup_data.symbol_data;
                                    let depth = lookup_data.depth;
                                    if depth > 0 && symbol_data.0.is_suffix_required() {
                                        self.set_to_function_non_locals(
                                            symbol_data.get_mangled_name(),
                                        );
                                    }
                                }
                                ResolveResult::NotInitialized(_, _) => unreachable!(),
                                ResolveResult::InvalidGenericTypeArgsProvided(err) => {
                                    let err = err_for_generic_type_args(
                                        &err,
                                        ok_identifier.core_ref().name.range(),
                                        IdentKind::Function,
                                    );
                                    self.errors.push(err);
                                }
                                ResolveResult::Unresolved => {
                                    match self.try_resolving_user_defined_type(
                                        ok_identifier,
                                        false,
                                        true,
                                    ) {
                                        ResolveResult::Ok(_, _, _) => {}
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
                                        ResolveResult::InvalidGenericTypeArgsProvided(err) => {
                                            let err = err_for_generic_type_args(
                                                &err,
                                                ok_identifier.core_ref().name.range(),
                                                IdentKind::UserDefinedType,
                                            );
                                            self.errors.push(err);
                                        }
                                        ResolveResult::Unresolved => {
                                            match self.try_resolving_variable(ok_identifier, false)
                                            {
                                                ResolveResult::Ok(lookup_data, _, _) => {
                                                    let depth = lookup_data.depth;
                                                    if depth > 0 {
                                                        self.set_to_variable_non_locals(
                                                            lookup_data
                                                                .symbol_data
                                                                .get_mangled_name(),
                                                            lookup_data.enclosing_func_scope_depth,
                                                        );
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
                                                ResolveResult::InvalidGenericTypeArgsProvided(
                                                    err,
                                                ) => {
                                                    let err = err_for_generic_type_args(
                                                        &err,
                                                        ok_identifier.core_ref().name.range(),
                                                        IdentKind::Variable,
                                                    );
                                                    self.errors.push(err);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        if let Some(params) = &core_func_call.params {
                            self.walk_comma_separated_expressions(params)
                        }
                    }
                    CoreAtomStartNode::ClassMethodCall(class_method_call) => {
                        let core_class_method_call = class_method_call.core_ref();
                        if let CoreIdentifierInUseNode::Ok(ok_identifier) =
                            core_class_method_call.class_name.core_ref()
                        {
                            self.try_resolving_user_defined_type(ok_identifier, true, false);
                        }
                        self.walk_identifier_in_use(&core_class_method_call.class_method_name);
                        if let Some(params) = &core_class_method_call.params {
                            self.walk_comma_separated_expressions(params);
                        }
                    }
                }
                None
            }
            _ => Some(()),
        }
    }
}
