use super::helper::err_for_generic_ty_args;
use crate::ast::ast::{
    AtomStartNode, BoundedMethodKind, CallExpressionNode, CallableBodyNode, CallablePrototypeNode,
    CoreAssignmentNode, CoreAtomNode, CoreIdentifierInDeclNode, CoreIdentifierInUseNode,
    CoreRVariableDeclarationNode, CoreSelfKeywordNode, EnumDeclarationNode,
    EnumVariantExprOrClassMethodCallNode, ForLoopStatementNode, FunctionWrapperNode,
    IdentifierInUseNode, InterfaceDeclarationNode, LambdaTypeDeclarationNode,
    MatchCaseStatementNode, OkIdentifierInDeclNode, OkIdentifierInUseNode, OkSelfKeywordNode,
    SelfKeywordNode, UnresolvedIdentifier, UserDefinedTypeNode,
};
use crate::ast::iterators::SymbolSeparatedSequenceIterator;
use crate::code::JarvilCodeHandler;
use crate::core::string_interner::{IdentName, Interner};
use crate::error::diagnostics::{
    ConstructorNotFoundInsideStructDeclarationError, FieldsNotInitializedInConstructorError,
    GenericTypeResolvedToOutsideScopeError, GenericTypesDeclarationInsideConstructorFoundError,
    IdentifierNotFoundInAnyNamespaceError, IdentifierUsedBeforeInitializedError,
    InferredLambdaVariableTypeMismatchedWithTypeFromAnnotationError,
    InitMethodNotAllowedInsideConstructorError, InterfaceAlreadyExistInBoundsDeclarationError,
    InvalidLoopControlFlowStatementFoundError, MainFunctionNotFoundError,
    MainFunctionWrongTypeError, NonVoidConstructorReturnTypeError, SelfNotFoundError,
};
use crate::error::error::JarvilProgramAnalysisErrors;
use crate::error::helper::IdentifierKind;
use crate::scope::concrete::{MethodGenericsInstantiationContext, TurbofishTypes};
use crate::scope::errors::GenericTypeArgsCheckError;
use crate::scope::lookup::{LookupData, LookupResult};
use crate::scope::mangled::MangledIdentifierName;
use crate::scope::namespace::Namespace;
use crate::scope::scope::ScopeIndex;
use crate::scope::semantic_db::SemanticStateDatabase;
use crate::scope::symbol::core::{
    ConcreteSymbolDataEntry, IdentDeclId, SymbolDataEntry, SymbolIndex,
};
use crate::scope::symbol::function::FunctionSymbolData;
use crate::scope::symbol::function::{CallableKind, CallablePrototypeData};
use crate::scope::symbol::interfaces::{InterfaceBounds, InterfaceObject};
use crate::scope::symbol::interfaces::{InterfaceData, InterfaceSymbolData};
use crate::scope::symbol::types::core::UserDefinedTypeSymbolData;
use crate::scope::symbol::types::core::{UserDefineTypeKind, UserDefinedTypeData};
use crate::scope::symbol::types::generic_ty::GenericTypeDeclarationPlaceCategory;
use crate::scope::symbol::types::generic_ty::GenericTypeParams;
use crate::scope::symbol::variables::VariableSymbolData;
use crate::scope::traits::AbstractSymbol;
use crate::types::traits::TypeLike;
use crate::{
    ast::{
        ast::{
            ASTNode, BlockNode, CoreAtomStartNode, CoreStatementIndentWrapperNode,
            CoreStatementNode, StructDeclarationNode, TypeExpressionNode, TypeResolveKind,
            VariableDeclarationNode,
        },
        traits::Node,
        walk::Visitor,
    },
    error::diagnostics::{Diagnostics, IdentifierAlreadyDeclaredError, IdentifierNotDeclaredError},
    scope::{symbol::function::CallableData, symbol::variables::VariableData},
    types::core::Type,
};
use rustc_hash::{FxHashMap, FxHashSet};
use serde::Serialize;
use std::vec;
use text_size::TextRange;

#[derive(Debug)]
pub enum ResolveResult<T: AbstractSymbol> {
    Ok(LookupData<T>, Option<TurbofishTypes>),
    InvalidGenericTypeArgsProvided(GenericTypeArgsCheckError),
    NotInitialized(TextRange, IdentName),
    Unresolved,
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
#[derive(Default)]
pub enum BlockKind {
    #[default]
    Function,
    Lambda,
    LambdaType,
    Struct,
    Interface,
    Method,
    Conditional,
    Loop,
    Enum,
    Match,
    Case,
}


impl BlockKind {
    fn is_generics_shielding_block(&self) -> bool {
        match self {
            BlockKind::Function
            | BlockKind::Method
            | BlockKind::LambdaType
            | BlockKind::Struct
            | BlockKind::Interface
            | BlockKind::Lambda
            | BlockKind::Enum => true,
            BlockKind::Conditional | BlockKind::Loop | BlockKind::Match | BlockKind::Case => false,
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
            | BlockKind::Enum
            | BlockKind::Loop
            | BlockKind::Match
            | BlockKind::Case => false,
        }
    }
}

pub struct ClassContext {
    is_containing_self: bool,
}

pub struct BlockContext {
    inner_non_locals: FxHashSet<MangledIdentifierName<VariableData>>,
    block_kind: BlockKind,
    scope_index: ScopeIndex,
}

pub struct Context {
    class_context_stack: Vec<ClassContext>,
    block_context_stack: Vec<BlockContext>,
}

pub struct JarvilResolver<'ctx> {
    scope_index: ScopeIndex,
    code_handler: &'ctx JarvilCodeHandler<'ctx>,
    errors: &'ctx JarvilProgramAnalysisErrors,
    context: Context,
    indent_level: usize,
    semantic_db: SemanticStateDatabase,
}

impl<'ctx> JarvilResolver<'ctx> {
    pub fn new(
        code_handler: &'ctx JarvilCodeHandler<'ctx>,
        errors: &'ctx JarvilProgramAnalysisErrors,
    ) -> Self {
        JarvilResolver {
            scope_index: ScopeIndex::global(),
            code_handler,
            errors,
            context: Context {
                class_context_stack: vec![],
                block_context_stack: vec![BlockContext {
                    inner_non_locals: FxHashSet::default(),
                    block_kind: BlockKind::Function,
                    scope_index: ScopeIndex::global(),
                }],
            },
            indent_level: 0,
            semantic_db: SemanticStateDatabase::new(),
        }
    }

    pub fn code_handler(&self) -> &JarvilCodeHandler {
        self.code_handler
    }

    pub fn interner(&self) -> &Interner {
        self.semantic_db.interner()
    }

    pub fn resolve_ast(mut self, ast: &BlockNode) -> SemanticStateDatabase {
        let code_block = ast.0.as_ref();

        for stmt in &code_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }

        match self
            .semantic_db
            .namespace_ref()
            .funcs_ref()
            .get(self.scope_index, self.semantic_db.interner().intern("main"))
        {
            Some(symbol_index) => {
                let func_meta_data = self.semantic_db.func_symbol_ref(symbol_index);
                let prototype = func_meta_data.concretized_prototype(
                    self.semantic_db.namespace_ref(),
                    MethodGenericsInstantiationContext::default(),
                );
                let params = prototype.params();
                let return_ty = prototype.return_ty();

                if !params.is_empty() || !return_ty.is_void() {
                    let span =
                        symbol_index.decl_line_number(self.semantic_db.namespace_ref().funcs_ref());
                    let err = MainFunctionWrongTypeError::new(span);

                    self.errors
                        .log_error(Diagnostics::MainFunctionWrongType(err));
                }
            }
            None => {
                let err = MainFunctionNotFoundError::new();
                self.errors
                    .log_error(Diagnostics::MainFunctionNotFound(err));
            }
        }

        self.semantic_db
    }

    fn open_block(&mut self, block_kind: BlockKind) {
        let new_scope_index = self
            .semantic_db
            .namespace_mut_ref()
            .open_scope(self.scope_index, block_kind);

        self.scope_index = new_scope_index;
        self.indent_level += 1;
        self.context.block_context_stack.push(BlockContext {
            inner_non_locals: FxHashSet::default(),
            block_kind,
            scope_index: new_scope_index,
        });
    }

    fn close_block(&mut self, block: Option<&BlockNode>) {
        let parent_scope_index = match self
            .semantic_db
            .namespace_ref()
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
            self.semantic_db
                .set_non_locals(block, non_locals.inner_non_locals);
        }
    }

    fn set_curr_class_context_is_containing_self(&mut self, value: bool) {
        let len = self.context.class_context_stack.len();
        self.context.class_context_stack[len - 1].is_containing_self = value;
    }

    fn curr_class_context_is_containing_self(&self) -> bool {
        let len = self.context.class_context_stack.len();
        self.context.class_context_stack[len - 1].is_containing_self
    }

    fn set_to_variable_non_locals(
        &mut self,
        name: MangledIdentifierName<VariableData>,
        enclosing_func_scope_depth: Option<usize>,
    ) {
        let len = self.context.block_context_stack.len();

        if let Some(enclosing_func_scope_depth) = enclosing_func_scope_depth {
            self.context.block_context_stack[len - 1 - (enclosing_func_scope_depth - 1)]
                .inner_non_locals
                .insert(name);
        }
    }

    fn check_enclosing_loop_scope(&self) -> bool {
        let mut index = self.context.block_context_stack.len() - 1;

        loop {
            let block_kind = self.context.block_context_stack[index].block_kind;
            match block_kind {
                BlockKind::Loop => return true,
                BlockKind::Function | BlockKind::Lambda | BlockKind::Method => return false,
                BlockKind::Struct
                | BlockKind::Interface
                | BlockKind::LambdaType
                | BlockKind::Enum => unreachable!(),
                BlockKind::Conditional | BlockKind::Match | BlockKind::Case => {
                    index -= 1;
                }
            }
        }
    }

    fn enclosing_generics_declarative_scope_index(&self) -> (ScopeIndex, Option<ScopeIndex>) {
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

    fn bind_decl_to_identifier_in_decl(
        &mut self,
        node: &OkIdentifierInDeclNode,
        symbol_entry: SymbolDataEntry,
    ) {
        self.semantic_db
            .identifier_in_decl_binding_table_mut_ref()
            .insert(node.clone(), symbol_entry);
    }

    fn bind_decl_to_identifier_in_use<T: AbstractSymbol>(
        &mut self,
        node: &OkIdentifierInUseNode,
        symbol_obj: &T,
        is_concrete_types_none_allowed: bool,
    ) -> Result<Option<TurbofishTypes>, GenericTypeArgsCheckError> {
        let (concrete_types, ty_ranges) =
            self.extract_angle_bracket_content_from_identifier_in_use(node);

        symbol_obj.check_generic_ty_args(
            concrete_types.as_ref(),
            ty_ranges.as_ref(),
            is_concrete_types_none_allowed,
            self.semantic_db.err_logging_context(),
        )?;

        let concrete_symbol_entry =
            ConcreteSymbolDataEntry::new(symbol_obj.entry(), concrete_types.clone());

        self.semantic_db
            .identifier_in_use_binding_table_mut_ref()
            .insert(node.clone(), concrete_symbol_entry);

        Ok(concrete_types)
    }

    fn bind_decl_to_self_keyword(
        &mut self,
        node: &OkSelfKeywordNode,
        symbol_index: SymbolIndex<VariableData>,
    ) {
        self.semantic_db
            .self_keyword_binding_table_mut_ref()
            .insert(node.clone(), symbol_index);
    }

    fn try_resolving<
        T: AbstractSymbol,
        U: Fn(&Namespace, ScopeIndex, IdentName) -> LookupResult<T>,
    >(
        &mut self,
        identifier: &OkIdentifierInUseNode,
        lookup_fn: U,
        ident_kind: IdentifierKind,
        log_error: bool,
        is_concrete_types_none_allowed: bool,
    ) -> ResolveResult<T> {
        let name = identifier.token_value(self.code_handler, self.semantic_db.interner());

        match lookup_fn(self.semantic_db.namespace_ref(), self.scope_index, name) {
            LookupResult::Ok(lookup_data) => {
                match self.bind_decl_to_identifier_in_use(
                    identifier,
                    &lookup_data.symbol_obj,
                    is_concrete_types_none_allowed,
                ) {
                    Ok(concrete_types) => ResolveResult::Ok(lookup_data, concrete_types),
                    Err(err) => {
                        if log_error {
                            let err = err_for_generic_ty_args(
                                &err,
                                identifier.core_ref().name.range(),
                                ident_kind,
                            );

                            self.errors.log_error(err);
                        }

                        ResolveResult::InvalidGenericTypeArgsProvided(err)
                    }
                }
            }
            LookupResult::NotInitialized(decl_range) => {
                if log_error {
                    let err = IdentifierUsedBeforeInitializedError::new(
                        self.semantic_db.interner().lookup(name),
                        ident_kind,
                        decl_range,
                        identifier.range(),
                    );

                    self.errors
                        .log_error(Diagnostics::IdentifierUsedBeforeInitialized(err));
                }
                ResolveResult::NotInitialized(decl_range, name)
            }
            LookupResult::Unresolved => {
                if log_error {
                    let err = IdentifierNotDeclaredError::new(ident_kind, identifier.range());

                    self.errors
                        .log_error(Diagnostics::IdentifierNotDeclared(err));
                }
                ResolveResult::Unresolved
            }
        }
    }

    fn try_resolving_variable(
        &mut self,
        identifier: &OkIdentifierInUseNode,
        log_error: bool,
    ) -> ResolveResult<VariableSymbolData> {
        let lookup_fn = |namespace: &Namespace, scope_index: ScopeIndex, key: IdentName| {
            namespace.lookup_in_variables_namespace(scope_index, key)
        };

        self.try_resolving(
            identifier,
            lookup_fn,
            IdentifierKind::Variable,
            log_error,
            false,
        )
    }

    fn try_resolving_func(
        &mut self,
        identifier: &OkIdentifierInUseNode,
        log_error: bool,
    ) -> ResolveResult<FunctionSymbolData> {
        let lookup_fn = |namespace: &Namespace, scope_index: ScopeIndex, key: IdentName| {
            namespace.lookup_in_funcs_namespace(scope_index, key)
        };

        self.try_resolving(
            identifier,
            lookup_fn,
            IdentifierKind::Function,
            log_error,
            true,
        )
    }

    fn try_resolving_user_defined_ty(
        &mut self,
        identifier: &OkIdentifierInUseNode,
        log_error: bool,
        is_concrete_types_none_allowed: bool,
    ) -> ResolveResult<UserDefinedTypeSymbolData> {
        let lookup_fn = |namespace: &Namespace, scope_index: ScopeIndex, key: IdentName| {
            namespace.lookup_in_types_namespace(scope_index, key)
        };

        self.try_resolving(
            identifier,
            lookup_fn,
            IdentifierKind::UserDefinedType,
            log_error,
            is_concrete_types_none_allowed,
        )
    }

    fn try_resolving_interface(
        &mut self,
        identifier: &OkIdentifierInUseNode,
        log_error: bool,
    ) -> ResolveResult<InterfaceSymbolData> {
        let lookup_fn = |namespace: &Namespace, scope_index: ScopeIndex, key: IdentName| {
            namespace.lookup_in_interfaces_namespace(scope_index, key)
        };

        self.try_resolving(
            identifier,
            lookup_fn,
            IdentifierKind::Interface,
            log_error,
            false,
        )
    }

    fn try_resolving_self_keyword(
        &mut self,
        self_keyword: &OkSelfKeywordNode,
    ) -> Option<(SymbolIndex<VariableData>, usize)> {
        let name = self_keyword.token_value(self.code_handler, self.semantic_db.interner());
        debug_assert!(self.semantic_db.interner().lookup(name) == "self");

        match self
            .semantic_db
            .namespace_ref()
            .lookup_in_variables_namespace(self.scope_index, name)
        {
            LookupResult::Ok(lookup_data) => {
                let symbol_obj = lookup_data.symbol_obj;
                let depth = lookup_data.depth;

                self.bind_decl_to_self_keyword(self_keyword, symbol_obj.symbol_index());

                Some((symbol_obj.symbol_index(), depth))
            }
            LookupResult::NotInitialized(_) => unreachable!(),
            LookupResult::Unresolved => None,
        }
    }

    fn try_declare_and_bind<V, T, U>(
        &mut self,
        identifier: &OkIdentifierInDeclNode,
        declare_fn: U,
        unique_id: IdentDeclId<V>,
    ) -> Result<T, (IdentName, TextRange)>
    where
        T: AbstractSymbol,
        U: Fn(
            &mut Namespace,
            ScopeIndex,
            IdentName,
            TextRange,
            IdentDeclId<V>,
        ) -> Result<T, (IdentName, TextRange)>,
    {
        let name = identifier.token_value(self.code_handler, self.semantic_db.interner());
        let result = declare_fn(
            self.semantic_db.namespace_mut_ref(),
            self.scope_index,
            name,
            identifier.core_ref().name.range(),
            unique_id,
        );

        match result {
            Ok(symbol_obj) => {
                self.bind_decl_to_identifier_in_decl(identifier, symbol_obj.entry());

                Ok(symbol_obj)
            }
            Err(err) => Err(err),
        }
    }

    fn try_declare_and_bind_variable(
        &mut self,
        identifier: &OkIdentifierInDeclNode,
    ) -> Result<VariableSymbolData, (IdentName, TextRange)> {
        let declare_fn = |namespace: &mut Namespace,
                          scope_index: ScopeIndex,
                          name: IdentName,
                          decl_range: TextRange,
                          unique_id: IdentDeclId<VariableData>| {
            namespace.declare_variable(scope_index, name, decl_range, unique_id)
        };
        let unique_id = self
            .semantic_db
            .unique_key_generator_mut_ref()
            .generate_unique_id_for_variable();

        self.try_declare_and_bind(identifier, declare_fn, unique_id)
    }

    fn try_declare_and_bind_func(
        &mut self,
        identifier: &OkIdentifierInDeclNode,
    ) -> Result<FunctionSymbolData, (IdentName, TextRange)> {
        let declare_fn = |namespace: &mut Namespace,
                          scope_index: ScopeIndex,
                          name: IdentName,
                          decl_range: TextRange,
                          unique_id: IdentDeclId<CallableData>| {
            namespace.declare_func(scope_index, name, decl_range, unique_id)
        };
        let unique_id = self
            .semantic_db
            .unique_key_generator_mut_ref()
            .generate_unique_id_for_func();

        self.try_declare_and_bind(identifier, declare_fn, unique_id)
    }

    fn try_declare_and_bind_struct_ty(
        &mut self,
        identifier: &OkIdentifierInDeclNode,
    ) -> Result<UserDefinedTypeSymbolData, (IdentName, TextRange)> {
        let declare_fn = |namespace: &mut Namespace,
                          scope_index: ScopeIndex,
                          name: IdentName,
                          decl_range: TextRange,
                          unique_id: IdentDeclId<UserDefinedTypeData>| {
            namespace.declare_struct_ty(scope_index, name, decl_range, unique_id)
        };
        let unique_id = self
            .semantic_db
            .unique_key_generator_mut_ref()
            .generate_unique_id_for_ty();

        self.try_declare_and_bind(identifier, declare_fn, unique_id)
    }

    fn try_declare_and_bind_enum_ty(
        &mut self,
        identifier: &OkIdentifierInDeclNode,
    ) -> Result<UserDefinedTypeSymbolData, (IdentName, TextRange)> {
        let declare_fn = |namespace: &mut Namespace,
                          scope_index: ScopeIndex,
                          name: IdentName,
                          decl_range: TextRange,
                          unique_id: IdentDeclId<UserDefinedTypeData>| {
            namespace.declare_enum_ty(scope_index, name, decl_range, unique_id)
        };
        let unique_id = self
            .semantic_db
            .unique_key_generator_mut_ref()
            .generate_unique_id_for_ty();

        self.try_declare_and_bind(identifier, declare_fn, unique_id)
    }

    fn try_declare_and_bind_interface(
        &mut self,
        identifier: &OkIdentifierInDeclNode,
    ) -> Result<InterfaceSymbolData, (IdentName, TextRange)> {
        let declare_fn = |namespace: &mut Namespace,
                          scope_index: ScopeIndex,
                          name: IdentName,
                          decl_range: TextRange,
                          unique_id: IdentDeclId<InterfaceData>| {
            namespace.declare_interface(scope_index, name, decl_range, unique_id)
        };
        let unique_id = self
            .semantic_db
            .unique_key_generator_mut_ref()
            .generate_unique_id_for_interface();

        self.try_declare_and_bind(identifier, declare_fn, unique_id)
    }

    pub fn ty_from_user_defined_ty_expr<'a>(
        &mut self,
        user_defined_ty_expr: &'a UserDefinedTypeNode,
        scope_index: ScopeIndex,
    ) -> TypeResolveKind<'a> {
        let CoreIdentifierInUseNode::Ok(ok_identifier) =
            user_defined_ty_expr.core_ref().name.core_ref()
        else {
            return TypeResolveKind::Invalid;
        };

        let name = ok_identifier.token_value(self.code_handler, self.semantic_db.interner());
        match self
            .semantic_db
            .namespace_ref()
            .lookup_in_types_namespace(scope_index, name)
        {
            LookupResult::Ok(lookup_data) => {
                let symbol_obj = lookup_data.symbol_obj;
                let resolved_scope_index = symbol_obj.0.scope_index();
                let ty_kind = self.semantic_db.ty_symbol_ref(symbol_obj.0).kind();

                let result = match ty_kind {
                    UserDefineTypeKind::Struct => {
                        match self.bind_decl_to_identifier_in_use(ok_identifier, &symbol_obj, false)
                        {
                            Ok(concrete_types) => TypeResolveKind::Resolved(Type::new_with_struct(
                                symbol_obj.0,
                                concrete_types,
                            )),
                            Err(err) => TypeResolveKind::Unresolved(vec![
                                UnresolvedIdentifier::InvalidGenericTypeArgsProvided(
                                    ok_identifier,
                                    err,
                                ),
                            ]),
                        }
                    }
                    UserDefineTypeKind::Enum => {
                        match self.bind_decl_to_identifier_in_use(ok_identifier, &symbol_obj, false)
                        {
                            Ok(concrete_types) => TypeResolveKind::Resolved(Type::new_with_enum(
                                symbol_obj.0,
                                concrete_types,
                            )),
                            Err(err) => TypeResolveKind::Unresolved(vec![
                                UnresolvedIdentifier::InvalidGenericTypeArgsProvided(
                                    ok_identifier,
                                    err,
                                ),
                            ]),
                        }
                    }
                    UserDefineTypeKind::Lambda => {
                        match self.bind_decl_to_identifier_in_use(ok_identifier, &symbol_obj, false)
                        {
                            Ok(concrete_types) => TypeResolveKind::Resolved(
                                Type::new_with_lambda_named(symbol_obj.0, concrete_types),
                            ),
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
                            self.enclosing_generics_declarative_scope_index();
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
                                    &symbol_obj,
                                    false,
                                ) {
                                    Ok(concrete_types) => {
                                        debug_assert!(concrete_types.is_none());
                                        TypeResolveKind::Resolved(Type::new_with_generic(
                                            symbol_obj.0,
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
                                    symbol_obj.0.decl_line_number(
                                        self.semantic_db.namespace_ref().types_ref(),
                                    ),
                                ),
                            ]),
                        }
                    }
                };

                result
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

    fn ty_from_expr(&mut self, ty_expr: &TypeExpressionNode) -> Type {
        let ty = match ty_expr.ty_before_resolved(self, self.scope_index) {
            TypeResolveKind::Resolved(ty) => ty,
            TypeResolveKind::Invalid => Type::new_with_unknown(),
            TypeResolveKind::Unresolved(unresolved) => {
                for unresolved_identifier in unresolved {
                    match unresolved_identifier {
                        UnresolvedIdentifier::Unresolved(identifier) => {
                            let err = IdentifierNotDeclaredError::new(
                                IdentifierKind::UserDefinedType,
                                identifier.range(),
                            );

                            self.errors
                                .log_error(Diagnostics::IdentifierNotDeclared(err));
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
                                .log_error(Diagnostics::GenericTypeResolvedToOutsideScope(err));
                        }
                        UnresolvedIdentifier::NotInitialized(identifier, decl_range) => {
                            let name = identifier
                                .token_value(self.code_handler, self.semantic_db.interner());
                            let err = IdentifierUsedBeforeInitializedError::new(
                                self.semantic_db.interner().lookup(name),
                                IdentifierKind::UserDefinedType,
                                decl_range,
                                identifier.range(),
                            );

                            self.errors
                                .log_error(Diagnostics::IdentifierUsedBeforeInitialized(err));
                        }
                        UnresolvedIdentifier::InvalidGenericTypeArgsProvided(identifier, err) => {
                            let err = err_for_generic_ty_args(
                                &err,
                                identifier.core_ref().name.range(),
                                IdentifierKind::UserDefinedType,
                            );

                            self.errors.log_error(err);
                        }
                    }
                }

                Type::new_with_unknown()
            }
        };

        self.semantic_db.set_ty_expr_obj_mapping(ty_expr, &ty);

        ty
    }

    fn interface_obj_from_expr(
        &mut self,
        interface_expr: &OkIdentifierInUseNode,
    ) -> Option<InterfaceObject> {
        match self.try_resolving_interface(interface_expr, true) {
            ResolveResult::Ok(lookup_data, concrete_types) => Some(InterfaceObject::new(
                lookup_data.symbol_obj.symbol_index(),
                concrete_types,
            )),
            _ => None,
        }
    }

    fn interface_bounds_from_iter(
        &mut self,
        iter: SymbolSeparatedSequenceIterator<IdentifierInUseNode>,
    ) -> InterfaceBounds {
        let mut interface_bounds = InterfaceBounds::default();

        for interface_expr in iter {
            if let CoreIdentifierInUseNode::Ok(interface_expr) = interface_expr.core_ref() {
                if let Some(interface_obj) = self.interface_obj_from_expr(interface_expr) {
                    if let Some(previous_decl_range) = interface_bounds.insert(
                        interface_obj,
                        interface_expr.range(),
                        self.semantic_db.namespace_ref(),
                    ) {
                        let name = interface_expr
                            .token_value(self.code_handler, self.semantic_db.interner());
                        let err = InterfaceAlreadyExistInBoundsDeclarationError::new(
                            self.semantic_db.interner().lookup(name),
                            previous_decl_range,
                            interface_expr.range(),
                        );

                        self.errors
                            .log_error(Diagnostics::InterfaceAlreadyExistInBoundsDeclaration(err));
                    }
                }
            }
        }

        interface_bounds
    }

    fn extract_angle_bracket_content_from_identifier_in_use(
        &mut self,
        ok_identifier_in_use: &OkIdentifierInUseNode,
    ) -> (Option<TurbofishTypes>, Option<Vec<TextRange>>) {
        let Some((_, generic_ty_args, _)) = &ok_identifier_in_use.core_ref().generic_ty_args else {
            return (None, None);
        };
        let mut concrete_types: Vec<Type> = vec![];
        let mut ty_ranges: Vec<TextRange> = vec![];

        for generic_ty_expr in generic_ty_args.iter() {
            let ty = self.ty_from_expr(generic_ty_expr);
            concrete_types.push(ty);
            ty_ranges.push(generic_ty_expr.range())
        }

        (Some(TurbofishTypes::new(concrete_types)), Some(ty_ranges))
    }

    fn declare_angle_bracket_content_from_identifier_in_decl(
        &mut self,
        ok_identifier_in_decl: &OkIdentifierInDeclNode,
        decl_place_category: GenericTypeDeclarationPlaceCategory,
    ) -> (Option<GenericTypeParams>, Option<TurbofishTypes>) {
        let Some((_, generic_ty_decls, _)) = &ok_identifier_in_decl.core_ref().generic_ty_decls
        else {
            return (None, None);
        };
        let mut generic_ty_params_vec: Vec<(IdentName, InterfaceBounds, TextRange)> = vec![];
        let mut concrete_types: Vec<Type> = vec![];

        for (index, generic_ty_decl) in generic_ty_decls.iter().enumerate() {
            let core_generic_ty_decl = generic_ty_decl.core_ref();
            let CoreIdentifierInDeclNode::Ok(ok_identifier_in_decl) =
                core_generic_ty_decl.generic_ty_name.core_ref()
            else {
                continue;
            };

            debug_assert!(ok_identifier_in_decl.core_ref().generic_ty_decls.is_none());

            let generic_ty_name =
                ok_identifier_in_decl.token_value(self.code_handler, self.semantic_db.interner());
            let mut interface_bounds = InterfaceBounds::default();

            if let Some((_, interface_bounds_node)) = &core_generic_ty_decl.interface_bounds {
                interface_bounds = self.interface_bounds_from_iter(interface_bounds_node.iter())
            }

            let unique_id = self
                .semantic_db
                .unique_key_generator_mut_ref()
                .generate_unique_id_for_ty();

            match self
                .semantic_db
                .namespace_mut_ref()
                .declare_generic_ty_with_meta_data(
                    self.scope_index,
                    generic_ty_name,
                    index,
                    decl_place_category,
                    &interface_bounds,
                    ok_identifier_in_decl.range(),
                    unique_id,
                ) {
                Ok(symbol_obj) => {
                    self.bind_decl_to_identifier_in_decl(ok_identifier_in_decl, symbol_obj.entry());
                    generic_ty_params_vec.push((
                        generic_ty_name,
                        interface_bounds,
                        ok_identifier_in_decl.range(),
                    ));
                    concrete_types.push(Type::new_with_generic(symbol_obj.0))
                }
                Err((param_name, previous_decl_range)) => {
                    let err = IdentifierAlreadyDeclaredError::new(
                        IdentifierKind::UserDefinedType,
                        self.semantic_db.interner().lookup(param_name),
                        previous_decl_range,
                        ok_identifier_in_decl.range(),
                    );
                    self.errors
                        .log_error(Diagnostics::IdentifierAlreadyDeclared(err));
                }
            }
        }

        (
            Some(GenericTypeParams::new(generic_ty_params_vec)),
            Some(TurbofishTypes::new(concrete_types)),
        )
    }

    fn is_already_a_method(
        methods: &FxHashMap<IdentName, (CallableData, TextRange)>,
        class_methods: &FxHashMap<IdentName, (CallableData, TextRange)>,
        name: &IdentName,
    ) -> Option<TextRange> {
        match methods.get(name) {
            Some((_, previous_decl_range)) => Some(*previous_decl_range),
            None => class_methods
                .get(name)
                .map(|(_, previous_decl_range)| *previous_decl_range),
        }
    }

    fn declare_callable_prototype(
        &mut self,
        callable_prototype: &CallablePrototypeNode,
        optional_identifier_in_decl: Option<&OkIdentifierInDeclNode>,
    ) -> (
        Vec<Type>,
        Type,
        Option<TextRange>,
        Option<GenericTypeParams>,
    ) {
        let generic_ty_decls = match optional_identifier_in_decl {
            Some(ok_identifier) => {
                self.declare_angle_bracket_content_from_identifier_in_decl(
                    ok_identifier,
                    GenericTypeDeclarationPlaceCategory::InCallable,
                )
                .0
            }
            None => None,
        };

        let core_callable_prototype = callable_prototype.core_ref();
        let params = &core_callable_prototype.params;
        let return_ty = &core_callable_prototype.return_ty;
        let mut param_types_vec = vec![];
        let mut return_ty_range = None;

        let return_ty = match return_ty {
            Some((_, return_ty_expr)) => {
                return_ty_range = Some(return_ty_expr.range());
                self.ty_from_expr(return_ty_expr)
            }
            None => Type::new_with_void(),
        };

        if let Some(params) = params {
            let params_iter = params.iter();

            for param in params_iter {
                let core_param = param.core_ref();
                let param_name = &core_param.name;
                let CoreIdentifierInDeclNode::Ok(ok_identifier) = param_name.core_ref() else {
                    continue;
                };
                let param_name =
                    ok_identifier.token_value(self.code_handler, self.semantic_db.interner());
                let param_ty = self.ty_from_expr(&core_param.data_ty);
                let unique_id = self
                    .semantic_db
                    .unique_key_generator_mut_ref()
                    .generate_unique_id_for_variable();
                let result = self
                    .semantic_db
                    .namespace_mut_ref()
                    .declare_variable_with_ty(
                        self.scope_index,
                        param_name,
                        &param_ty,
                        ok_identifier.range(),
                        true,
                        unique_id,
                    );

                match result {
                    Ok(symbol_obj) => {
                        self.bind_decl_to_identifier_in_decl(ok_identifier, symbol_obj.entry());
                        param_types_vec.push(param_ty);
                    }
                    Err((param_name, previous_decl_range)) => {
                        let err = IdentifierAlreadyDeclaredError::new(
                            IdentifierKind::Variable,
                            self.semantic_db.interner().lookup(param_name),
                            previous_decl_range,
                            ok_identifier.range(),
                        );
                        self.errors
                            .log_error(Diagnostics::IdentifierAlreadyDeclared(err));
                    }
                }
            }
        }

        (
            param_types_vec,
            return_ty,
            return_ty_range,
            generic_ty_decls,
        )
    }

    fn resolve_callable_body(
        &mut self,
        symbol_obj: Option<&FunctionSymbolData>,
        optional_identifier_in_decl: Option<&OkIdentifierInDeclNode>,
        callable_body: Option<&BlockNode>,
        callable_prototype: &CallablePrototypeNode,
    ) -> (Vec<Type>, Type, Option<TextRange>) {
        self.open_block(if let Some(callable_body) = callable_body {
            callable_body.core_ref().kind
        } else {
            BlockKind::default() // `Function`: function declarations inside `.d.jv` files won't have body
        });

        let (param_types_vec, return_ty, return_ty_range, generic_ty_decls) =
            self.declare_callable_prototype(callable_prototype, optional_identifier_in_decl);

        if let Some(symbol_obj) = symbol_obj {
            self.semantic_db
                .func_symbol_mut_ref(symbol_obj.symbol_index())
                .set_generics(generic_ty_decls);
        }

        if let Some(callable_body) = callable_body {
            for stmt in &callable_body.0.as_ref().stmts {
                self.walk_stmt_indent_wrapper(stmt);
            }
        }

        self.close_block(callable_body);

        (param_types_vec, return_ty, return_ty_range)
    }

    fn resolve_method_body(
        &mut self,
        callable_body: &CallableBodyNode,
        optional_identifier_in_decl: Option<&OkIdentifierInDeclNode>,
    ) -> (
        Vec<Type>,
        Type,
        Option<TextRange>,
        Option<GenericTypeParams>,
    ) {
        let core_callable_body = callable_body.core_ref();
        let callable_body = &core_callable_body.block;

        self.open_block(callable_body.core_ref().kind);

        let (param_types_vec, return_ty, return_ty_range, generic_ty_decls) = self
            .declare_callable_prototype(&core_callable_body.prototype, optional_identifier_in_decl);

        for stmt in &callable_body.0.as_ref().stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }

        self.close_block(Some(callable_body));

        (
            param_types_vec,
            return_ty,
            return_ty_range,
            generic_ty_decls,
        )
    }

    fn resolve_constructor_body(
        &mut self,
        callable_body: &CallableBodyNode,
    ) -> (Vec<Type>, Type, Option<TextRange>, FxHashSet<IdentName>) {
        let core_callable_body = callable_body.core_ref();
        let mut initialized_fields: FxHashSet<IdentName> = FxHashSet::default();
        let callable_body = &core_callable_body.block;

        self.open_block(callable_body.core_ref().kind);

        let (param_types_vec, return_ty, return_ty_range, _) =
            self.declare_callable_prototype(&core_callable_body.prototype, None);

        for stmt in &callable_body.0.as_ref().stmts {
            let stmt = match stmt.core_ref() {
                CoreStatementIndentWrapperNode::CorrectlyIndented(stmt) => stmt,
                CoreStatementIndentWrapperNode::IncorrectlyIndented(stmt) => {
                    let core_stmt = stmt.core_ref();
                    &core_stmt.stmt
                }
                _ => continue,
            };
            self.walk_stmt(stmt);

            // collect for all the assignment statements with format: `self.<PROPERTY_NAME>`
            // in order to check which all fields are not initialized inside the constructor
            if let CoreStatementNode::Assignment(assignment) = stmt.core_ref() {
                if let CoreAssignmentNode::Ok(ok_assignment) = assignment.core_ref() {
                    if let CoreAtomNode::PropertyAccess(property_access) =
                        ok_assignment.core_ref().l_atom.core_ref()
                    {
                        if let CoreIdentifierInUseNode::Ok(property_name) =
                            property_access.core_ref().propertry.core_ref()
                        {
                            if let CoreAtomNode::AtomStart(atom_start) =
                                property_access.core_ref().atom.core_ref()
                            {
                                if let CoreAtomStartNode::SelfKeyword(_) = atom_start.core_ref() {
                                    let property_name_str = property_name.token_value(
                                        self.code_handler,
                                        self.semantic_db.interner(),
                                    );
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
            return_ty,
            return_ty_range,
            initialized_fields,
        )
    }

    fn declare_variable(&mut self, variable_decl: &VariableDeclarationNode) {
        let core_variable_decl = variable_decl.core_ref();
        let mut symbol_obj: Option<VariableSymbolData> = None;

        if let CoreIdentifierInDeclNode::Ok(ok_identifier) = core_variable_decl.name.core_ref() {
            match self.try_declare_and_bind_variable(ok_identifier) {
                Ok(local_symbol_obj) => {
                    symbol_obj = Some(local_symbol_obj);
                }
                Err((name, previous_decl_range)) => {
                    let err = IdentifierAlreadyDeclaredError::new(
                        IdentifierKind::Variable,
                        self.semantic_db.interner().lookup(name),
                        previous_decl_range,
                        ok_identifier.range(),
                    );
                    self.errors
                        .log_error(Diagnostics::IdentifierAlreadyDeclared(err));
                }
            }
        }

        let ty_from_optional_annotation = core_variable_decl
            .ty_annotation
            .as_ref()
            .map(|(_, ty_expr)| (self.ty_from_expr(ty_expr), ty_expr.range()));

        // Except `CoreRAssignmentNode::LAMBDA`, type of the variable is set in the `type_checker.rs`. For `CoreRAssignmentNode::LAMBDA`,
        // it's type is set to the variable symbol_data here itself.
        match core_variable_decl.r_node.core_ref() {
            CoreRVariableDeclarationNode::Lambda(lambda_r_assign) => {
                // For case of lambda variable, it is allowed to be referenced inside the body to
                // enable recursive definitions
                if let Some(symbol_obj) = &symbol_obj {
                    self.semantic_db
                        .variable_symbol_mut_ref(symbol_obj.symbol_index())
                        .set_is_init(true);
                }

                let core_lambda_r_assign = lambda_r_assign.core_ref();
                let body = &core_lambda_r_assign.body.core_ref().block;
                let prototype = &core_lambda_r_assign.body.core_ref().prototype;
                let (params_vec, return_ty, _) =
                    self.resolve_callable_body(None, None, Some(body), prototype);
                let lambda_ty = Type::new_with_lambda_unnamed(CallablePrototypeData::new(
                    params_vec, return_ty,
                ));
                let final_variable_ty = match ty_from_optional_annotation {
                    Some((ty_from_optional_annotation, range)) => {
                        if !ty_from_optional_annotation
                            .is_eq(&lambda_ty, self.semantic_db.namespace_ref())
                        {
                            let err = InferredLambdaVariableTypeMismatchedWithTypeFromAnnotationError::new(
                                ty_from_optional_annotation.to_string(self.semantic_db.err_logging_context()),
                                lambda_ty.to_string(self.semantic_db.err_logging_context()),
                                range
                            );

                            self.errors.log_error(Diagnostics::InferredLambdaVariableTypeMismatchedWithTypeFromAnnotation(err));
                        }

                        ty_from_optional_annotation
                    }
                    None => lambda_ty,
                };

                if let Some(symbol_obj) = &symbol_obj {
                    self.semantic_db
                        .variable_symbol_mut_ref(symbol_obj.symbol_index())
                        .set_data_ty(&final_variable_ty);
                }
            }
            CoreRVariableDeclarationNode::Expression(expr_r_assign) => {
                self.walk_expr_stmt(expr_r_assign);

                if let Some(symbol_obj) = &symbol_obj {
                    match ty_from_optional_annotation {
                        Some((ty, _)) => self
                            .semantic_db
                            .variable_symbol_mut_ref(symbol_obj.symbol_index())
                            .set_data_ty_from_optional_annotation(ty),
                        None => {
                            self.semantic_db
                                .variable_symbol_mut_ref(symbol_obj.symbol_index())
                                .set_is_init(true);
                        }
                    }
                }
            }
        }
    }

    fn declare_func(&mut self, func_wrapper: &FunctionWrapperNode) {
        let core_func_decl = func_wrapper.core_ref().func_decl.core_ref();
        let func_name = &core_func_decl.name;
        let body = &core_func_decl.body.core_ref().block;
        let prototype = &core_func_decl.body.core_ref().prototype;
        let mut optional_ok_identifier_node = None;
        let mut symbol_obj: Option<FunctionSymbolData> = None;

        if let CoreIdentifierInDeclNode::Ok(ok_identifier) = func_name.core_ref() {
            optional_ok_identifier_node = Some(ok_identifier);

            match self.try_declare_and_bind_func(ok_identifier) {
                Ok(local_symbol_obj) => symbol_obj = Some(local_symbol_obj),
                Err((name, previous_decl_range)) => {
                    let err = IdentifierAlreadyDeclaredError::new(
                        IdentifierKind::Function,
                        self.semantic_db.interner().lookup(name),
                        previous_decl_range,
                        ok_identifier.range(),
                    );

                    self.errors
                        .log_error(Diagnostics::IdentifierAlreadyDeclared(err));
                }
            }
        }

        let (param_types_vec, return_ty, _) = self.resolve_callable_body(
            symbol_obj.as_ref(),
            optional_ok_identifier_node,
            Some(body),
            prototype,
        );

        if let Some(symbol_obj) = &symbol_obj {
            self.semantic_db
                .func_symbol_mut_ref(symbol_obj.symbol_index())
                .set_meta_data(param_types_vec, return_ty, CallableKind::Function);
        }
    }

    fn declare_struct_ty(&mut self, struct_decl: &StructDeclarationNode) {
        self.context.class_context_stack.push(ClassContext {
            is_containing_self: false,
        });

        let core_struct_decl = struct_decl.core_ref();
        let struct_body = &core_struct_decl.block;
        let implementing_interfaces_node = &core_struct_decl.implementing_interfaces;
        let mut optional_ok_identifier_node = None;
        let mut symbol_obj: Option<UserDefinedTypeSymbolData> = None;

        if let CoreIdentifierInDeclNode::Ok(ok_identifier) = core_struct_decl.name.core_ref() {
            optional_ok_identifier_node = Some(ok_identifier);

            match self.try_declare_and_bind_struct_ty(ok_identifier) {
                Ok(local_symbol_obj) => {
                    symbol_obj = Some(local_symbol_obj);
                }
                Err((name, previous_decl_range)) => {
                    let err = IdentifierAlreadyDeclaredError::new(
                        IdentifierKind::UserDefinedType,
                        self.semantic_db.interner().lookup(name),
                        previous_decl_range,
                        ok_identifier.range(),
                    );

                    self.errors
                        .log_error(Diagnostics::IdentifierAlreadyDeclared(err));
                }
            }
        };

        self.open_block(struct_body.core_ref().kind);

        let (struct_generic_ty_decls, struct_ty) = match optional_ok_identifier_node {
            Some(ok_identifier) => {
                let (struct_generic_ty_decls, concrete_types) = self
                    .declare_angle_bracket_content_from_identifier_in_decl(
                        ok_identifier,
                        GenericTypeDeclarationPlaceCategory::InType,
                    );
                let struct_ty = match &symbol_obj {
                    Some(symbol_obj) => Type::new_with_struct(symbol_obj.0, concrete_types),
                    None => Type::new_with_unknown(),
                };

                (struct_generic_ty_decls, struct_ty)
            }
            None => (None, Type::new_with_unknown()),
        };

        let unique_id = self
            .semantic_db
            .unique_key_generator_mut_ref()
            .generate_unique_id_for_variable();
        let self_str_id = self.semantic_db.interner().intern("self");
        let result = self
            .semantic_db
            .namespace_mut_ref()
            .declare_variable_with_ty(
                self.scope_index,
                self_str_id,
                &struct_ty,
                core_struct_decl.name.range(),
                true,
                unique_id,
            );

        debug_assert!(result.is_ok());

        let mut implementing_interfaces: Option<InterfaceBounds> = None;

        if let Some((_, interfaces_node)) = implementing_interfaces_node {
            let interface_bounds = self.interface_bounds_from_iter(interfaces_node.iter());

            if interface_bounds.len() > 0 {
                implementing_interfaces = Some(interface_bounds);
            }
        }

        if let Some(symbol_obj) = &symbol_obj {
            self.semantic_db
                .ty_symbol_mut_ref(symbol_obj.0)
                .struct_data_mut_ref()
                .set_generics_and_interfaces(struct_generic_ty_decls, implementing_interfaces);
        }

        let mut fields_map: FxHashMap<IdentName, (Type, TextRange)> = FxHashMap::default();
        let mut constructor: Option<(CallableData, TextRange)> = None;
        let mut methods: FxHashMap<IdentName, (CallableData, TextRange)> = FxHashMap::default();
        let mut class_methods: FxHashMap<IdentName, (CallableData, TextRange)> =
            FxHashMap::default();
        let mut initialized_fields: FxHashSet<IdentName> = FxHashSet::default();

        for stmt in &struct_body.0.as_ref().stmts {
            let stmt = match stmt.core_ref() {
                CoreStatementIndentWrapperNode::CorrectlyIndented(stmt) => stmt,
                CoreStatementIndentWrapperNode::IncorrectlyIndented(stmt) => &stmt.core_ref().stmt,
                _ => continue,
            };
            match stmt.core_ref() {
                CoreStatementNode::StructPropertyDeclaration(struct_property_decl) => {
                    let core_struct_property_decl = struct_property_decl.core_ref();
                    let name_ty_spec = core_struct_property_decl.name_ty_spec.core_ref();
                    let name = &name_ty_spec.name;

                    if let CoreIdentifierInDeclNode::Ok(ok_identifier) = name.core_ref() {
                        let field_name = ok_identifier
                            .token_value(self.code_handler, self.semantic_db.interner());
                        let ty = self.ty_from_expr(&name_ty_spec.data_ty);

                        match fields_map.get(&field_name) {
                            Some((_, previous_decl_range)) => {
                                let err = IdentifierAlreadyDeclaredError::new(
                                    IdentifierKind::Field,
                                    self.semantic_db.interner().lookup(field_name),
                                    *previous_decl_range,
                                    ok_identifier.range(),
                                );

                                self.errors
                                    .log_error(Diagnostics::IdentifierAlreadyDeclared(err));
                            }
                            None => {
                                fields_map.insert(field_name, (ty, ok_identifier.range()));
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
                        let method_name_str = ok_bounded_method_name
                            .token_value(self.code_handler, self.semantic_db.interner());

                        if method_name_str == self.semantic_db.interner().intern("__init__")
                            && constructor.is_none()
                        {
                            is_constructor = true;

                            if let Some((_, generic_ty_decls, _)) =
                                &ok_bounded_method_name.core_ref().generic_ty_decls
                            {
                                let err = GenericTypesDeclarationInsideConstructorFoundError::new(
                                    generic_ty_decls.range(),
                                );

                                self.errors.log_error(
                                    Diagnostics::GenericTypesDeclarationInsideConstructorFound(err),
                                );
                            }
                        }
                    }

                    let (param_types_vec, return_ty, return_ty_range, method_generic_ty_decls) =
                        if is_constructor {
                            let (
                                param_types_vec,
                                return_ty,
                                return_ty_range,
                                temp_initialized_fields,
                            ) = self.resolve_constructor_body(&core_func_decl.body);

                            initialized_fields = temp_initialized_fields;

                            (param_types_vec, return_ty, return_ty_range, None)
                        } else {
                            self.resolve_method_body(
                                &core_func_decl.body,
                                optional_ok_identifier_node,
                            )
                        };

                    if let CoreIdentifierInDeclNode::Ok(ok_bounded_method_name) =
                        core_func_decl.name.core_ref()
                    {
                        let method_meta_data = CallableData::new(
                            param_types_vec,
                            return_ty,
                            CallableKind::Method,
                            method_generic_ty_decls,
                        );
                        let method_name_str = ok_bounded_method_name
                            .token_value(self.code_handler, self.semantic_db.interner());

                        if method_name_str == self.semantic_db.interner().intern("__init__") {
                            match constructor {
                                Some((_, previous_decl_range)) => {
                                    let err = IdentifierAlreadyDeclaredError::new(
                                        IdentifierKind::Constructor,
                                        self.semantic_db.interner().lookup(method_name_str),
                                        previous_decl_range,
                                        ok_bounded_method_name.range(),
                                    );

                                    self.errors
                                        .log_error(Diagnostics::IdentifierAlreadyDeclared(err));
                                }
                                None => {
                                    if let Some(return_ty_range) = return_ty_range {
                                        let err =
                                            NonVoidConstructorReturnTypeError::new(return_ty_range);

                                        self.errors.log_error(
                                            Diagnostics::NonVoidConstructorReturnType(err),
                                        );
                                    }

                                    constructor =
                                        Some((method_meta_data, ok_bounded_method_name.range()));

                                    self.semantic_db.set_bounded_kind(
                                        bounded_method_wrapper,
                                        BoundedMethodKind::Constructor,
                                    );
                                }
                            }
                        } else {
                            match JarvilResolver::is_already_a_method(
                                &methods,
                                &class_methods,
                                &method_name_str,
                            ) {
                                Some(previous_decl_range) => {
                                    let err = IdentifierAlreadyDeclaredError::new(
                                        IdentifierKind::Method,
                                        self.semantic_db.interner().lookup(method_name_str),
                                        previous_decl_range,
                                        ok_bounded_method_name.range(),
                                    );

                                    self.errors
                                        .log_error(Diagnostics::IdentifierAlreadyDeclared(err));
                                }
                                None => {
                                    let is_containing_self =
                                        self.curr_class_context_is_containing_self();

                                    if is_containing_self {
                                        methods.insert(
                                            method_name_str,
                                            (method_meta_data, ok_bounded_method_name.range()),
                                        );

                                        self.semantic_db.set_bounded_kind(
                                            bounded_method_wrapper,
                                            BoundedMethodKind::Method,
                                        );
                                    } else {
                                        class_methods.insert(
                                            method_name_str,
                                            (method_meta_data, ok_bounded_method_name.range()),
                                        );

                                        self.semantic_db.set_bounded_kind(
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
                    let mut missing_fields_from_constructor: Vec<&IdentName> = vec![];

                    for (field_name, _) in fields_map.iter() {
                        if initialized_fields.get(field_name).is_none() {
                            missing_fields_from_constructor.push(field_name);
                        }
                    }

                    if !missing_fields_from_constructor.is_empty() {
                        let err = FieldsNotInitializedInConstructorError::new(
                            missing_fields_from_constructor,
                            construct_span,
                            self.semantic_db.interner(),
                        );

                        self.errors
                            .log_error(Diagnostics::FieldsNotInitializedInConstructor(err));
                    }
                }
                None => {
                    let err =
                        ConstructorNotFoundInsideStructDeclarationError::new(ok_identifier.range());

                    self.errors
                        .log_error(Diagnostics::ConstructorNotFoundInsideStructDeclaration(err));
                }
            }

            if let Some(symbol_obj) = &symbol_obj {
                self.semantic_db
                    .ty_symbol_mut_ref(symbol_obj.0)
                    .struct_data_mut_ref()
                    .set_meta_data(fields_map, constructor, methods, class_methods);
            }
        }

        self.context.class_context_stack.pop();
    }

    fn declare_enum_ty(&mut self, enum_ty_decl: &EnumDeclarationNode) {
        let core_enum_ty_decl = enum_ty_decl.core_ref();
        let name = &core_enum_ty_decl.name;
        let mut optional_ok_identifier_in_decl = None;
        let mut symbol_obj: Option<UserDefinedTypeSymbolData> = None;
        let enum_body = &core_enum_ty_decl.block;

        if let CoreIdentifierInDeclNode::Ok(ok_identifier_in_decl) = name.core_ref() {
            optional_ok_identifier_in_decl = Some(ok_identifier_in_decl);

            match self.try_declare_and_bind_enum_ty(ok_identifier_in_decl) {
                Ok(local_symbol_obj) => symbol_obj = Some(local_symbol_obj),
                Err((name, previous_decl_range)) => {
                    let err = IdentifierAlreadyDeclaredError::new(
                        IdentifierKind::UserDefinedType,
                        self.semantic_db.interner().lookup(name),
                        previous_decl_range,
                        ok_identifier_in_decl.range(),
                    );

                    self.errors
                        .log_error(Diagnostics::IdentifierAlreadyDeclared(err));
                }
            }
        }

        self.open_block(enum_body.core_ref().kind);

        let generic_ty_decls = match optional_ok_identifier_in_decl {
            Some(ok_identifier) => {
                self.declare_angle_bracket_content_from_identifier_in_decl(
                    ok_identifier,
                    GenericTypeDeclarationPlaceCategory::InType,
                )
                .0
            }
            None => None,
        };

        if let Some(symbol_obj) = &symbol_obj {
            self.semantic_db
                .ty_symbol_mut_ref(symbol_obj.0)
                .enum_data_mut_ref()
                .set_generics(generic_ty_decls);
        }

        let mut variants: Vec<(IdentName, Option<Type>, TextRange)> = vec![];
        let mut variants_map: FxHashMap<IdentName, TextRange> = FxHashMap::default();

        for stmt in &enum_body.0.as_ref().stmts {
            let stmt = match stmt.core_ref() {
                CoreStatementIndentWrapperNode::CorrectlyIndented(stmt) => stmt,
                CoreStatementIndentWrapperNode::IncorrectlyIndented(stmt) => &stmt.core_ref().stmt,
                _ => continue,
            };
            let enum_variant_decl = match stmt.core_ref() {
                CoreStatementNode::EnumVariantDeclaration(enum_variant_decl) => enum_variant_decl,
                _ => unreachable!(),
            };
            let core_enum_variant_decl = enum_variant_decl.core_ref();
            let variant_name = &core_enum_variant_decl.variant;
            let ty = &core_enum_variant_decl.ty;
            let mut variant_ty: Option<Type> = None;
            let CoreIdentifierInDeclNode::Ok(ok_identifier) = variant_name.core_ref() else {
                continue;
            };
            let variant_name =
                ok_identifier.token_value(self.code_handler, self.semantic_db.interner());

            if let Some((_, ty_expr, _)) = ty {
                variant_ty = Some(self.ty_from_expr(ty_expr));
            }

            match variants_map.get(&variant_name) {
                Some(previous_decl_range) => {
                    let err = IdentifierAlreadyDeclaredError::new(
                        IdentifierKind::Variant,
                        self.semantic_db.interner().lookup(variant_name),
                        *previous_decl_range,
                        ok_identifier.range(),
                    );

                    self.errors
                        .log_error(Diagnostics::IdentifierAlreadyDeclared(err));
                }
                None => {
                    variants.push((variant_name, variant_ty, ok_identifier.range()));
                    variants_map.insert(variant_name, ok_identifier.range());
                }
            }
        }

        self.close_block(Some(enum_body));

        if let Some(symbol_obj) = &symbol_obj {
            self.semantic_db
                .ty_symbol_mut_ref(symbol_obj.0)
                .enum_data_mut_ref()
                .set_meta_data(variants);
        }
    }

    fn declare_lambda_ty(&mut self, lambda_ty_decl: &LambdaTypeDeclarationNode) {
        let core_lambda_ty_decl = lambda_ty_decl.core_ref();
        let mut types_vec: Vec<Type> = vec![];
        let ty_tuple = &core_lambda_ty_decl.ty_tuple;
        let return_ty = &core_lambda_ty_decl.return_ty;
        let mut optional_ok_identifier_node: Option<&OkIdentifierInDeclNode> = None;
        let mut generic_ty_decls: Option<GenericTypeParams> = None;

        self.open_block(BlockKind::LambdaType);

        if let CoreIdentifierInDeclNode::Ok(ok_identifier) = core_lambda_ty_decl.name.core_ref() {
            optional_ok_identifier_node = Some(ok_identifier);
            generic_ty_decls = self
                .declare_angle_bracket_content_from_identifier_in_decl(
                    ok_identifier,
                    GenericTypeDeclarationPlaceCategory::InType,
                )
                .0;
        }

        let return_ty: Type = match return_ty {
            Some((_, return_ty_expr)) => self.ty_from_expr(return_ty_expr),
            None => Type::new_with_void(),
        };

        if let Some(ty_tuple) = ty_tuple {
            let ty_tuple_iter = ty_tuple.iter();

            for data_ty in ty_tuple_iter {
                let ty = self.ty_from_expr(data_ty);
                types_vec.push(ty);
            }
        }

        self.close_block(None);

        let Some(ok_identifier) = optional_ok_identifier_node else {
            return;
        };
        let name = ok_identifier.token_value(self.code_handler, self.semantic_db.interner());
        let unique_id = self
            .semantic_db
            .unique_key_generator_mut_ref()
            .generate_unique_id_for_ty();
        let result = self
            .semantic_db
            .namespace_mut_ref()
            .declare_lambda_ty_with_meta_data(
                self.scope_index,
                name,
                types_vec,
                return_ty,
                generic_ty_decls,
                ok_identifier.core_ref().name.range(),
                unique_id,
            );

        match result {
            Ok(symbol_obj) => {
                self.bind_decl_to_identifier_in_decl(ok_identifier, symbol_obj.entry());
            }
            Err((name, previous_decl_range)) => {
                let err = IdentifierAlreadyDeclaredError::new(
                    IdentifierKind::UserDefinedType,
                    self.semantic_db.interner().lookup(name),
                    previous_decl_range,
                    ok_identifier.range(),
                );

                self.errors
                    .log_error(Diagnostics::IdentifierAlreadyDeclared(err));
            }
        }
    }

    fn declare_interface(&mut self, interface_decl: &InterfaceDeclarationNode) {
        let core_interface_decl = interface_decl.core_ref();
        let name = &core_interface_decl.name;
        let mut optional_ok_identifier_in_decl = None;
        let mut symbol_obj: Option<InterfaceSymbolData> = None;
        let interface_body = &core_interface_decl.block;

        if let CoreIdentifierInDeclNode::Ok(ok_identifier_in_decl) = name.core_ref() {
            optional_ok_identifier_in_decl = Some(ok_identifier_in_decl);

            match self.try_declare_and_bind_interface(ok_identifier_in_decl) {
                Ok(local_symbol_obj) => symbol_obj = Some(local_symbol_obj),
                Err((name, previous_decl_range)) => {
                    let err = IdentifierAlreadyDeclaredError::new(
                        IdentifierKind::Interface,
                        self.semantic_db.interner().lookup(name),
                        previous_decl_range,
                        ok_identifier_in_decl.range(),
                    );

                    self.errors
                        .log_error(Diagnostics::IdentifierAlreadyDeclared(err));
                }
            }
        }

        self.open_block(interface_body.core_ref().kind);

        let generic_ty_decls = match optional_ok_identifier_in_decl {
            Some(ok_identifier) => {
                self.declare_angle_bracket_content_from_identifier_in_decl(
                    ok_identifier,
                    GenericTypeDeclarationPlaceCategory::InType,
                )
                .0
            }
            None => None,
        };

        if let Some(symbol_obj) = &symbol_obj {
            self.semantic_db
                .interface_symbol_mut_ref(symbol_obj.symbol_index())
                .set_generics(generic_ty_decls);
        }

        let mut fields_map: FxHashMap<IdentName, (Type, TextRange)> = FxHashMap::default();
        let mut methods: FxHashMap<IdentName, (CallableData, TextRange)> = FxHashMap::default();

        for stmt in &interface_body.0.as_ref().stmts {
            let stmt = match stmt.core_ref() {
                CoreStatementIndentWrapperNode::CorrectlyIndented(stmt) => stmt,
                CoreStatementIndentWrapperNode::IncorrectlyIndented(stmt) => &stmt.core_ref().stmt,
                _ => continue,
            };

            match stmt.core_ref() {
                CoreStatementNode::StructPropertyDeclaration(interface_property_decl) => {
                    let core_interface_property_decl = interface_property_decl.core_ref();
                    let name_ty_spec = core_interface_property_decl.name_ty_spec.core_ref();
                    let name = &name_ty_spec.name;

                    if let CoreIdentifierInDeclNode::Ok(ok_identifier) = name.core_ref() {
                        let field_name = ok_identifier
                            .token_value(self.code_handler, self.semantic_db.interner());
                        let ty = self.ty_from_expr(&name_ty_spec.data_ty);

                        match fields_map.get(&field_name) {
                            Some((_, previous_decl_range)) => {
                                let err = IdentifierAlreadyDeclaredError::new(
                                    IdentifierKind::Field,
                                    self.semantic_db.interner().lookup(field_name),
                                    *previous_decl_range,
                                    ok_identifier.range(),
                                );

                                self.errors
                                    .log_error(Diagnostics::IdentifierAlreadyDeclared(err));
                            }
                            None => {
                                fields_map.insert(field_name, (ty, ok_identifier.range()));
                            }
                        }
                    }
                }
                CoreStatementNode::InterfaceMethodPrototypeWrapper(interface_method_wrapper) => {
                    let core_interface_method_wrapper = interface_method_wrapper.core_ref();

                    if let CoreIdentifierInDeclNode::Ok(ok_identifier) =
                        core_interface_method_wrapper.name.core_ref()
                    {
                        let method_name = ok_identifier
                            .token_value(self.code_handler, self.semantic_db.interner());

                        if method_name == self.semantic_db.interner().intern("__init__") {
                            let err = InitMethodNotAllowedInsideConstructorError::new(
                                ok_identifier.core_ref().name.range(),
                            );

                            self.errors
                                .log_error(Diagnostics::InitMethodNotAllowedInsideConstructor(err));
                        } else {
                            self.open_block(BlockKind::Method);

                            let prototype = &core_interface_method_wrapper.prototype;
                            let (param_types_vec, return_ty, _, method_generic_ty_decls) =
                                self.declare_callable_prototype(prototype, Some(ok_identifier));

                            self.close_block(None);

                            let method_meta_data = CallableData::new(
                                param_types_vec,
                                return_ty,
                                CallableKind::Method,
                                method_generic_ty_decls,
                            );

                            methods.insert(method_name, (method_meta_data, ok_identifier.range()));
                        }
                    }
                }
                _ => unreachable!(),
            }
        }

        self.close_block(Some(interface_body));

        if let Some(symbol_obj) = &symbol_obj {
            self.semantic_db
                .interface_symbol_mut_ref(symbol_obj.symbol_index())
                .set_meta_data(fields_map, methods);
        }
    }

    fn resolve_match_case(&mut self, match_case: &MatchCaseStatementNode) {
        let core_match_case = match_case.core_ref();
        let block = &core_match_case.block;

        self.walk_expr(&core_match_case.expr);
        self.open_block(block.core_ref().kind);

        for stmt in &block.core_ref().stmts {
            let stmt = match stmt.core_ref() {
                CoreStatementIndentWrapperNode::CorrectlyIndented(stmt) => stmt,
                CoreStatementIndentWrapperNode::IncorrectlyIndented(stmt) => &stmt.core_ref().stmt,
                _ => continue,
            };
            let case_branch = match stmt.core_ref() {
                CoreStatementNode::CaseBranch(case_branch) => case_branch,
                _ => unreachable!(),
            };
            let core_case_branch = case_branch.core_ref();
            let case_block = &core_case_branch.block;

            self.open_block(case_block.core_ref().kind);

            if let Some((_, variable_name, _)) = &core_case_branch.variable_name {
                if let CoreIdentifierInDeclNode::Ok(ok_identifier) = variable_name.core_ref() {
                    match self.try_declare_and_bind_variable(ok_identifier) {
                        Ok(symbol_obj) => {
                            self.semantic_db
                                .variable_symbol_mut_ref(symbol_obj.symbol_index())
                                .set_is_init(true);
                        }
                        Err(_) => unreachable!(),
                    }
                }
            }

            for stmt in &case_block.core_ref().stmts {
                self.walk_stmt_indent_wrapper(stmt);
            }

            self.close_block(Some(case_block));
        }

        self.close_block(Some(block));
    }

    fn resolve_for_loop(&mut self, for_loop: &ForLoopStatementNode) {
        let core_for_loop = for_loop.core_ref();
        let loop_variable = &core_for_loop.loop_variable;
        let block = &core_for_loop.block;

        self.walk_expr(&core_for_loop.iterable_expr);
        self.open_block(block.core_ref().kind);

        if let CoreIdentifierInDeclNode::Ok(ok_loop_variable) = loop_variable.core_ref() {
            match self.try_declare_and_bind_variable(ok_loop_variable) {
                Ok(symbol_obj) => {
                    self.semantic_db
                        .variable_symbol_mut_ref(symbol_obj.symbol_index())
                        .set_is_init(true);
                }
                Err(_) => unreachable!(),
            }
        }

        for stmt in &block.core_ref().stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }

        self.close_block(Some(block));
    }

    fn resolve_variable(&mut self, identifier: &IdentifierInUseNode) {
        let CoreIdentifierInUseNode::Ok(ok_identifier) = identifier.core_ref() else {
            return;
        };

        if let ResolveResult::Ok(lookup_data, _) = self.try_resolving_variable(ok_identifier, true)
        {
            if lookup_data.depth > 0 {
                self.set_to_variable_non_locals(
                    lookup_data
                        .symbol_obj
                        .mangled_name(self.semantic_db.namespace_ref()),
                    lookup_data.enclosing_func_scope_depth,
                );
            }
        }
    }

    fn resolve_self_value(&mut self, self_keyword: &SelfKeywordNode) {
        let CoreSelfKeywordNode::Ok(ok_self_keyword) = self_keyword.core_ref() else {
            return;
        };

        match self.try_resolving_self_keyword(ok_self_keyword) {
            Some(_) => {
                self.set_curr_class_context_is_containing_self(true);
            }
            None => {
                let err = SelfNotFoundError::new(ok_self_keyword.range());

                self.errors.log_error(Diagnostics::SelfNotFound(err));
            }
        }
    }

    fn resolve_call_expr(&mut self, func_call: &CallExpressionNode) {
        let core_func_call = func_call.core_ref();

        if let CoreIdentifierInUseNode::Ok(ok_identifier) = core_func_call.func_name.core_ref() {
            // order of namespace search: function => type => variable
            match self.try_resolving_func(ok_identifier, false) {
                ResolveResult::Ok(_, _) => {}
                ResolveResult::NotInitialized(_, _) => unreachable!(),
                ResolveResult::InvalidGenericTypeArgsProvided(err) => {
                    let err = err_for_generic_ty_args(
                        &err,
                        ok_identifier.core_ref().name.range(),
                        IdentifierKind::Function,
                    );

                    self.errors.log_error(err);
                }
                ResolveResult::Unresolved => {
                    match self.try_resolving_user_defined_ty(ok_identifier, false, true) {
                        ResolveResult::Ok(_, _) => {}
                        ResolveResult::NotInitialized(decl_range, name) => {
                            let err = IdentifierUsedBeforeInitializedError::new(
                                self.semantic_db.interner().lookup(name),
                                IdentifierKind::UserDefinedType,
                                decl_range,
                                ok_identifier.range(),
                            );

                            self.errors
                                .log_error(Diagnostics::IdentifierUsedBeforeInitialized(err));
                        }
                        ResolveResult::InvalidGenericTypeArgsProvided(err) => {
                            let err = err_for_generic_ty_args(
                                &err,
                                ok_identifier.core_ref().name.range(),
                                IdentifierKind::UserDefinedType,
                            );

                            self.errors.log_error(err);
                        }
                        ResolveResult::Unresolved => {
                            match self.try_resolving_variable(ok_identifier, false) {
                                ResolveResult::Ok(lookup_data, _) => {
                                    let depth = lookup_data.depth;

                                    if depth > 0 {
                                        self.set_to_variable_non_locals(
                                            lookup_data
                                                .symbol_obj
                                                .mangled_name(self.semantic_db.namespace_ref()),
                                            lookup_data.enclosing_func_scope_depth,
                                        );
                                    }
                                }
                                ResolveResult::NotInitialized(decl_range, name) => {
                                    let err = IdentifierUsedBeforeInitializedError::new(
                                        self.semantic_db.interner().lookup(name),
                                        IdentifierKind::Variable,
                                        decl_range,
                                        ok_identifier.range(),
                                    );

                                    self.errors.log_error(
                                        Diagnostics::IdentifierUsedBeforeInitialized(err),
                                    );
                                }
                                ResolveResult::Unresolved => {
                                    let err = IdentifierNotFoundInAnyNamespaceError::new(
                                        ok_identifier.range(),
                                    );

                                    self.errors.log_error(
                                        Diagnostics::IdentifierNotFoundInAnyNamespace(err),
                                    );
                                }
                                ResolveResult::InvalidGenericTypeArgsProvided(err) => {
                                    let err = err_for_generic_ty_args(
                                        &err,
                                        ok_identifier.core_ref().name.range(),
                                        IdentifierKind::Variable,
                                    );

                                    self.errors.log_error(err);
                                }
                            }
                        }
                    }
                }
            }
        }

        if let Some(params) = &core_func_call.params {
            self.walk_comma_separated_expr(params)
        }
    }

    fn resolve_enum_variant_expr_or_class_method_call(
        &mut self,
        enum_variant_expr_or_class_method_call: &EnumVariantExprOrClassMethodCallNode,
    ) {
        let core_enum_variant_expr_or_class_method_call =
            enum_variant_expr_or_class_method_call.core_ref();

        if let CoreIdentifierInUseNode::Ok(ok_identifier) =
            core_enum_variant_expr_or_class_method_call
                .ty_name
                .core_ref()
        {
            self.try_resolving_user_defined_ty(ok_identifier, true, false);
        }

        self.walk_identifier_in_use(&core_enum_variant_expr_or_class_method_call.property_name);

        if let Some((_, params, _)) = &core_enum_variant_expr_or_class_method_call.params {
            if let Some(params) = params {
                self.walk_comma_separated_expr(params);
            }
        }
    }

    fn resolve_identifier(&mut self, atom_start: &AtomStartNode) {
        match atom_start.core_ref() {
            CoreAtomStartNode::Identifier(identifier) => {
                self.resolve_variable(identifier);
            }
            CoreAtomStartNode::SelfKeyword(self_keyword) => {
                self.resolve_self_value(self_keyword);
            }
            CoreAtomStartNode::Call(func_call) => {
                self.resolve_call_expr(func_call);
            }
            CoreAtomStartNode::EnumVariantExprOrClassMethodCall(
                enum_variant_expr_or_class_method_call,
            ) => self.resolve_enum_variant_expr_or_class_method_call(
                enum_variant_expr_or_class_method_call,
            ),
        }
    }
}

impl<'ctx> Visitor for JarvilResolver<'ctx> {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match node {
            ASTNode::Block(block) => {
                let core_block = block.0.as_ref();

                self.open_block(core_block.kind);

                for stmt in &core_block.stmts {
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
                self.declare_func(func_wrapper);
                None
            }
            ASTNode::StructDeclaration(struct_decl) => {
                self.declare_struct_ty(struct_decl);
                None
            }
            ASTNode::EnumDeclaration(enum_decl) => {
                self.declare_enum_ty(enum_decl);
                None
            }
            ASTNode::InterfaceDeclaration(interface_decl) => {
                self.declare_interface(interface_decl);
                None
            }
            ASTNode::LambdaTypeDeclaration(lambda_ty_decl) => {
                self.declare_lambda_ty(lambda_ty_decl);
                None
            }
            ASTNode::TypeExpression(ty_expr) => {
                self.ty_from_expr(ty_expr);
                None
            }
            ASTNode::MatchCase(match_case) => {
                self.resolve_match_case(match_case);
                None
            }
            ASTNode::ForLoop(for_loop) => {
                self.resolve_for_loop(for_loop);
                None
            }
            ASTNode::AtomStart(atom_start) => {
                self.resolve_identifier(atom_start);
                None
            }
            ASTNode::Break(break_stmt) => {
                if !self.check_enclosing_loop_scope() {
                    let err =
                        InvalidLoopControlFlowStatementFoundError::new(break_stmt.range(), "break");
                    self.errors
                        .log_error(Diagnostics::InvalidLoopControlFlowStatementFound(err));
                }
                None
            }
            ASTNode::Continue(continue_stmt) => {
                if !self.check_enclosing_loop_scope() {
                    let err = InvalidLoopControlFlowStatementFoundError::new(
                        continue_stmt.range(),
                        "continue",
                    );

                    self.errors
                        .log_error(Diagnostics::InvalidLoopControlFlowStatementFound(err));
                }
                None
            }
            _ => Some(()),
        }
    }
}
