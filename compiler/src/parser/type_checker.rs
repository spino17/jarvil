// See `https://www.csd.uwo.ca/~mmorenom/CS447/Lectures/TypeChecking.html/node1.html` for information about various cases that type-checker needs to
// cover and the representation of type expressions in terms of type objects.

use super::components::assignment::R_ASSIGNMENT_STARTING_SYMBOLS;
use super::helper::err_for_generic_ty_args;
use crate::ast::ast::{
    ArrayExpressionNode, CallExpressionNode, CallNode, ConditionalBlockNode,
    ConditionalStatementNode, CoreIdentifierInDeclNode, CoreIdentifierInUseNode,
    EnumVariantExprOrClassMethodCallNode, ForLoopStatementNode, HashMapExpressionNode,
    IdentifierInUseNode, IndexAccessNode, MatchCaseStatementNode, MethodAccessNode,
    OkIdentifierInDeclNode, OkIdentifierInUseNode, PropertyAccessNode, StructDeclarationNode,
    TupleExpressionNode, WhileLoopStatementNode,
};
use crate::code::JarvilCodeHandler;
use crate::core::common::RefOrOwned;
use crate::core::string_interner::StrId;
use crate::error::diagnostics::{
    ClassMethodExpectedParenthesisError, EnumVariantDoesNotExistError,
    EnumVariantsMissingFromMatchCaseStatementError, ExpectedValueForEnumVariantError,
    GenericTypeArgsNotExpectedError, IncorrectEnumNameError, IncorrectExpressionTypeError,
    InferredTypesNotBoundedByInterfacesError, InterfaceMethodsInStructCheckError,
    MissingTokenError, NonIterableExpressionError, NotAllConcreteTypesInferredError,
    PropertyResolvedToMultipleInterfaceObjectsError,
    RightSideExpressionTypeMismatchedWithTypeFromAnnotationError, TypeInferenceFailedError,
    UnexpectedValueProvidedToEnumVariantError,
};
use crate::error::error::JarvilProgramAnalysisErrors;
use crate::error::helper::IdentifierKind;
use crate::scope::concrete::{
    ConcreteSymbolIndex, FunctionGenericsInstantiationContext, MethodGenericsInstantiationContext,
    TurbofishTypes, TypeGenericsInstantiationContext,
};
use crate::scope::errors::GenericTypeArgsCheckError;
use crate::scope::symbol::core::{ConcreteSymbolDataEntry, SymbolDataEntry};
use crate::scope::symbol::function::{CallableData, PartialCallableDataPrototypeCheckError};
use crate::scope::symbol::types::enum_ty::EnumTypeData;
use crate::scope::symbol::types::generic_ty::GenericTypeParams;
use crate::scope::symbol::types::generic_ty::{
    GenericTypeDeclarationPlaceCategory, GenericTypePropertyQueryResult,
};
use crate::scope::symbol::types::struct_ty::StructTypeData;
use crate::scope::symbol::variables::VariableData;
use crate::types::array::core::Array;
use crate::types::core::TypeStringifyContext;
use crate::types::generic::Generic;
use crate::types::hashmap::core::HashMap;
use crate::types::non_struct::NonStructMethodsHandler;
use crate::types::r#struct::Struct;
use crate::types::traits::UserDefinedType;
use crate::types::tuple::Tuple;
use crate::{
    ast::{
        ast::{
            ASTNode, AssignmentNode, AtomNode, AtomStartNode, AtomicExpressionNode,
            BinaryExpressionNode, BlockNode, BoundedMethodWrapperNode, CallablePrototypeNode,
            ComparisonNode, CoreAssignmentNode, CoreAtomNode, CoreAtomStartNode,
            CoreAtomicExpressionNode, CoreExpressionNode, CoreRVariableDeclarationNode,
            CoreSelfKeywordNode, CoreStatementIndentWrapperNode, CoreStatementNode, CoreTokenNode,
            CoreTypeDeclarationNode, CoreUnaryExpressionNode, ExpressionNode,
            OnlyUnaryExpressionNode, RAssignmentNode, ReturnStatementNode, StatementNode,
            SymbolSeparatedSequenceNode, TokenNode, TypeExpressionNode, UnaryExpressionNode,
            VariableDeclarationNode,
        },
        traits::Node,
        walk::Visitor,
    },
    constants::common::{BOOL, FLOAT, INT, STRING},
    error::{
        diagnostics::{
            BinaryOperatorInvalidOperandsError, ClassmethodDoesNotExistError,
            ConstructorNotFoundForTypeError, Diagnostics, ExpressionIndexingNotValidError,
            ExpressionNotCallableError, FieldNotCallableError, IdentifierNotCallableError,
            ImmutableTypeNotAssignableError, InvalidIndexExpressionForTupleError,
            InvalidReturnStatementError, LessParamsCountError, MismatchedParamTypeError,
            MismatchedReturnTypeError, MismatchedTypesOnLeftRightError, MoreParamsCountError,
            NoReturnStatementInFunctionError, PropertyDoesNotExistError, PropertyNotSupportedError,
            RightSideWithVoidTypeNotAllowedError, TupleIndexOutOfBoundError,
            UnaryOperatorInvalidUseError, UnresolvedIndexExpressionInTupleError,
        },
        helper::PropertyKind,
    },
    lexer::token::{BinaryOperatorKind, UnaryOperatorKind},
    scope::{
        semantic_db::SemanticStateDatabase, symbol::function::CallablePrototypeData,
        symbol::types::core::UserDefinedTypeData,
    },
    types::{
        atomic::Atomic,
        core::{CoreType, Type},
        traits::TypeLike,
    },
};
use rustc_hash::FxHashSet;
use text_size::TextRange;

#[derive(Debug)]
struct Context {
    func_stack: Vec<Type>, // (is_constructor, return_type)
}

pub enum AtomicTokenExprKind {
    Bool,
    Integer,
    Float,
    Literal,
}

#[derive(Clone, Debug)]
pub enum InferredConcreteTypesEntry {
    Uninferred,
    Inferred(Type),
}

#[derive(Debug)]
pub enum CallExpressionPrototypeEquivalenceCheckResult<'a> {
    HasConcretePrototype(RefOrOwned<'a, CallablePrototypeData>),
    NeedsTypeInference(&'a GenericTypeParams),
}

#[derive(Debug)]
pub enum StructPropertyCheckResult {
    PropertyExist(Type),
    PropertyDoesNotExist,
    NonStructType,
}

#[derive(Debug)]
pub enum PrototypeEquivalenceCheckError {
    LessParams((usize, usize)), // (expected_params_num, received_params_num),
    MoreParams(usize),
    TypeInferenceFailed,
    NotAllConcreteTypesInferred,
    ConcreteTypesCannotBeInferred,
    MismatchedType(Vec<(String, String, usize, TextRange)>), // (expected_type, received_type, index_of_param, span)
    InferredTypesNotBoundedByInterfaces(Vec<(String, String)>, Vec<Type>), // (`inferred_ty` str, `interface_bounds` str)
}

#[derive(Debug)]
pub enum AtomStartTypeCheckError {
    PrototypeEquivalenceCheckFailed(PrototypeEquivalenceCheckError),
    IdentifierNotCallable(String),
    ConstructorNotFoundForTypeError(StrId),
}

impl From<PrototypeEquivalenceCheckError> for AtomStartTypeCheckError {
    fn from(value: PrototypeEquivalenceCheckError) -> Self {
        AtomStartTypeCheckError::PrototypeEquivalenceCheckFailed(value)
    }
}

#[derive(Debug)]
pub enum MethodAccessTypeCheckError {
    MethodNotFound,
    FieldNotCallable(Type),
    PrototypeEquivalenceCheckFailed(PrototypeEquivalenceCheckError),
    GenericTypeArgsCheckFailed(GenericTypeArgsCheckError, IdentifierKind),
    AmbigiousPropertyResolution(Vec<String>),
}

impl From<PrototypeEquivalenceCheckError> for MethodAccessTypeCheckError {
    fn from(value: PrototypeEquivalenceCheckError) -> Self {
        MethodAccessTypeCheckError::PrototypeEquivalenceCheckFailed(value)
    }
}

impl From<PartialCallableDataPrototypeCheckError> for MethodAccessTypeCheckError {
    fn from(value: PartialCallableDataPrototypeCheckError) -> Self {
        match value {
            PartialCallableDataPrototypeCheckError::PrototypeEquivalenceCheckFailed(err) => {
                MethodAccessTypeCheckError::PrototypeEquivalenceCheckFailed(err)
            }
            PartialCallableDataPrototypeCheckError::GenericTypeArgsCheckFailed(err) => {
                MethodAccessTypeCheckError::GenericTypeArgsCheckFailed(err, IdentifierKind::Method)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum TupleIndexCheckResult {
    Ok(usize),
    PositiveIndexOutOfBound,
    NegativeIndexOutOfBound,
}

pub struct JarvilTypeChecker<'ctx> {
    code_handler: &'ctx JarvilCodeHandler<'ctx>,
    errors: &'ctx JarvilProgramAnalysisErrors,
    context: Context,
    non_struct_methods_handler: NonStructMethodsHandler,
    semantic_db: SemanticStateDatabase,
}

impl<'ctx> JarvilTypeChecker<'ctx> {
    pub fn new(
        code_handler: &'ctx JarvilCodeHandler<'ctx>,
        errors: &'ctx JarvilProgramAnalysisErrors,
        semantic_db: SemanticStateDatabase,
    ) -> Self {
        let non_struct_methods_handler = NonStructMethodsHandler::new(semantic_db.interner());
        JarvilTypeChecker {
            code_handler,
            errors,
            context: Context { func_stack: vec![] },
            non_struct_methods_handler,
            semantic_db,
        }
    }

    pub fn semantic_db(&self) -> &SemanticStateDatabase {
        &self.semantic_db
    }

    pub fn err_logging_context(&self) -> TypeStringifyContext {
        self.semantic_db.err_logging_context()
    }

    pub fn check_ast(mut self, ast: &BlockNode) -> SemanticStateDatabase {
        let core_block = ast.0.as_ref();
        for stmt in &core_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        self.semantic_db
    }

    fn ty_from_expression(&self, ty_expr: &TypeExpressionNode) -> Type {
        self.semantic_db.ty_from_expr(ty_expr)
    }

    fn extract_angle_bracket_content_from_identifier_in_use(
        &self,
        ok_identifier_in_use: &OkIdentifierInUseNode,
    ) -> (Option<TurbofishTypes>, Option<Vec<TextRange>>) {
        let Some((_, generic_ty_args, _)) = &ok_identifier_in_use.core_ref().generic_ty_args else {
            return (None, None);
        };
        let mut concrete_types: Vec<Type> = vec![];
        let mut ty_ranges: Vec<TextRange> = vec![];
        for generic_ty_expr in generic_ty_args.iter() {
            let ty = self.ty_from_expression(generic_ty_expr);
            concrete_types.push(ty);
            ty_ranges.push(generic_ty_expr.range())
        }
        (Some(TurbofishTypes::new(concrete_types)), Some(ty_ranges))
    }

    fn is_unary_expr_int_valued(&self, unary: &UnaryExpressionNode) -> Option<i32> {
        match unary.core_ref() {
            CoreUnaryExpressionNode::Unary(unary) => {
                let core_unary = unary.core_ref();
                let operator_kind = &core_unary.operator_kind;
                let operand_value = self.is_unary_expr_int_valued(&core_unary.unary_expr);
                match operand_value {
                    Some(value) => match operator_kind {
                        UnaryOperatorKind::Plus => Some(value),
                        UnaryOperatorKind::Minus => Some(-value),
                        UnaryOperatorKind::Not => None,
                    },
                    None => None,
                }
            }
            CoreUnaryExpressionNode::Atomic(atomic) => match atomic.core_ref() {
                CoreAtomicExpressionNode::Integer(integer_valued_token) => {
                    match integer_valued_token.core_ref() {
                        CoreTokenNode::Ok(ok_token) => {
                            let value = ok_token.token_value_str(&self.code_handler);
                            match value.parse::<i32>() {
                                Ok(value) => Some(value),
                                Err(_) => None,
                            }
                        }
                        _ => None,
                    }
                }
                _ => None,
            },
        }
    }

    fn is_valid_index_for_tuple(
        &self,
        index_value: i32,
        tuple_len: usize,
    ) -> TupleIndexCheckResult {
        if index_value >= 0 {
            if index_value < tuple_len as i32 {
                TupleIndexCheckResult::Ok(index_value as usize)
            } else {
                TupleIndexCheckResult::PositiveIndexOutOfBound
            }
        } else if -(tuple_len as i32) <= index_value {
            TupleIndexCheckResult::Ok((tuple_len as i32 + index_value) as usize)
        } else {
            TupleIndexCheckResult::NegativeIndexOutOfBound
        }
    }

    fn is_binary_operation_valid(
        &self,
        l_ty: &Type,
        r_ty: &Type,
        operator_kind: &BinaryOperatorKind,
    ) -> Option<Type> {
        l_ty.check_operator(r_ty, operator_kind, self.semantic_db.namespace_ref())
    }

    pub fn infer_concrete_types_from_arguments(
        &self,
        generic_ty_decls: &GenericTypeParams,
        expected_prototype: &CallablePrototypeData,
        global_concrete_types: Option<&TurbofishTypes>,
        received_params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
        inference_category: GenericTypeDeclarationPlaceCategory,
    ) -> Result<TurbofishTypes, PrototypeEquivalenceCheckError> {
        let Some(received_params) = received_params else {
            return Err(PrototypeEquivalenceCheckError::ConcreteTypesCannotBeInferred);
        };
        let generic_ty_decls_len = generic_ty_decls.len();
        let mut inferred_concrete_types: Vec<InferredConcreteTypesEntry> =
            vec![InferredConcreteTypesEntry::Uninferred; generic_ty_decls_len];
        let mut num_inferred_types = 0; // this should be `len_concrete_types` at the end of inference process
        let received_params_iter = received_params.iter();
        let expected_params = expected_prototype.params();
        let expected_params_len = expected_params.len();
        let mut params_len = 0;

        for (index, received_param) in received_params_iter.enumerate() {
            let param_ty = self.check_expr(received_param);
            if index >= expected_params_len {
                return Err(PrototypeEquivalenceCheckError::MoreParams(
                    expected_params_len,
                ));
            }
            let expected_ty = &expected_params[index];
            let inference_result = expected_ty.try_infer_ty_or_check_equivalence(
                &param_ty,
                &mut inferred_concrete_types,
                global_concrete_types,
                &mut num_inferred_types,
                inference_category,
                self.semantic_db.namespace_ref(),
            );
            if let Err(()) = inference_result {
                return Err(PrototypeEquivalenceCheckError::TypeInferenceFailed);
            }
            params_len += 1;
        }

        if expected_params_len > params_len {
            return Err(PrototypeEquivalenceCheckError::LessParams((
                expected_params_len,
                params_len,
            )));
        }
        if num_inferred_types != generic_ty_decls_len {
            return Err(PrototypeEquivalenceCheckError::NotAllConcreteTypesInferred);
        }

        let unpacked_inferred_concrete_types: Vec<Type> = inferred_concrete_types
            .into_iter()
            .map(|x| match x {
                InferredConcreteTypesEntry::Inferred(ty) => ty,
                InferredConcreteTypesEntry::Uninferred => unreachable!(),
            })
            .collect();
        let mut error_strs: Vec<(String, String)> = vec![]; // Vec of (inferred_ty string, interface_bounds string)

        for (index, inferred_ty) in unpacked_inferred_concrete_types.iter().enumerate() {
            let interface_bounds = generic_ty_decls.interface_bounds(index);
            if !inferred_ty
                .is_ty_bounded_by_interfaces(interface_bounds, self.semantic_db.namespace_ref())
            {
                error_strs.push((
                    inferred_ty.to_string(self.err_logging_context()),
                    interface_bounds.to_string(self.err_logging_context()),
                ));
            }
        }

        if !error_strs.is_empty() {
            return Err(
                PrototypeEquivalenceCheckError::InferredTypesNotBoundedByInterfaces(
                    error_strs,
                    unpacked_inferred_concrete_types,
                ),
            );
        }
        Ok(TurbofishTypes::new(unpacked_inferred_concrete_types))
    }

    pub fn check_params_ty_and_count(
        &self,
        expected_param_data: &Vec<Type>,
        received_params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<(), PrototypeEquivalenceCheckError> {
        let expected_params_len = expected_param_data.len();
        let Some(received_params) = received_params else {
            return if expected_params_len != 0 {
                Err(PrototypeEquivalenceCheckError::LessParams((
                    expected_params_len,
                    0,
                )))
            } else {
                Ok(())
            };
        };
        let received_params_iter = received_params.iter();
        let mut index = 0;
        let mut mismatch_types_vec: Vec<(String, String, usize, TextRange)> = vec![];

        for received_param in received_params_iter {
            let param_ty = self.check_expr(received_param);
            if index >= expected_params_len {
                return Err(PrototypeEquivalenceCheckError::MoreParams(
                    expected_params_len,
                ));
            }
            let expected_param_ty = &expected_param_data[index];
            if !param_ty.is_eq(expected_param_ty, self.semantic_db.namespace_ref()) {
                mismatch_types_vec.push((
                    expected_param_ty.to_string(self.err_logging_context()),
                    param_ty.to_string(self.err_logging_context()),
                    index + 1,
                    received_param.range(),
                ));
            }
            index += 1;
        }

        if index < expected_params_len {
            return Err(PrototypeEquivalenceCheckError::LessParams((
                expected_params_len,
                index,
            )));
        }
        if !mismatch_types_vec.is_empty() {
            return Err(PrototypeEquivalenceCheckError::MismatchedType(
                mismatch_types_vec,
            ));
        }

        Ok(())
    }

    fn check_func_call_expr(
        &self,
        concrete_symbol_index: &ConcreteSymbolIndex<CallableData>,
        params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, AtomStartTypeCheckError> {
        let func_data = self
            .semantic_db
            .func_symbol_ref(concrete_symbol_index.symbol_index());
        let concrete_types = concrete_symbol_index.concrete_types();
        let prototype_result = match concrete_types {
            Some(concrete_types) => {
                // CASE 1
                let context = FunctionGenericsInstantiationContext::new(Some(concrete_types));
                let concrete_prototype = func_data.concretized_prototype(
                    self.semantic_db.namespace_ref(),
                    context.into_method_context(),
                );
                CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(
                    concrete_prototype,
                )
            }
            None => {
                match func_data.generics() {
                    Some(generic_ty_decls) => {
                        // CASE 2
                        CallExpressionPrototypeEquivalenceCheckResult::NeedsTypeInference(
                            generic_ty_decls,
                        )
                    }
                    // CASE 4
                    None => CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(
                        func_data.concretized_prototype(
                            self.semantic_db.namespace_ref(),
                            MethodGenericsInstantiationContext::default(),
                        ),
                    ),
                }
            }
        };
        match prototype_result {
            CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(prototype) => {
                let return_ty = prototype.is_received_params_valid(self, params)?;
                Ok(return_ty)
            }
            CallExpressionPrototypeEquivalenceCheckResult::NeedsTypeInference(generic_ty_decls) => {
                let concrete_types = self.infer_concrete_types_from_arguments(
                    generic_ty_decls,
                    func_data.structural_prototype(),
                    None,
                    params,
                    GenericTypeDeclarationPlaceCategory::InCallable,
                )?;
                let context = FunctionGenericsInstantiationContext::new(Some(&concrete_types));
                Ok(func_data.concretized_return_ty(
                    self.semantic_db.namespace_ref(),
                    context.into_method_context(),
                ))
            }
        }
    }

    fn check_variable_call_expr(
        &self,
        concrete_symbol_index: &ConcreteSymbolIndex<VariableData>,
        params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, AtomStartTypeCheckError> {
        debug_assert!(concrete_symbol_index.concrete_types().is_none());
        let lambda_ty = self
            .semantic_db
            .variable_symbol_ref(concrete_symbol_index.symbol_index())
            .ty();
        match lambda_ty.core_ty() {
            CoreType::Lambda(lambda_data) => {
                let return_ty = lambda_data.is_received_params_valid(self, params)?;
                Ok(return_ty)
            }
            _ => Err(AtomStartTypeCheckError::IdentifierNotCallable(
                lambda_ty.to_string(self.err_logging_context()),
            )),
        }
    }

    fn check_user_defined_ty_call_expr(
        &self,
        name: StrId,
        concrete_symbol_index: &ConcreteSymbolIndex<UserDefinedTypeData>,
        params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, AtomStartTypeCheckError> {
        let UserDefinedTypeData::Struct(struct_data) = self
            .semantic_db
            .ty_symbol_ref(concrete_symbol_index.symbol_index())
        else {
            return Err(AtomStartTypeCheckError::ConstructorNotFoundForTypeError(
                name,
            ));
        };
        let concrete_types = concrete_symbol_index.concrete_types();
        let constructor_meta_data = struct_data.constructor();
        let prototype_result = match concrete_types {
            Some(concrete_types) => {
                // CASE 1
                let context = TypeGenericsInstantiationContext::new(Some(concrete_types));
                let concrete_prototype = constructor_meta_data.concretized_prototype(
                    self.semantic_db.namespace_ref(),
                    context.into_method_context(),
                );
                CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(
                    concrete_prototype,
                )
            }
            None => {
                match struct_data.generics() {
                    Some(generic_ty_decls) => {
                        // CASE 2
                        CallExpressionPrototypeEquivalenceCheckResult::NeedsTypeInference(
                            generic_ty_decls,
                        )
                    }
                    None => {
                        // CASE 4
                        CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(
                            constructor_meta_data.concretized_prototype(
                                self.semantic_db.namespace_ref(),
                                MethodGenericsInstantiationContext::default(),
                            ),
                        )
                    }
                }
            }
        };
        match prototype_result {
            CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(prototype) => {
                self.check_params_ty_and_count(prototype.params(), params)?;
                let return_ty = Type::new_with_struct(
                    concrete_symbol_index.symbol_index(),
                    concrete_types.cloned(), // expensive clone
                );
                Ok(return_ty)
            }
            CallExpressionPrototypeEquivalenceCheckResult::NeedsTypeInference(generic_ty_decls) => {
                let concrete_types = self.infer_concrete_types_from_arguments(
                    generic_ty_decls,
                    constructor_meta_data.structural_prototype(),
                    None,
                    params,
                    GenericTypeDeclarationPlaceCategory::InType,
                )?;
                Ok(Type::new_with_struct(
                    concrete_symbol_index.symbol_index(),
                    Some(concrete_types),
                ))
            }
        }
    }

    fn check_atom_start_call_expr(&self, call_expr: &CallExpressionNode) -> Type {
        let core_call_expr = call_expr.core_ref();
        let func_name = &core_call_expr.func_name;
        let params = &core_call_expr.params;
        let CoreIdentifierInUseNode::Ok(ok_identifier) = func_name.core_ref() else {
            return Type::new_with_unknown();
        };

        // NOTE: For call expression syntax like `f(...) or f<...>(...)` while resolving
        // we provide the freedom to have no <...> even if `f` expects to have it because it
        // can be inferred. This inference is done here.
        // There are 4 cases:
        //     CASE 1. <...> in decl, <...> in usage => successful resolution and saves the key index.
        //     CASE 2. <...> in decl, <...> not in usage => successful resolution
        //          (entry will be created in `node vs concrete_symbol_data` mapping with None as key).
        //     CASE 3. <...> not in decl, <...> in usage => error while resolving
        //     CASE 4. <...> not in decl, <...> not in usage => no error

        let Some(concrete_symbol_entry) =
            self.semantic_db.symbol_for_identifier_in_use(ok_identifier)
        else {
            return Type::new_with_unknown();
        };
        let result = match concrete_symbol_entry {
            ConcreteSymbolDataEntry::Variable(concrete_symbol_index) => {
                self.check_variable_call_expr(&concrete_symbol_index, params)
            }
            ConcreteSymbolDataEntry::Function(concrete_symbol_index) => {
                self.check_func_call_expr(&concrete_symbol_index, params)
            }
            ConcreteSymbolDataEntry::Type(concrete_symbol_index) => {
                let name =
                    ok_identifier.token_value(&self.code_handler, self.semantic_db.interner());
                self.check_user_defined_ty_call_expr(name, &concrete_symbol_index, params)
            }
            ConcreteSymbolDataEntry::Interface(_) => unreachable!(),
        };
        match result {
            Ok(return_ty) => return_ty,
            Err(err) => {
                match err {
                    AtomStartTypeCheckError::ConstructorNotFoundForTypeError(struct_name) => {
                        let err = ConstructorNotFoundForTypeError::new(
                            self.semantic_db.interner().lookup(struct_name),
                            func_name.range(),
                        );
                        self.errors
                            .log_error(Diagnostics::ConstructorNotFoundForType(err));
                    }
                    AtomStartTypeCheckError::IdentifierNotCallable(ty_str) => {
                        let err = IdentifierNotCallableError::new(ty_str, func_name.range());
                        self.errors
                            .log_error(Diagnostics::IdentifierNotCallable(err));
                    }
                    AtomStartTypeCheckError::PrototypeEquivalenceCheckFailed(
                        prototype_equivalence_err,
                    ) => {
                        self.log_params_ty_and_count_check_error(
                            func_name.range(),
                            prototype_equivalence_err,
                        );
                    }
                }
                Type::new_with_unknown()
            }
        }
    }

    fn check_class_method_call(
        &self,
        struct_data: &StructTypeData,
        concrete_types: Option<&TurbofishTypes>,
        ty_node: &OkIdentifierInUseNode,
        ty_name: StrId,
        property_name: &IdentifierInUseNode,
        params: &Option<(
            TokenNode,
            Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
            TokenNode,
        )>,
    ) -> Type {
        let CoreIdentifierInUseNode::Ok(property_name) = property_name.core_ref() else {
            return Type::new_with_unknown();
        };
        let params = match params {
            Some((_, params, _)) => params,
            None => {
                let err =
                    ClassMethodExpectedParenthesisError::new(property_name.range().end().into());
                self.errors
                    .log_error(Diagnostics::ClassMethodExpectedParenthesis(err));
                return Type::new_with_unknown();
            }
        };
        let class_method_name =
            property_name.token_value(&self.code_handler, self.semantic_db.interner());
        let context = TypeGenericsInstantiationContext::new(concrete_types);
        match struct_data.try_class_method(&class_method_name, context) {
            Some((partial_concrete_callable_data, _)) => {
                let (concrete_types, ty_ranges) =
                    self.extract_angle_bracket_content_from_identifier_in_use(property_name);
                let result = partial_concrete_callable_data.is_received_params_valid(
                    self,
                    concrete_types,
                    ty_ranges,
                    params,
                );
                match result {
                    Ok(return_ty) => return_ty,
                    Err(err) => {
                        match err {
                            PartialCallableDataPrototypeCheckError::PrototypeEquivalenceCheckFailed(
                                prototype_check_err
                            ) => {
                                self.log_params_ty_and_count_check_error(
                                    property_name.range(),
                                    prototype_check_err,
                                );
                            }
                            PartialCallableDataPrototypeCheckError::GenericTypeArgsCheckFailed(
                                generic_typ_args_check_err
                            ) => {
                                let err = err_for_generic_ty_args(
                                    &generic_typ_args_check_err,
                                    ty_node.core_ref().name.range(),
                                    IdentifierKind::Method
                                );
                                    self.errors.log_error(err);
                                }
                            }
                        Type::new_with_unknown()
                    }
                }
            }
            None => {
                let err = ClassmethodDoesNotExistError::new(
                    self.semantic_db.interner().lookup(ty_name),
                    property_name.range(),
                );
                self.errors
                    .log_error(Diagnostics::ClassmethodDoesNotExist(err));
                Type::new_with_unknown()
            }
        }
    }

    fn check_enum_variant_expr(
        &self,
        enum_data: &EnumTypeData,
        concrete_symbol_index: &ConcreteSymbolIndex<UserDefinedTypeData>,
        ty_name: StrId,
        property_name: &IdentifierInUseNode,
        params: &Option<(
            TokenNode,
            Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
            TokenNode,
        )>,
    ) -> Type {
        let CoreIdentifierInUseNode::Ok(property_name) = property_name.core_ref() else {
            return Type::new_with_unknown();
        };
        let variant_name =
            property_name.token_value(&self.code_handler, self.semantic_db.interner());
        if property_name.core_ref().generic_ty_args.is_some() {
            let err = GenericTypeArgsNotExpectedError::new(
                IdentifierKind::Variant,
                property_name.range(),
            );
            self.errors
                .log_error(Diagnostics::GenericTypeArgsNotExpected(err));
            return Type::new_with_unknown();
        }
        let concrete_types = concrete_symbol_index.concrete_types();
        let context = TypeGenericsInstantiationContext::new(concrete_types);
        match enum_data.try_ty_for_variant(variant_name, self.semantic_db.namespace_ref(), context)
        {
            Some(expected_ty) => match params {
                Some((_, params, rparen)) => match params {
                    Some(params) => match expected_ty {
                        Some(expected_ty) => {
                            let core_params = params.core_ref();
                            let expr = &core_params.entity;
                            if let Some((comma, _)) = &core_params.remaining_entities {
                                let err = MissingTokenError::new(
                                    &[")"],
                                    match comma.core_ref() {
                                        CoreTokenNode::Ok(ok_token) => &ok_token.core_ref().token,
                                        CoreTokenNode::MissingTokens(missing_token) => {
                                            &missing_token.core_ref().received_token
                                        }
                                    },
                                );
                                self.errors.log_error(Diagnostics::MissingToken(err));
                                return Type::new_with_unknown();
                            }
                            let expr_ty = self.check_expr(expr);
                            if !expr_ty.is_eq(&expected_ty, self.semantic_db.namespace_ref()) {
                                let err = IncorrectExpressionTypeError::new(
                                    expected_ty.to_string(self.err_logging_context()),
                                    expr_ty.to_string(self.err_logging_context()),
                                    expr.range(),
                                );
                                self.errors
                                    .log_error(Diagnostics::IncorrectExpressionType(err));
                                return Type::new_with_unknown();
                            }
                        }
                        None => {
                            let err = UnexpectedValueProvidedToEnumVariantError::new(
                                self.semantic_db.interner().lookup(variant_name).to_string(),
                                params.range(),
                            );
                            self.errors
                                .log_error(Diagnostics::UnexpectedValueProvidedToEnumVariant(err));
                            return Type::new_with_unknown();
                        }
                    },
                    None => {
                        let err = MissingTokenError::new(
                            &R_ASSIGNMENT_STARTING_SYMBOLS,
                            match rparen.core_ref() {
                                CoreTokenNode::Ok(ok_token) => &ok_token.core_ref().token,
                                CoreTokenNode::MissingTokens(missing_token) => {
                                    &missing_token.core_ref().received_token
                                }
                            },
                        );
                        self.errors.log_error(Diagnostics::MissingToken(err));
                        return Type::new_with_unknown();
                    }
                },
                None => {
                    if let Some(expected_ty) = expected_ty {
                        let err = ExpectedValueForEnumVariantError::new(
                            expected_ty.to_string(self.err_logging_context()),
                            property_name.range(),
                        );
                        self.errors
                            .log_error(Diagnostics::ExpectedValueForEnumVariant(err));
                        return Type::new_with_unknown();
                    }
                }
            },
            None => {
                let err = EnumVariantDoesNotExistError::new(
                    self.semantic_db.interner().lookup(ty_name).to_string(),
                    property_name.range(),
                );
                self.errors
                    .log_error(Diagnostics::EnumVariantDoesNotExist(err));
                return Type::new_with_unknown();
            }
        }
        Type::new_with_enum(
            concrete_symbol_index.symbol_index(),
            concrete_types.cloned(),
        )
    }

    fn check_atom_start_enum_variant_expr_or_class_method_call(
        &self,
        enum_variant_expr_or_class_method_call: &EnumVariantExprOrClassMethodCallNode,
    ) -> Type {
        let core_enum_variant_expr_or_class_method_call =
            enum_variant_expr_or_class_method_call.core_ref();
        let ty = &core_enum_variant_expr_or_class_method_call.ty_name;
        let property_name = &core_enum_variant_expr_or_class_method_call.property_name;
        let params = &core_enum_variant_expr_or_class_method_call.params;
        let CoreIdentifierInUseNode::Ok(ok_identifier) = ty.core_ref() else {
            return Type::new_with_unknown();
        };
        let ty_name = ok_identifier.token_value(&self.code_handler, self.semantic_db.interner());
        match self
            .semantic_db
            .ty_symbol_for_identifier_in_use(ok_identifier)
        {
            Some(concrete_symbol_index) => {
                match &self
                    .semantic_db
                    .ty_symbol_ref(concrete_symbol_index.symbol_index())
                {
                    UserDefinedTypeData::Struct(struct_data) => self.check_class_method_call(
                        struct_data,
                        concrete_symbol_index.concrete_types(),
                        ok_identifier,
                        ty_name,
                        property_name,
                        params,
                    ),
                    UserDefinedTypeData::Enum(enum_data) => self.check_enum_variant_expr(
                        enum_data,
                        &concrete_symbol_index,
                        ty_name,
                        property_name,
                        params,
                    ),
                    UserDefinedTypeData::Lambda(_) | UserDefinedTypeData::Generic(_) => {
                        let err =
                            PropertyNotSupportedError::new("classmethod".to_string(), ty.range());
                        self.errors
                            .log_error(Diagnostics::PropertyNotSupported(err));
                        Type::new_with_unknown()
                    }
                }
            }
            None => Type::new_with_unknown(),
        }
    }

    fn check_atom_start(&self, atom_start: &AtomStartNode) -> Type {
        let core_atom_start = atom_start.core_ref();
        match core_atom_start {
            CoreAtomStartNode::Identifier(token) => match token.core_ref() {
                CoreIdentifierInUseNode::Ok(ok_identifier) => {
                    match self
                        .semantic_db
                        .variable_symbol_for_identifier_in_use(ok_identifier)
                    {
                        Some(concrete_symbol_index) => self
                            .semantic_db
                            .variable_symbol_ref(concrete_symbol_index.symbol_index())
                            .ty()
                            .clone(),
                        None => Type::new_with_unknown(),
                    }
                }
                _ => Type::new_with_unknown(),
            },
            CoreAtomStartNode::SelfKeyword(self_keyword) => {
                let core_self_keyword = self_keyword.core_ref();
                match core_self_keyword {
                    CoreSelfKeywordNode::Ok(ok_self_keyword) => {
                        match self.semantic_db.self_keyword_symbol_ref(ok_self_keyword) {
                            Some(symbol_index) => self
                                .semantic_db
                                .variable_symbol_ref(symbol_index)
                                .ty()
                                .clone(),
                            None => Type::new_with_unknown(),
                        }
                    }
                    _ => Type::new_with_unknown(),
                }
            }
            CoreAtomStartNode::Call(call_expr) => self.check_atom_start_call_expr(call_expr),
            CoreAtomStartNode::EnumVariantExprOrClassMethodCall(
                enum_variant_expr_or_class_method_call,
            ) => self.check_atom_start_enum_variant_expr_or_class_method_call(
                enum_variant_expr_or_class_method_call,
            ),
        }
    }

    fn check_call(&self, call: &CallNode) -> (Type, Option<Type>) {
        let core_call = call.core_ref();
        let atom = &core_call.atom;
        let params = &core_call.params;
        let (atom_ty, _) = self.check_atom(atom);
        match &atom_ty.core_ty() {
            CoreType::Lambda(lambda_data) => {
                let result = lambda_data.is_received_params_valid(self, params);
                match result {
                    Ok(return_ty) => (return_ty, Some(atom_ty)),
                    Err(err) => {
                        self.log_params_ty_and_count_check_error(atom.range(), err);
                        (Type::new_with_unknown(), Some(atom_ty))
                    }
                }
            }
            _ => {
                let err = ExpressionNotCallableError::new(atom.range());
                self.errors
                    .log_error(Diagnostics::ExpressionNotCallable(err));
                (Type::new_with_unknown(), Some(atom_ty))
            }
        }
    }

    fn check_property_access(&self, property_access: &PropertyAccessNode) -> (Type, Option<Type>) {
        let core_property_access = property_access.core_ref();
        let atom = &core_property_access.atom;
        let (atom_ty, _) = self.check_atom(atom);
        let property = &core_property_access.propertry;
        let CoreIdentifierInUseNode::Ok(ok_identifier) = property.core_ref() else {
            return (Type::new_with_unknown(), Some(atom_ty));
        };
        if ok_identifier.core_ref().generic_ty_args.is_some() {
            let err = GenericTypeArgsNotExpectedError::new(
                IdentifierKind::Field,
                ok_identifier.core_ref().name.range(),
            );
            self.errors
                .log_error(Diagnostics::GenericTypeArgsNotExpected(err));
            return (Type::new_with_unknown(), Some(atom_ty));
        }
        let property_name_str =
            ok_identifier.token_value(&self.code_handler, self.semantic_db.interner());
        let result = match atom_ty.core_ty() {
            CoreType::Struct(struct_ty) => {
                let concrete_types = struct_ty.concrete_types();
                let ty_data = self.semantic_db.ty_symbol_ref(struct_ty.symbol_index());
                let struct_data = ty_data.struct_data_ref();
                let context = TypeGenericsInstantiationContext::new(concrete_types);
                match struct_data.try_field(
                    &property_name_str,
                    self.semantic_db.namespace_ref(),
                    context,
                ) {
                    Some((ty, _)) => Ok(ty),
                    None => Err(Diagnostics::PropertyDoesNotExist(
                        PropertyDoesNotExistError::new(
                            PropertyKind::Field,
                            atom_ty.to_string(self.err_logging_context()),
                            property.range(),
                            atom.range(),
                        ),
                    )),
                }
            }
            CoreType::Generic(generic_ty) => {
                let ty_data = self.semantic_db.ty_symbol_ref(generic_ty.symbol_index());
                let generic_data = ty_data.generic_data_ref();
                match generic_data.try_field(&property_name_str, self.err_logging_context()) {
                    GenericTypePropertyQueryResult::Ok((ty, _)) => Ok(ty),
                    GenericTypePropertyQueryResult::AmbigiousPropertyResolution(
                        property_containing_interface_objs,
                    ) => Err(Diagnostics::PropertyResolvedToMultipleInterfaceObjects(
                        PropertyResolvedToMultipleInterfaceObjectsError::new(
                            property.range(),
                            property_containing_interface_objs,
                            PropertyKind::Field,
                        ),
                    )),
                    GenericTypePropertyQueryResult::None => Err(Diagnostics::PropertyDoesNotExist(
                        PropertyDoesNotExistError::new(
                            PropertyKind::Field,
                            atom_ty.to_string(self.err_logging_context()),
                            property.range(),
                            atom.range(),
                        ),
                    )),
                }
            }
            _ => Err(Diagnostics::PropertyDoesNotExist(
                PropertyDoesNotExistError::new(
                    PropertyKind::Field,
                    atom_ty.to_string(self.err_logging_context()),
                    property.range(),
                    atom.range(),
                ),
            )),
        };
        match result {
            Ok(property_ty) => (property_ty, Some(atom_ty)),
            Err(err) => {
                self.errors.log_error(err);
                (Type::new_with_unknown(), Some(atom_ty))
            }
        }
    }

    fn check_method_access_for_struct_ty(
        &self,
        struct_ty: &Struct,
        method_name_ok_identifier: &OkIdentifierInUseNode,
        params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, MethodAccessTypeCheckError> {
        // for syntax `<struct_obj>.<property_name>([<params>])` first type-checker tries to find `property_name` in fields
        // (for example: a field with lambda type) and then it goes on to find it in methods.
        // This is in sync with what Python does.

        let method_name =
            method_name_ok_identifier.token_value(&self.code_handler, self.semantic_db.interner());
        let concrete_types = struct_ty.concrete_types();
        let ty_data = self.semantic_db.ty_symbol_ref(struct_ty.symbol_index());
        let struct_data = ty_data.struct_data_ref();
        let context = TypeGenericsInstantiationContext::new(concrete_types);
        // first check if it's a property
        match struct_data.try_field(&method_name, self.semantic_db.namespace_ref(), context) {
            Some((propetry_ty, _)) => {
                if method_name_ok_identifier
                    .core_ref()
                    .generic_ty_args
                    .is_some()
                {
                    Err(MethodAccessTypeCheckError::GenericTypeArgsCheckFailed(
                        GenericTypeArgsCheckError::GenericTypeArgsNotExpected,
                        IdentifierKind::Field,
                    ))
                } else {
                    // check if the `property_ty` is callable
                    match propetry_ty.core_ty() {
                        CoreType::Lambda(lambda_ty) => {
                            let return_ty = lambda_ty.is_received_params_valid(self, params)?;
                            Ok(return_ty)
                        }
                        _ => Err(MethodAccessTypeCheckError::FieldNotCallable(propetry_ty)),
                    }
                }
            }
            None => {
                // if field is not there then check in methods
                match struct_data.try_method(&method_name, context) {
                    Some((partial_concrete_callable_data, _)) => {
                        let (concrete_types, ty_ranges) = self
                            .extract_angle_bracket_content_from_identifier_in_use(
                                method_name_ok_identifier,
                            );
                        let return_ty = partial_concrete_callable_data.is_received_params_valid(
                            self,
                            concrete_types,
                            ty_ranges,
                            params,
                        )?;
                        Ok(return_ty)
                    }
                    None => Err(MethodAccessTypeCheckError::MethodNotFound),
                }
            }
        }
    }

    fn check_method_access_for_generic_ty(
        &self,
        generic_ty: &Generic,
        method_name_ok_identifier: &OkIdentifierInUseNode,
        params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, MethodAccessTypeCheckError> {
        let method_name =
            method_name_ok_identifier.token_value(&self.code_handler, self.semantic_db.interner());
        let ty_data = self.semantic_db.ty_symbol_ref(generic_ty.symbol_index());
        let generic_data = ty_data.generic_data_ref();
        let interface_bounds = generic_data.interface_bounds();
        match generic_data.try_field(&method_name, self.err_logging_context()) {
            GenericTypePropertyQueryResult::Ok((propetry_ty, _)) => {
                if method_name_ok_identifier
                    .core_ref()
                    .generic_ty_args
                    .is_some()
                {
                    Err(MethodAccessTypeCheckError::GenericTypeArgsCheckFailed(
                        GenericTypeArgsCheckError::GenericTypeArgsNotExpected,
                        IdentifierKind::Field,
                    ))
                } else {
                    // check if the `property_ty` is callable
                    match propetry_ty.core_ty() {
                        CoreType::Lambda(lambda_ty) => {
                            let return_ty = lambda_ty.is_received_params_valid(self, params)?;
                            Ok(return_ty)
                        }
                        _ => Err(MethodAccessTypeCheckError::FieldNotCallable(propetry_ty)),
                    }
                }
            }
            GenericTypePropertyQueryResult::AmbigiousPropertyResolution(
                property_containing_interface_objs,
            ) => Err(MethodAccessTypeCheckError::AmbigiousPropertyResolution(
                property_containing_interface_objs,
            )),
            GenericTypePropertyQueryResult::None => {
                // if field is not there then check in methods
                match generic_data.has_method(&method_name, self.err_logging_context()) {
                    GenericTypePropertyQueryResult::Ok(interface_index) => {
                        let interface_obj =
                            interface_bounds.interface_obj_at_index(interface_index);
                        let concrete_symbol_index = interface_obj.core_symbol();
                        let interface_data = self
                            .semantic_db
                            .interface_symbol_ref(concrete_symbol_index.symbol_index());
                        let concrete_types = concrete_symbol_index.concrete_types();
                        let context = TypeGenericsInstantiationContext::new(concrete_types);
                        match interface_data.try_method(&method_name, context) {
                            Some((partial_concrete_callable_data, _)) => {
                                let (concrete_types, ty_ranges) = self
                                    .extract_angle_bracket_content_from_identifier_in_use(
                                        method_name_ok_identifier,
                                    );
                                let return_ty = partial_concrete_callable_data
                                    .is_received_params_valid(
                                        self,
                                        concrete_types,
                                        ty_ranges,
                                        params,
                                    )?;
                                Ok(return_ty)
                            }
                            None => Err(MethodAccessTypeCheckError::MethodNotFound),
                        }
                    }
                    GenericTypePropertyQueryResult::AmbigiousPropertyResolution(
                        property_containing_interface_objs,
                    ) => Err(MethodAccessTypeCheckError::AmbigiousPropertyResolution(
                        property_containing_interface_objs,
                    )),
                    GenericTypePropertyQueryResult::None => {
                        Err(MethodAccessTypeCheckError::MethodNotFound)
                    }
                }
            }
        }
    }

    fn check_method_access_for_array_ty(
        &self,
        array_ty: &Array,
        method_name_ok_identifier: &OkIdentifierInUseNode,
        params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, MethodAccessTypeCheckError> {
        let method_name = method_name_ok_identifier.token_value_str(&self.code_handler);
        let Some(prototype) = self.non_struct_methods_handler.try_method_for_array(
            array_ty,
            &method_name,
            self.semantic_db.namespace_ref(),
        ) else {
            return Err(MethodAccessTypeCheckError::MethodNotFound);
        };
        if method_name_ok_identifier
            .core_ref()
            .generic_ty_args
            .is_some()
        {
            Err(MethodAccessTypeCheckError::GenericTypeArgsCheckFailed(
                GenericTypeArgsCheckError::GenericTypeArgsNotExpected,
                IdentifierKind::Field,
            ))
        } else {
            let return_ty = prototype.is_received_params_valid(self, params)?;
            Ok(return_ty)
        }
    }

    fn check_method_access_for_hashmap_ty(
        &self,
        hashmap_ty: &HashMap,
        method_name_ok_identifier: &OkIdentifierInUseNode,
        params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, MethodAccessTypeCheckError> {
        let method_name = method_name_ok_identifier.token_value_str(&self.code_handler);
        let Some(prototype) = self.non_struct_methods_handler.try_method_for_hashmap(
            hashmap_ty,
            &method_name,
            self.semantic_db.namespace_ref(),
        ) else {
            return Err(MethodAccessTypeCheckError::MethodNotFound);
        };
        if method_name_ok_identifier
            .core_ref()
            .generic_ty_args
            .is_some()
        {
            Err(MethodAccessTypeCheckError::GenericTypeArgsCheckFailed(
                GenericTypeArgsCheckError::GenericTypeArgsNotExpected,
                IdentifierKind::Field,
            ))
        } else {
            let return_ty = prototype.is_received_params_valid(self, params)?;
            Ok(return_ty)
        }
    }

    fn check_method_access(&self, method_access: &MethodAccessNode) -> (Type, Option<Type>) {
        let core_method_access = method_access.core_ref();
        let atom = &core_method_access.atom;
        let (atom_ty, _) = self.check_atom(atom);
        let method = &core_method_access.method_name;
        let params = &core_method_access.params;
        let CoreIdentifierInUseNode::Ok(ok_identifier) = method.core_ref() else {
            return (Type::new_with_unknown(), Some(atom_ty));
        };
        let result = match &atom_ty.core_ty() {
            CoreType::Struct(struct_ty) => {
                self.check_method_access_for_struct_ty(struct_ty, ok_identifier, params)
            }
            CoreType::Generic(generic_ty) => {
                self.check_method_access_for_generic_ty(generic_ty, ok_identifier, params)
            }
            CoreType::Array(array_ty) => {
                self.check_method_access_for_array_ty(array_ty, ok_identifier, params)
            }
            CoreType::HashMap(hashmap_ty) => {
                self.check_method_access_for_hashmap_ty(hashmap_ty, ok_identifier, params)
            }
            _ => Err(MethodAccessTypeCheckError::MethodNotFound),
        };
        match result {
            Ok(return_ty) => (return_ty, Some(atom_ty)),
            Err(err) => {
                match err {
                    MethodAccessTypeCheckError::MethodNotFound => {
                        let err = PropertyDoesNotExistError::new(
                            PropertyKind::Method,
                            atom_ty.to_string(self.err_logging_context()),
                            method.range(),
                            atom.range(),
                        );
                        self.errors
                            .log_error(Diagnostics::PropertyDoesNotExist(err));
                    }
                    MethodAccessTypeCheckError::FieldNotCallable(ty) => {
                        let err = FieldNotCallableError::new(
                            ty.to_string(self.err_logging_context()),
                            ok_identifier.range(),
                        );
                        self.errors.log_error(Diagnostics::FieldNotCallable(err));
                    }
                    MethodAccessTypeCheckError::GenericTypeArgsCheckFailed(
                        generic_ty_args_check_err,
                        kind,
                    ) => {
                        let err = err_for_generic_ty_args(
                            &generic_ty_args_check_err,
                            ok_identifier.core_ref().name.range(),
                            kind,
                        );
                        self.errors.log_error(err);
                    }
                    MethodAccessTypeCheckError::PrototypeEquivalenceCheckFailed(
                        prototype_check_err,
                    ) => {
                        self.log_params_ty_and_count_check_error(
                            ok_identifier.range(),
                            prototype_check_err,
                        );
                    }
                    MethodAccessTypeCheckError::AmbigiousPropertyResolution(
                        method_containing_interface_objs,
                    ) => {
                        let err = PropertyResolvedToMultipleInterfaceObjectsError::new(
                            ok_identifier.range(),
                            method_containing_interface_objs,
                            PropertyKind::Method,
                        );
                        self.errors.log_error(
                            Diagnostics::PropertyResolvedToMultipleInterfaceObjects(err),
                        );
                    }
                }
                (Type::new_with_unknown(), Some(atom_ty))
            }
        }
    }

    fn check_index_access_for_tuple_ty(
        &self,
        tuple_ty: &Tuple,
        index_expr: &ExpressionNode,
    ) -> Type {
        let sub_types = tuple_ty.sub_types();
        let CoreExpressionNode::Unary(index_unary_expr) = index_expr.core_ref() else {
            let err = InvalidIndexExpressionForTupleError::new(index_expr.range());
            self.errors
                .log_error(Diagnostics::InvalidIndexExpressionForTuple(err));
            return Type::new_with_unknown();
        };
        let Some(index_value) = self.is_unary_expr_int_valued(index_unary_expr) else {
            let err = UnresolvedIndexExpressionInTupleError::new(index_expr.range());
            self.errors
                .log_error(Diagnostics::UnresolvedIndexExpressionInTuple(err));
            return Type::new_with_unknown();
        };
        match self.is_valid_index_for_tuple(index_value, sub_types.len()) {
            TupleIndexCheckResult::Ok(index_value) => sub_types[index_value].clone(),
            TupleIndexCheckResult::PositiveIndexOutOfBound => {
                let err = TupleIndexOutOfBoundError::new(sub_types.len(), index_expr.range());
                self.errors
                    .log_error(Diagnostics::TupleIndexOutOfBound(err));
                Type::new_with_unknown()
            }
            TupleIndexCheckResult::NegativeIndexOutOfBound => {
                let err = TupleIndexOutOfBoundError::new(sub_types.len(), index_expr.range());
                self.errors
                    .log_error(Diagnostics::TupleIndexOutOfBound(err));
                Type::new_with_unknown()
            }
        }
    }

    fn check_index_access(&self, index_access: &IndexAccessNode) -> (Type, Option<Type>) {
        let core_index_access = index_access.core_ref();
        let atom = &core_index_access.atom;
        let (atom_ty, _) = self.check_atom(atom);
        let index_expr = &core_index_access.index;
        let index_ty = self.check_expr(index_expr);
        let result = match atom_ty.core_ty() {
            CoreType::Tuple(tuple_data) => {
                return (
                    self.check_index_access_for_tuple_ty(tuple_data, index_expr),
                    Some(atom_ty),
                )
            }
            CoreType::Array(array_data) => {
                if index_ty.is_int() {
                    Some(array_data.element_ty().clone())
                } else {
                    None
                }
            }
            CoreType::Atomic(atomic_data) => match atomic_data {
                Atomic::String => {
                    if index_ty.is_int() {
                        Some(Type::new_with_atomic("str"))
                    } else {
                        None
                    }
                }
                _ => None,
            },
            CoreType::HashMap(hashmap_data) => {
                // TODO - instead of having `is_hashable` check, replace it with `is_type_bounded_by` `Hash` interface
                if index_ty.is_eq(hashmap_data.key_ty(), self.semantic_db.namespace_ref())
                    && index_ty.is_hashable()
                {
                    Some(hashmap_data.value_ty().clone())
                } else {
                    None
                }
            }
            CoreType::Struct(_struct_data) => {
                // TODO - check if struct implements `Iterable` interface
                None
            }
            CoreType::Generic(_generic_data) => {
                // TODO - check if generic type is bounded by `Iterable` interface
                None
            }
            _ => None,
        };
        match result {
            Some(ty) => (ty, Some(atom_ty)),
            None => {
                let err = ExpressionIndexingNotValidError::new(
                    &atom_ty,
                    &index_ty,
                    atom.range(),
                    index_expr.range(),
                    self.err_logging_context(),
                );
                self.errors
                    .log_error(Diagnostics::ExpressionIndexingNotValid(err));
                (Type::new_with_unknown(), Some(atom_ty))
            }
        }
    }

    fn check_atom(&self, atom: &AtomNode) -> (Type, Option<Type>) {
        let core_atom = atom.core_ref();
        match core_atom {
            CoreAtomNode::AtomStart(atom_start) => (self.check_atom_start(atom_start), None),
            CoreAtomNode::Call(call) => self.check_call(call),
            CoreAtomNode::PropertyAccess(property_access) => {
                self.check_property_access(property_access)
            }
            CoreAtomNode::MethodAccess(method_access) => self.check_method_access(method_access),
            CoreAtomNode::IndexAccess(index_access) => self.check_index_access(index_access),
        }
    }

    fn check_r_assign(&self, r_assign: &RAssignmentNode) -> Type {
        let core_r_assign = r_assign.core_ref();
        self.check_expr(&core_r_assign.expr.core_ref().expr)
    }

    fn check_token(&self, token: &TokenNode, kind: AtomicTokenExprKind) -> Type {
        match token.core_ref() {
            CoreTokenNode::Ok(_) => match kind {
                AtomicTokenExprKind::Integer => Type::new_with_atomic(INT),
                AtomicTokenExprKind::Bool => Type::new_with_atomic(BOOL),
                AtomicTokenExprKind::Float => Type::new_with_atomic(FLOAT),
                AtomicTokenExprKind::Literal => Type::new_with_atomic(STRING),
            },
            _ => Type::new_with_unknown(),
        }
    }

    fn check_array_expr(&self, array_expr: &ArrayExpressionNode) -> Type {
        let core_array_expr = array_expr.core_ref();
        let Some(initials) = &core_array_expr.initials else {
            //let err = ExpressionTypeCannotBeInferredError::new(array_expr.range());
            //self.log_error(Diagnostics::ExpressionTypeCannotBeInferred(err));
            //Type::new_with_array(Type::new_with_unknown())
            todo!()
        };
        let mut initials_iter = initials.iter();
        let first_expr_ty = match initials_iter.next() {
            Some(expr) => self.check_expr(expr),
            None => unreachable!(),
        };

        for expr in initials_iter {
            let ty = self.check_expr(expr);
            if !ty.is_eq(&first_expr_ty, self.semantic_db.namespace_ref()) {
                let err = IncorrectExpressionTypeError::new(
                    first_expr_ty.to_string(self.err_logging_context()),
                    ty.to_string(self.err_logging_context()),
                    expr.range(),
                );
                self.errors
                    .log_error(Diagnostics::IncorrectExpressionType(err));
            }
        }

        Type::new_with_array(first_expr_ty)
    }

    fn check_hashmap_expr(&self, hashmap_expr: &HashMapExpressionNode) -> Type {
        let core_hashmap_expr = hashmap_expr.core_ref();
        let Some(initials) = &core_hashmap_expr.initials else {
            //let err = ExpressionTypeCannotBeInferredError::new(hashmap_expr.range());
            //self.log_error(Diagnostics::ExpressionTypeCannotBeInferred(err));
            //Type::new_with_array(Type::new_with_unknown())
            todo!()
        };
        let mut initials_iter = initials.iter();
        let (first_key_ty, first_value_ty) = match initials_iter.next() {
            Some(key_value_pair) => {
                let key_ty = self.check_expr(&key_value_pair.core_ref().key_expr);
                let value_ty = self.check_expr(&key_value_pair.core_ref().value_expr);
                (key_ty, value_ty)
            }
            None => unreachable!(),
        };

        for key_value_pair in initials_iter {
            let core_key_value_pair = key_value_pair.core_ref();
            let key_ty = self.check_expr(&core_key_value_pair.key_expr);
            let value_ty = self.check_expr(&core_key_value_pair.value_expr);
            if !key_ty.is_eq(&first_key_ty, self.semantic_db.namespace_ref()) {
                let err = IncorrectExpressionTypeError::new(
                    first_key_ty.to_string(self.err_logging_context()),
                    key_ty.to_string(self.err_logging_context()),
                    core_key_value_pair.key_expr.range(),
                );
                self.errors
                    .log_error(Diagnostics::IncorrectExpressionType(err));
            }
            if !value_ty.is_eq(&first_value_ty, self.semantic_db.namespace_ref()) {
                let err = IncorrectExpressionTypeError::new(
                    first_value_ty.to_string(self.err_logging_context()),
                    value_ty.to_string(self.err_logging_context()),
                    core_key_value_pair.value_expr.range(),
                );
                self.errors
                    .log_error(Diagnostics::IncorrectExpressionType(err));
            }
        }

        Type::new_with_hashmap(first_key_ty, first_value_ty)
    }

    fn check_tuple_expr(&self, tuple_expr: &TupleExpressionNode) -> Type {
        let mut sub_types = vec![];
        for expr in tuple_expr.core_ref().initials.iter() {
            sub_types.push(self.check_expr(expr));
        }
        Type::new_with_tuple(sub_types)
    }

    fn check_atomic_expr(&self, atomic_expr: &AtomicExpressionNode) -> Type {
        let core_atomic_expr = atomic_expr.core_ref();
        match core_atomic_expr {
            CoreAtomicExpressionNode::Bool(token) => {
                self.check_token(token, AtomicTokenExprKind::Bool)
            }
            CoreAtomicExpressionNode::Integer(token) => {
                self.check_token(token, AtomicTokenExprKind::Integer)
            }
            CoreAtomicExpressionNode::FloatingPointNumber(token) => {
                self.check_token(token, AtomicTokenExprKind::Float)
            }
            CoreAtomicExpressionNode::Literal(token) => {
                self.check_token(token, AtomicTokenExprKind::Literal)
            }
            CoreAtomicExpressionNode::ParenthesisedExpression(parenthesised_expr) => {
                self.check_expr(&parenthesised_expr.core_ref().expr)
            }
            CoreAtomicExpressionNode::ArrayExpression(array_expr) => {
                self.check_array_expr(array_expr)
            }
            CoreAtomicExpressionNode::HashMapExpression(hashmap_expr) => {
                self.check_hashmap_expr(hashmap_expr)
            }
            CoreAtomicExpressionNode::TupleExpression(tuple_expr) => {
                self.check_tuple_expr(tuple_expr)
            }
            CoreAtomicExpressionNode::Atom(atom) => self.check_atom(atom).0,
            CoreAtomicExpressionNode::MissingTokens(_) => Type::new_with_unknown(),
        }
    }

    fn check_only_unary_expr(&self, only_unary_expr: &OnlyUnaryExpressionNode) -> Type {
        let core_only_unary_expr = only_unary_expr.core_ref();
        let unary_expr = &core_only_unary_expr.unary_expr;
        let operand_ty = self.check_unary_expr(unary_expr);
        let operator = &core_only_unary_expr.operator;
        let operator_kind = &core_only_unary_expr.operator_kind;
        match operator_kind {
            UnaryOperatorKind::Plus | UnaryOperatorKind::Minus => {
                if operand_ty.is_numeric() {
                    operand_ty
                } else {
                    let err = UnaryOperatorInvalidUseError::new(
                        operand_ty.to_string(self.err_logging_context()),
                        "numeric (`int`, `float`)",
                        "`+` or `-`",
                        unary_expr.range(),
                        operator.range(),
                    );
                    self.errors
                        .log_error(Diagnostics::UnaryOperatorInvalidUse(err));
                    Type::new_with_unknown()
                }
            }
            UnaryOperatorKind::Not => {
                if operand_ty.is_bool() {
                    operand_ty
                } else {
                    let err = UnaryOperatorInvalidUseError::new(
                        operand_ty.to_string(self.err_logging_context()),
                        "bool",
                        "`not`",
                        unary_expr.range(),
                        operator.range(),
                    );
                    self.errors
                        .log_error(Diagnostics::UnaryOperatorInvalidUse(err));
                    Type::new_with_unknown()
                }
            }
        }
    }

    fn check_unary_expr(&self, unary_expr: &UnaryExpressionNode) -> Type {
        let core_unary_expr = unary_expr.core_ref();
        match core_unary_expr {
            CoreUnaryExpressionNode::Atomic(atomic) => self.check_atomic_expr(atomic),
            CoreUnaryExpressionNode::Unary(unary) => self.check_only_unary_expr(unary),
        }
    }

    fn check_binary_expr(&self, binary_expr: &BinaryExpressionNode) -> Type {
        let core_binary_expr = binary_expr.core_ref();
        let left_expr = &core_binary_expr.left_expr;
        let right_expr = &core_binary_expr.right_expr;
        let l_ty = self.check_expr(left_expr);
        let operator = &core_binary_expr.operator;
        let operator_kind = &core_binary_expr.operator_kind;
        let r_ty = self.check_expr(right_expr);
        let result = self.is_binary_operation_valid(&l_ty, &r_ty, operator_kind);
        match result {
            Some(ty) => ty,
            None => {
                let err = BinaryOperatorInvalidOperandsError::new(
                    &l_ty,
                    &r_ty,
                    left_expr.range(),
                    right_expr.range(),
                    operator.range(),
                    self.err_logging_context(),
                );
                self.errors
                    .log_error(Diagnostics::BinaryOperatorInvalidOperands(err));
                Type::new_with_unknown()
            }
        }
    }

    fn check_comp_expr(&self, comp_expr: &ComparisonNode) -> Type {
        let core_comp_expr = comp_expr.core_ref();
        let operands = &core_comp_expr.operands;
        let operators = &core_comp_expr.operators;
        let operands_len = operands.len();

        for index in 1..operands_len {
            let left_expr = &operands[index - 1];
            let right_expr = &operands[index];
            let l_ty = self.check_expr(left_expr);
            let r_ty = self.check_expr(right_expr);
            let operator = &operators[index - 1];
            let operator_kind = operators[index - 1]
                .is_binary_operator()
                .expect("operator token is always valid");
            debug_assert!(
                operator_kind.is_comparison(),
                "all the operators in `ComparisonNode` should be comparison operators"
            );
            let result = self.is_binary_operation_valid(&l_ty, &r_ty, &operator_kind);
            match result {
                Some(ty) => match ty.core_ty() {
                    CoreType::Atomic(atomic) => debug_assert!(atomic.is_bool()),
                    CoreType::Unknown => return Type::new_with_unknown(),
                    _ => unreachable!("comparison operator always result into `bool` type"),
                },
                None => {
                    let err = BinaryOperatorInvalidOperandsError::new(
                        &l_ty,
                        &r_ty,
                        left_expr.range(),
                        right_expr.range(),
                        operator.range(),
                        self.err_logging_context(),
                    );
                    self.errors
                        .log_error(Diagnostics::BinaryOperatorInvalidOperands(err));
                    return Type::new_with_unknown();
                }
            }
        }

        Type::new_with_atomic(BOOL)
    }

    fn check_expr(&self, expr: &ExpressionNode) -> Type {
        let core_expr = expr.core_ref();
        match core_expr {
            CoreExpressionNode::Unary(unary_expr) => self.check_unary_expr(unary_expr),
            CoreExpressionNode::Binary(binary_expr) => self.check_binary_expr(binary_expr),
            CoreExpressionNode::Comparison(comparison_expr) => {
                self.check_comp_expr(comparison_expr)
            }
        }
    }

    fn check_assignment(&self, assignment: &AssignmentNode) {
        let core_assignment = assignment.core_ref();
        let (l_ty, r_assign, range) = match core_assignment {
            CoreAssignmentNode::Ok(ok_assignment) => {
                let core_ok_assignment = ok_assignment.core_ref();
                let l_expr = &core_ok_assignment.l_atom;
                let (l_ty, interior_atom_ty) = self.check_atom(l_expr);
                if let CoreAtomNode::IndexAccess(l_index_expr) = l_expr.core_ref() {
                    if let Some(interior_atom_ty) = interior_atom_ty {
                        if interior_atom_ty.is_immutable() {
                            let err = ImmutableTypeNotAssignableError::new(
                                interior_atom_ty.to_string(self.err_logging_context()),
                                l_index_expr.core_ref().atom.range(),
                            );
                            self.errors
                                .log_error(Diagnostics::ImmutableTypeNotAssignable(err));
                        }
                    }
                }
                let r_assign = &core_ok_assignment.r_assign;
                (l_ty, r_assign, l_expr.range())
            }
            CoreAssignmentNode::InvalidLValue(invalid_l_value) => {
                let core_invalid_l_value = invalid_l_value.core_ref();
                let expr = &core_invalid_l_value.l_expr;
                let r_assign = &core_invalid_l_value.r_assign;
                let l_ty = self.check_expr(expr);
                (l_ty, r_assign, expr.range())
            }
        };
        let r_ty = self.check_r_assign(r_assign);
        if r_ty.is_void() {
            let err = RightSideWithVoidTypeNotAllowedError::new(r_assign.range());
            self.errors
                .log_error(Diagnostics::RightSideWithVoidTypeNotAllowed(err));
            return;
        }
        if !l_ty.is_eq(&r_ty, self.semantic_db.namespace_ref()) {
            let err = MismatchedTypesOnLeftRightError::new(
                &l_ty,
                &r_ty,
                range,
                r_assign.range(),
                self.err_logging_context(),
            );
            self.errors
                .log_error(Diagnostics::MismatchedTypesOnLeftRight(err));
        }
    }

    fn check_variable_decl(&mut self, variable_decl: &VariableDeclarationNode) {
        let core_variable_decl = variable_decl.core_ref();
        let r_variable_decl = &core_variable_decl.r_node;
        let core_r_variable_decl = r_variable_decl.core_ref();
        let r_ty = match core_r_variable_decl {
            CoreRVariableDeclarationNode::Expression(expr_stmt) => {
                self.check_expr(&expr_stmt.core_ref().expr)
            }
            CoreRVariableDeclarationNode::Lambda(lambda) => {
                // type for `lambda` is already checked and set to the variable during name-resolution
                let body = lambda.core_ref().body.core_ref();
                self.check_callable_body(&body.prototype, &body.block);
                return;
            }
        };
        if r_ty.is_void() {
            let err = RightSideWithVoidTypeNotAllowedError::new(r_variable_decl.range());
            self.errors
                .log_error(Diagnostics::RightSideWithVoidTypeNotAllowed(err));
        }
        let CoreIdentifierInDeclNode::Ok(ok_identifier) = core_variable_decl.name.core_ref() else {
            return;
        };
        let Some(symbol_index) = self
            .semantic_db
            .variable_symbol_for_identifier_in_decl(ok_identifier)
        else {
            return;
        };
        let variable_ty = self.semantic_db.variable_symbol_ref(symbol_index).ty();
        //.clone();
        if variable_ty.is_unset() {
            // TODO - check if the `r_type` is ambigious type
            // enforce availablity of type annotation here!
            self.semantic_db
                .variable_symbol_mut_ref(symbol_index)
                .set_data_ty(&r_ty);
        } else if !variable_ty.is_eq(&r_ty, self.semantic_db.namespace_ref()) {
            let err = RightSideExpressionTypeMismatchedWithTypeFromAnnotationError::new(
                &variable_ty,
                &r_ty,
                core_variable_decl.name.range(),
                r_variable_decl.range(),
                self.err_logging_context(),
            );
            self.errors.log_error(
                Diagnostics::RightSideExpressionTypeMismatchedWithTypeFromAnnotation(err),
            )
        }
    }

    fn check_callable_prototype(&self, callable_prototype: &CallablePrototypeNode) -> Type {
        let core_callable_prototype = callable_prototype.0.as_ref();
        let return_ty_node = &core_callable_prototype.return_ty;
        match return_ty_node {
            Some((_, return_ty_expr)) => self.ty_from_expression(return_ty_expr),
            None => Type::new_with_void(),
        }
    }

    fn check_callable_body(&mut self, prototype: &CallablePrototypeNode, body: &BlockNode) {
        let return_ty = self.check_callable_prototype(prototype);
        self.context.func_stack.push(return_ty.clone());
        let mut has_return_stmt: Option<TextRange> = None;

        for stmt in &body.0.as_ref().stmts {
            let stmt = match stmt.core_ref() {
                CoreStatementIndentWrapperNode::CorrectlyIndented(stmt) => stmt,
                CoreStatementIndentWrapperNode::IncorrectlyIndented(stmt) => {
                    let core_stmt = stmt.core_ref();
                    &core_stmt.stmt
                }
                _ => continue,
            };
            self.walk_stmt(stmt);
            if let CoreStatementNode::Return(return_stmt) = stmt.core_ref() {
                has_return_stmt = Some(return_stmt.range());
            }
        }

        if has_return_stmt.is_none() && !return_ty.is_void() {
            let (_, return_ty_node) = prototype.core_ref().return_ty.as_ref().unwrap();
            let err = NoReturnStatementInFunctionError::new(return_ty_node.range());
            self.errors
                .log_error(Diagnostics::NoReturnStatementInFunction(err));
        }
        self.context.func_stack.pop();
    }

    fn check_bounded_method(&mut self, bounded_method_wrapper: &BoundedMethodWrapperNode) {
        let core_bounded_method_wrapper = bounded_method_wrapper.0.as_ref();
        let body = core_bounded_method_wrapper
            .func_decl
            .core_ref()
            .body
            .core_ref();
        self.check_callable_body(&body.prototype, &body.block);
    }

    fn check_return_stmt(&self, return_stmt: &ReturnStatementNode) {
        let core_return_stmt = return_stmt.core_ref();
        let func_stack_len = self.context.func_stack.len();
        if func_stack_len == 0 {
            let err = InvalidReturnStatementError::new(return_stmt.range());
            self.errors
                .log_error(Diagnostics::InvalidReturnStatement(err));
        }
        let expr = &core_return_stmt.expr;
        let expr_ty = match expr {
            Some(expr) => self.check_expr(expr),
            _ => Type::new_with_void(),
        };
        let expected_ty = &self.context.func_stack[func_stack_len - 1];
        if !expr_ty.is_eq(expected_ty, self.semantic_db.namespace_ref()) {
            let err = MismatchedReturnTypeError::new(
                &expected_ty,
                &expr_ty,
                core_return_stmt.return_keyword.range(),
                self.err_logging_context(),
            );
            self.errors
                .log_error(Diagnostics::MismatchedReturnType(err));
        }
    }

    fn check_struct_decl(&mut self, struct_decl: &StructDeclarationNode) {
        let core_struct_decl = struct_decl.core_ref();
        self.walk_block(&core_struct_decl.block);
        let CoreIdentifierInDeclNode::Ok(ok_identifier) = core_struct_decl.name.core_ref() else {
            return;
        };
        let Some(symbol_index) = self
            .semantic_db
            .ty_symbol_for_identifier_in_decl(ok_identifier)
        else {
            return;
        };

        let ty_data = self.semantic_db.ty_symbol_ref(symbol_index);
        let struct_data = ty_data.struct_data_ref();
        let implementing_interfaces = struct_data.implementing_interfaces();
        let Some(implementing_interfaces) = implementing_interfaces else {
            return;
        };
        let struct_methods = struct_data.methods_ref();

        for (interface_obj, range) in implementing_interfaces.iter() {
            let interface_concrete_symbol_index = interface_obj.core_symbol();
            let concrete_types = interface_concrete_symbol_index.concrete_types();
            let interface_data = self
                .semantic_db
                .interface_symbol_ref(interface_concrete_symbol_index.symbol_index());
            let context = TypeGenericsInstantiationContext::new(concrete_types);
            let partial_concrete_interface_methods =
                interface_data.partially_concrete_interface_methods(context);
            if let Err((missing_interface_method_names, errors)) =
                partial_concrete_interface_methods.is_struct_implements_interface_methods(
                    struct_methods,
                    self.semantic_db.namespace_ref(),
                )
            {
                let err = InterfaceMethodsInStructCheckError::new(
                    missing_interface_method_names,
                    errors,
                    interface_obj.to_string(self.err_logging_context()),
                    *range,
                    self.semantic_db.interner(),
                );
                self.errors
                    .log_error(Diagnostics::InterfaceMethodsInStructCheck(err));
            }
        }
    }

    fn check_conditional_block(&mut self, conditional_block: &ConditionalBlockNode) {
        let core_conditional_block = conditional_block.core_ref();
        let condition_expr = &core_conditional_block.condition_expr;
        let ty = self.check_expr(condition_expr);
        if !ty.is_bool() {
            let err = IncorrectExpressionTypeError::new(
                "bool".to_string(),
                ty.to_string(self.err_logging_context()),
                condition_expr.range(),
            );
            self.errors
                .log_error(Diagnostics::IncorrectExpressionType(err));
        }
        self.walk_block(&core_conditional_block.block);
    }

    fn check_conditional_stmt(&mut self, conditional_stmt: &ConditionalStatementNode) {
        let core_conditional_stmt = conditional_stmt.core_ref();
        self.check_conditional_block(&core_conditional_stmt.if_block);
        for elif in &core_conditional_stmt.elifs {
            self.check_conditional_block(elif);
        }
        if let Some((_, _, else_block)) = &core_conditional_stmt.else_block {
            self.walk_block(else_block);
        }
    }

    fn check_match_case_stmt(&mut self, match_case: &MatchCaseStatementNode) {
        let core_match_case = match_case.core_ref();
        let expr = &core_match_case.expr;
        let match_block = &core_match_case.block;
        let expr_ty = self.check_expr(expr);

        let CoreType::Enum(enum_ty) = expr_ty.core_ty() else {
            let err = IncorrectExpressionTypeError::new(
                "<enum>".to_string(),
                expr_ty.to_string(self.err_logging_context()),
                expr.range(),
            );
            self.errors
                .log_error(Diagnostics::IncorrectExpressionType(err));
            return;
        };

        let mut checked_variants: FxHashSet<StrId> = FxHashSet::default();
        let expr_enum_name = enum_ty.name();
        let concrete_types = enum_ty.concrete_types();
        let context = TypeGenericsInstantiationContext::new(concrete_types);
        let ty_data = self.semantic_db.ty_symbol_ref(enum_ty.symbol_index());
        let enum_data = ty_data.enum_data_ref();
        let mut symbol_index_ty_vec = vec![];
        let mut case_blocks = vec![];
        let mut enum_name_decls = vec![];

        for stmt in &match_block.0.as_ref().stmts {
            let stmt = match stmt.core_ref() {
                CoreStatementIndentWrapperNode::CorrectlyIndented(stmt) => stmt,
                CoreStatementIndentWrapperNode::IncorrectlyIndented(stmt) => {
                    let core_stmt = stmt.core_ref();
                    &core_stmt.stmt
                }
                _ => continue,
            };
            let CoreStatementNode::CaseBranch(case_branch) = stmt.core_ref() else {
                unreachable!()
            };
            let core_case_branch = case_branch.core_ref();
            let enum_name = &core_case_branch.enum_name;
            if let CoreIdentifierInDeclNode::Ok(enum_name) = enum_name.core_ref() {
                let enum_name_str =
                    enum_name.token_value(&self.code_handler, self.semantic_db.interner());
                if expr_enum_name != enum_name_str {
                    let err = IncorrectEnumNameError::new(
                        self.semantic_db.interner().lookup(expr_enum_name),
                        self.semantic_db.interner().lookup(enum_name_str),
                        enum_name.range(),
                    );
                    self.errors.log_error(Diagnostics::IncorrectEnumName(err));
                } else {
                    enum_name_decls.push(enum_name.clone());
                    let variant_name = &core_case_branch.variant_name;
                    if let CoreIdentifierInDeclNode::Ok(variant_name) = variant_name.core_ref() {
                        let variant_name_str = variant_name
                            .token_value(&self.code_handler, self.semantic_db.interner());
                        match enum_data.try_ty_for_variant(
                            variant_name_str,
                            self.semantic_db.namespace_ref(),
                            context,
                        ) {
                            Some(expected_ty) => {
                                checked_variants.insert(variant_name_str);
                                let variable_name = &core_case_branch.variable_name;
                                match variable_name {
                                    Some((_, variable_name, _)) => {
                                        match expected_ty {
                                            Some(expected_ty) => {
                                                if let CoreIdentifierInDeclNode::Ok(variable_name) =
                                                    variable_name.core_ref()
                                                {
                                                    if let Some(symbol_index) = self
                                                        .semantic_db
                                                        .variable_symbol_for_identifier_in_decl(
                                                            variable_name,
                                                        )
                                                    {
                                                        // self.semantic_state_db.namespace.variables.get_symbol_data_mut_ref(symbol_data).data.set_data_type(&expected_ty);
                                                        symbol_index_ty_vec
                                                            .push((symbol_index, expected_ty));
                                                    }
                                                };
                                            }
                                            None => {
                                                let err =
                                                    UnexpectedValueProvidedToEnumVariantError::new(
                                                        self.semantic_db
                                                            .interner()
                                                            .lookup(variant_name_str)
                                                            .to_string(),
                                                        variable_name.range(),
                                                    );
                                                self.errors.log_error(Diagnostics::UnexpectedValueProvidedToEnumVariant(err));
                                            }
                                        }
                                    }
                                    None => {
                                        if let Some(expected_ty) = expected_ty {
                                            let err = ExpectedValueForEnumVariantError::new(
                                                expected_ty.to_string(self.err_logging_context()),
                                                variant_name.range(),
                                            );
                                            self.errors.log_error(
                                                Diagnostics::ExpectedValueForEnumVariant(err),
                                            );
                                        }
                                    }
                                }
                            }
                            None => {
                                let err = EnumVariantDoesNotExistError::new(
                                    self.semantic_db
                                        .interner()
                                        .lookup(enum_name_str)
                                        .to_string(),
                                    variant_name.range(),
                                );
                                self.errors
                                    .log_error(Diagnostics::EnumVariantDoesNotExist(err));
                            }
                        }
                    }
                }
            }
            let case_block = &core_case_branch.block;
            case_blocks.push(case_block.clone());
        }

        // report any missing enum variant case
        let mut missing_variants: Vec<StrId> = vec![];
        for (variant, _, _) in enum_data.variants() {
            if !checked_variants.contains(variant) {
                missing_variants.push(*variant);
            }
        }
        if !missing_variants.is_empty() {
            let err = EnumVariantsMissingFromMatchCaseStatementError::new(
                self.semantic_db
                    .interner()
                    .lookup(expr_enum_name)
                    .to_string(),
                missing_variants,
                expr.range(),
                self.semantic_db.interner(),
            );
            self.errors
                .log_error(Diagnostics::EnumVariantsMissingFromMatchCaseStatement(err));
        }

        // set the symbol_index to enum_name nodes
        for enum_name in enum_name_decls {
            self.semantic_db
                .identifier_in_decl_binding_table_mut_ref()
                .insert(enum_name, SymbolDataEntry::Type(enum_ty.symbol_index()));
        }

        // set types to the enum variant associated variables
        for (symbol_index, ty) in symbol_index_ty_vec {
            self.semantic_db
                .variable_symbol_mut_ref(symbol_index)
                .set_data_ty(&ty);
        }

        // traverse the enum variant case blocks
        for block in case_blocks {
            self.walk_block(&block);
        }
    }

    fn check_while_loop_stmt(&mut self, while_loop_stmt: &WhileLoopStatementNode) {
        let core_while_loop = while_loop_stmt.core_ref();
        let condition_expr = &core_while_loop.condition_expr;
        let ty = self.check_expr(condition_expr);
        if !ty.is_bool() {
            let err = IncorrectExpressionTypeError::new(
                "bool".to_string(),
                ty.to_string(self.err_logging_context()),
                condition_expr.range(),
            );
            self.errors
                .log_error(Diagnostics::IncorrectExpressionType(err));
        }
        self.walk_block(&core_while_loop.block);
    }

    fn check_for_loop_stmt(&mut self, for_loop_stmt: &ForLoopStatementNode) {
        let core_for_loop = for_loop_stmt.core_ref();
        let iterable_expr = &core_for_loop.iterable_expr;
        let iterable_expr_ty = self.check_expr(iterable_expr);
        let element_ty: Option<RefOrOwned<Type>> = match iterable_expr_ty.core_ty() {
            CoreType::Array(array_data) => Some(RefOrOwned::Ref(array_data.element_ty())),
            CoreType::HashMap(hashmap_data) => Some(RefOrOwned::Ref(hashmap_data.key_ty())),
            CoreType::Atomic(atomic_data) => match atomic_data {
                Atomic::String => Some(RefOrOwned::Owned(Type::new_with_atomic("str"))),
                Atomic::Float | Atomic::Int | Atomic::Bool => None,
            },
            CoreType::Struct(_struct_data) => {
                // TODO - check if struct implement `Iterable` interface
                None
            }
            CoreType::Generic(_generic_data) => {
                // TODO - check if struct implement `Iterable` interface
                None
            }
            CoreType::Enum(_)
            | CoreType::Lambda(_)
            | CoreType::Tuple(_)
            | CoreType::Unknown
            | CoreType::Unset
            | CoreType::Void => None,
        };
        let loop_variable = &core_for_loop.loop_variable;
        if let Some(element_ty) = element_ty {
            if let CoreIdentifierInDeclNode::Ok(ok_loop_variable) = loop_variable.core_ref() {
                if let Some(symbol_index) = self
                    .semantic_db
                    .variable_symbol_for_identifier_in_decl(ok_loop_variable)
                {
                    self.semantic_db
                        .variable_symbol_mut_ref(symbol_index)
                        .set_data_ty(&element_ty);
                }
            };
        } else {
            let err = NonIterableExpressionError::new(
                iterable_expr_ty.to_string(self.err_logging_context()),
                iterable_expr.range(),
            );
            self.errors
                .log_error(Diagnostics::NonIterableExpression(err));
        }
        self.walk_block(&core_for_loop.block);
    }

    fn check_stmt(&mut self, stmt: &StatementNode) {
        match stmt.core_ref() {
            CoreStatementNode::Expression(expr_stmt) => {
                let core_expr_stmt = expr_stmt.core_ref();
                self.check_expr(&core_expr_stmt.expr);
            }
            CoreStatementNode::MatchCase(match_case_stmt) => {
                self.check_match_case_stmt(match_case_stmt)
            }
            CoreStatementNode::Assignment(assignment) => {
                self.check_assignment(assignment);
            }
            CoreStatementNode::VariableDeclaration(variable_decl) => {
                self.check_variable_decl(variable_decl);
            }
            CoreStatementNode::FunctionWrapper(func_wrapper) => {
                let body = &func_wrapper.core_ref().func_decl.core_ref().body.core_ref();
                self.check_callable_body(&body.prototype, &body.block);
            }
            CoreStatementNode::BoundedMethodWrapper(bounded_method_wrapper) => {
                self.check_bounded_method(bounded_method_wrapper);
            }
            CoreStatementNode::Return(return_stmt) => {
                self.check_return_stmt(return_stmt);
            }
            CoreStatementNode::Conditional(conditional_stmt) => {
                self.check_conditional_stmt(conditional_stmt)
            }
            CoreStatementNode::WhileLoop(while_loop_stmt) => {
                self.check_while_loop_stmt(while_loop_stmt)
            }
            CoreStatementNode::ForLoop(for_loop_stmt) => {
                self.check_for_loop_stmt(for_loop_stmt);
            }
            CoreStatementNode::TypeDeclaration(ty_decl) => match ty_decl.core_ref() {
                CoreTypeDeclarationNode::Struct(struct_decl) => {
                    self.check_struct_decl(struct_decl);
                }
                CoreTypeDeclarationNode::Lambda(_)
                | CoreTypeDeclarationNode::Enum(_)
                | CoreTypeDeclarationNode::MissingTokens(_) => {}
            },
            CoreStatementNode::InterfaceDeclaration(interface_decl) => {
                self.walk_block(&interface_decl.core_ref().block);
            }
            CoreStatementNode::StructPropertyDeclaration(_)
            | CoreStatementNode::InterfaceMethodPrototypeWrapper(_)
            | CoreStatementNode::DeclareFunctionPrototype(_)
            | CoreStatementNode::EnumVariantDeclaration(_)
            | CoreStatementNode::CaseBranch(_)
            | CoreStatementNode::Break(_)
            | CoreStatementNode::Continue(_) => (),
        }
    }

    fn log_params_ty_and_count_check_error(
        &self,
        range: TextRange,
        result: PrototypeEquivalenceCheckError,
    ) {
        match result {
            PrototypeEquivalenceCheckError::LessParams((
                expected_params_count,
                received_params_count,
            )) => {
                let err =
                    LessParamsCountError::new(expected_params_count, received_params_count, range);
                self.errors.log_error(Diagnostics::LessParamsCount(err));
            }
            PrototypeEquivalenceCheckError::MoreParams(expected_params_count) => {
                let err = MoreParamsCountError::new(expected_params_count, range);
                self.errors.log_error(Diagnostics::MoreParamsCount(err));
            }
            PrototypeEquivalenceCheckError::MismatchedType(params_vec) => {
                let err = MismatchedParamTypeError::new(params_vec);
                self.errors.log_error(Diagnostics::MismatchedParamType(err));
            }
            PrototypeEquivalenceCheckError::NotAllConcreteTypesInferred => {
                let err = NotAllConcreteTypesInferredError::new(range);
                self.errors
                    .log_error(Diagnostics::NotAllConcreteTypesInferred(err))
            }
            PrototypeEquivalenceCheckError::TypeInferenceFailed
            | PrototypeEquivalenceCheckError::ConcreteTypesCannotBeInferred => {
                let err = TypeInferenceFailedError::new(range);
                self.errors.log_error(Diagnostics::TypeInferenceFailed(err));
            }
            PrototypeEquivalenceCheckError::InferredTypesNotBoundedByInterfaces(
                err_strs,
                concrete_types,
            ) => {
                let err = InferredTypesNotBoundedByInterfacesError::new(
                    range,
                    err_strs,
                    concrete_types,
                    self.err_logging_context(),
                );
                self.errors
                    .log_error(Diagnostics::InferredTypesNotBoundedByInterfaces(err));
            }
        }
    }
}

impl<'ctx> Visitor for JarvilTypeChecker<'ctx> {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match node {
            ASTNode::Statement(stmt) => {
                self.check_stmt(stmt);
                None
            }
            _ => Some(()),
        }
    }
}
