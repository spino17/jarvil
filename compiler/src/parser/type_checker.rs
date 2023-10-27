// See `https://www.csd.uwo.ca/~mmorenom/CS447/Lectures/TypeChecking.html/node1.html` for information about various cases that type-checker needs to
// cover and the representation of type expressions in terms of type objects.

use crate::ast::ast::{
    ArrayExpressionNode, CallExpressionNode, CallNode, ClassMethodCallNode, ConditionalBlockNode,
    ConditionalStatementNode, CoreIdentifierInDeclNode, CoreIdentifierInUseNode,
    HashMapExpressionNode, IndexAccessNode, InterfaceMethodTerminalNode, MethodAccessNode,
    OkIdentifierInDeclNode, OkIdentifierInUseNode, PropertyAccessNode, StructDeclarationNode,
    TupleExpressionNode,
};
use crate::core::string_interner::StrId;
use crate::error::diagnostics::{
    GenericTypeArgsNotExpectedError, IncorrectExpressionTypeError,
    InferredTypesNotBoundedByInterfacesError, InterfaceMethodsInStructCheckError,
    NotAllConcreteTypesInferredError, RightSideExpressionTypeMismatchedWithTypeFromAnnotationError,
    TypeInferenceFailedError,
};
use crate::error::helper::IdentifierKind;
use crate::scope::concrete::{ConcreteSymbolData, ConcreteTypesTuple, ConcretizationContext};
use crate::scope::core::GenericTypeParams;
use crate::scope::errors::GenericTypeArgsCheckError;
use crate::scope::function::{
    CallableData, PartialCallableDataPrototypeCheckError, PrototypeConcretizationResult,
};
use crate::scope::handler::ConcreteSymbolDataEntry;
use crate::scope::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::scope::variables::VariableData;
use crate::types::array::core::Array;
use crate::types::core::AbstractNonStructTypes;
use crate::types::generic::Generic;
use crate::types::hashmap::core::HashMap;
use crate::types::r#struct::Struct;
use crate::types::tuple::Tuple;
use crate::{
    ast::{
        ast::{
            ASTNode, AssignmentNode, AtomNode, AtomStartNode, AtomicExpressionNode,
            BinaryExpressionNode, BlockNode, BoundedMethodKind, BoundedMethodWrapperNode,
            CallablePrototypeNode, ComparisonNode, CoreAssignmentNode, CoreAtomNode,
            CoreAtomStartNode, CoreAtomicExpressionNode, CoreExpressionNode,
            CoreRVariableDeclarationNode, CoreSelfKeywordNode, CoreStatementIndentWrapperNode,
            CoreStatementNode, CoreTokenNode, CoreTypeDeclarationNode, CoreUnaryExpressionNode,
            ExpressionNode, LambdaDeclarationNode, NameTypeSpecNode, Node, OnlyUnaryExpressionNode,
            RAssignmentNode, RVariableDeclarationNode, ReturnStatementNode, StatementNode,
            SymbolSeparatedSequenceNode, TokenNode, TypeExpressionNode, UnaryExpressionNode,
            VariableDeclarationNode,
        },
        walk::Visitor,
    },
    code::JarvilCode,
    constants::common::{BOOL, FLOAT, INT, STRING},
    error::{
        diagnostics::{
            BinaryOperatorInvalidOperandsError, ClassmethodDoesNotExistError,
            ConstructorNotFoundForTypeError, Diagnostics,
            ExplicitReturnStatementFoundInConstructorBodyError, ExpressionIndexingNotValidError,
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
        function::CallablePrototypeData, handler::SemanticStateDatabase,
        types::core::UserDefinedTypeData,
    },
    types::{
        atomic::Atomic,
        core::{AbstractType, CoreType, Type},
    },
};
use std::cell::UnsafeCell;
use text_size::TextRange;

use super::helper::err_for_generic_type_args;

#[derive(Debug)]
struct Context {
    func_stack: Vec<(bool, Type)>, // (is_constructor, return_type)
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
    HasConcretePrototype(PrototypeConcretizationResult<'a>),
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

#[derive(Debug)]
pub enum StructConstructorPrototypeCheckResult {
    Basic(Type),
    Inferred(ConcreteTypesTuple),
}

#[derive(Debug, Clone)]
pub enum TupleIndexCheckResult {
    Ok(usize),
    PositiveIndexOutOfBound,
    NegativeIndexOutOfBound,
}

pub struct TypeChecker {
    code: JarvilCode,
    errors: UnsafeCell<Vec<Diagnostics>>,
    context: Context,
    pub semantic_state_db: SemanticStateDatabase,
}

impl TypeChecker {
    pub fn new(code: JarvilCode, semantic_state_db: SemanticStateDatabase) -> Self {
        TypeChecker {
            code,
            errors: UnsafeCell::new(vec![]),
            context: Context { func_stack: vec![] },
            semantic_state_db,
        }
    }

    pub fn log_error(&self, err: Diagnostics) {
        // This method is unsafe! in favour of performance. This code is safe
        // if we guarentee that there will only be one mutable reference to the
        // `errors`. This condition currently holds true as throughout the AST
        // pass we are only pushing `err` to it with no other mutable or immutable
        // references.
        unsafe {
            let errors_ref = &mut *self.errors.get();
            errors_ref.push(err);
        };
    }

    pub fn check_ast(
        mut self,
        ast: &BlockNode,
        global_errors: &mut Vec<Diagnostics>,
    ) -> (SemanticStateDatabase, JarvilCode) {
        let core_block = ast.0.as_ref();
        for stmt in core_block.stmts.as_ref() {
            self.walk_stmt_indent_wrapper(stmt);
        }
        unsafe {
            let errors_ref = &mut *self.errors.get();
            global_errors.append(errors_ref);
        };
        (self.semantic_state_db, self.code)
    }

    pub fn is_resolved(&self, node: &OkIdentifierInDeclNode) -> bool {
        self.semantic_state_db
            .identifier_in_decl_binding_table
            .get(node)
            .is_some()
    }

    pub fn type_obj_from_expression(&self, type_expr: &TypeExpressionNode) -> (Type, bool) {
        self.semantic_state_db.get_type_obj_from_expr(type_expr)
    }

    fn extract_angle_bracket_content_from_identifier_in_use(
        &self,
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

    pub fn params_and_return_type_obj_from_expr(
        &self,
        return_type: &Option<(TokenNode, TypeExpressionNode)>,
        params: &Option<SymbolSeparatedSequenceNode<NameTypeSpecNode>>,
    ) -> (Vec<Type>, Type, Option<(Vec<usize>, bool)>) {
        let mut params_vec: Vec<Type> = vec![];
        let mut generics_containing_params_indexes = vec![];
        let mut is_concretization_required_for_return_type = false;
        let return_type: Type = match return_type {
            Some((_, return_type_expr)) => {
                let (type_obj, ty_has_generics) = self.type_obj_from_expression(return_type_expr);
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
                let name = &core_param.name;
                if let CoreIdentifierInDeclNode::Ok(ok_identifier) = name.core_ref() {
                    if self.is_resolved(ok_identifier) {
                        let (param_ty, param_ty_has_generics) =
                            self.type_obj_from_expression(&core_param.data_type);
                        if param_ty_has_generics {
                            generics_containing_params_indexes.push(params_vec.len());
                        }
                        params_vec.push(param_ty);
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
        (params_vec, return_type, is_concretization_required)
    }

    pub fn type_of_lambda(&self, lambda_decl: &LambdaDeclarationNode) -> Type {
        let core_lambda_decl = lambda_decl.0.as_ref();
        let lambda_name = &core_lambda_decl.name;
        let core_callable_body = core_lambda_decl.body.core_ref();
        let prototype = core_callable_body.prototype.core_ref();
        let params = &prototype.params;
        let return_type = &prototype.return_type;
        let (params_vec, return_type, is_concretization_required) = match lambda_name.core_ref() {
            CoreIdentifierInDeclNode::Ok(ok_identifier) => match self
                .semantic_state_db
                .get_variable_symbol_data_for_identifier_in_decl(ok_identifier)
            {
                Some(symbol_data) => return symbol_data.get_core_ref().data_type.clone(),
                None => self.params_and_return_type_obj_from_expr(return_type, params),
            },
            _ => self.params_and_return_type_obj_from_expr(return_type, params),
        };

        Type::new_with_lambda_unnamed(CallablePrototypeData::new(
            params_vec,
            return_type,
            is_concretization_required,
        ))
    }

    pub fn is_unary_expr_int_valued(&self, unary: &UnaryExpressionNode) -> Option<i32> {
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
                            let value = ok_token.token_value_str(&self.code);
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

    pub fn is_valid_index_for_tuple(
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

    pub fn is_indexable_with_type(&self, other_ty: &Type, index_type: &Type) -> Option<Type> {
        // NOTE - case for `tuple` is already handled in the calling function
        match other_ty.0.as_ref() {
            CoreType::Array(array) => {
                if index_type.is_int() {
                    Some(array.element_type.clone())
                } else {
                    None
                }
            }
            CoreType::Atomic(atomic) => match atomic {
                Atomic::String => {
                    if index_type.is_int() {
                        Some(Type::new_with_atomic("str"))
                    } else {
                        None
                    }
                }
                _ => None,
            },
            CoreType::HashMap(hashmap) => {
                // TODO - instead of having `is_hashable` check, replace it with `is_type_bounded_by` `Hash` interface
                if index_type.is_eq(&hashmap.key_type) && index_type.is_hashable() {
                    Some(hashmap.value_type.clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn is_binary_operation_valid(
        &self,
        l_type: &Type,
        r_type: &Type,
        operator_kind: &BinaryOperatorKind,
    ) -> Option<Type> {
        //if l_type.is_unknown() || r_type.is_unknown() {
        //    return Some(Type::new_with_unknown());
        //}

        l_type.check_operator(r_type, operator_kind)
    }

    pub fn infer_concrete_types_from_arguments(
        &mut self,
        generic_type_decls: &GenericTypeParams,
        expected_prototype: &CallablePrototypeData,
        global_concrete_types: Option<&ConcreteTypesTuple>,
        received_params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
        inference_category: GenericTypeDeclarationPlaceCategory,
    ) -> Result<ConcreteTypesTuple, PrototypeEquivalenceCheckError> {
        // (inferred_concrete_types, params_ty_vec)
        match received_params {
            Some(received_params) => {
                let generic_type_decls_len = generic_type_decls.len();
                let mut inferred_concrete_types: Vec<InferredConcreteTypesEntry> =
                    vec![InferredConcreteTypesEntry::Uninferred; generic_type_decls_len];
                let mut num_inferred_types = 0; // this should be `len_concrete_types` at the end of inference process
                let received_params_iter = received_params.iter();
                let expected_params = &expected_prototype.params;
                let expected_params_len = expected_params.len();
                let mut mismatch_types_vec: Vec<(String, String, usize, TextRange)> = vec![];
                let mut params_len = 0;
                for (index, received_param) in received_params_iter.enumerate() {
                    let param_ty = self.check_expr(received_param);
                    if index >= expected_params_len {
                        return Err(PrototypeEquivalenceCheckError::MoreParams(
                            expected_params_len,
                        ));
                    }
                    let expected_ty = &expected_params[index];
                    if expected_ty.is_concretization_required() {
                        let inference_result = expected_ty.try_infer_type_or_check_equivalence(
                            &param_ty,
                            &mut inferred_concrete_types,
                            global_concrete_types,
                            &mut num_inferred_types,
                            inference_category,
                        );
                        if let Err(()) = inference_result {
                            return Err(PrototypeEquivalenceCheckError::TypeInferenceFailed);
                        }
                    } else if !param_ty.is_eq(expected_ty) {
                        mismatch_types_vec.push((
                            expected_ty.to_string(&self.semantic_state_db.interner),
                            param_ty.to_string(&self.semantic_state_db.interner),
                            index + 1,
                            received_param.range(),
                        ));
                    }
                    params_len += 1;
                }
                if expected_params_len > params_len {
                    return Err(PrototypeEquivalenceCheckError::LessParams((
                        expected_params_len,
                        params_len,
                    )));
                } else if !mismatch_types_vec.is_empty() {
                    return Err(PrototypeEquivalenceCheckError::MismatchedType(
                        mismatch_types_vec,
                    ));
                } else if num_inferred_types != generic_type_decls_len {
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
                    let interface_bounds = &generic_type_decls.0[index].1;
                    if !inferred_ty.is_type_bounded_by_interfaces(interface_bounds) {
                        error_strs.push((
                            inferred_ty.to_string(&self.semantic_state_db.interner),
                            interface_bounds.to_string(&self.semantic_state_db.interner),
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
                Ok(ConcreteTypesTuple::new(unpacked_inferred_concrete_types))
            }
            None => Err(PrototypeEquivalenceCheckError::ConcreteTypesCannotBeInferred),
        }
    }

    pub fn check_params_type_and_count(
        &mut self,
        expected_param_data: &Vec<Type>,
        received_params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<(), PrototypeEquivalenceCheckError> {
        let expected_params_len = expected_param_data.len();
        match received_params {
            Some(received_params) => {
                let received_params_iter = received_params.iter();
                let mut index = 0;
                let mut mismatch_types_vec: Vec<(String, String, usize, TextRange)> = vec![];
                for received_param in received_params_iter {
                    let param_type_obj = self.check_expr(received_param);
                    if index >= expected_params_len {
                        return Err(PrototypeEquivalenceCheckError::MoreParams(
                            expected_params_len,
                        ));
                    }
                    let expected_param_type = &expected_param_data[index];
                    if !param_type_obj.is_eq(expected_param_type) {
                        mismatch_types_vec.push((
                            expected_param_type.to_string(&self.semantic_state_db.interner),
                            param_type_obj.to_string(&self.semantic_state_db.interner),
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
                } else if !mismatch_types_vec.is_empty() {
                    return Err(PrototypeEquivalenceCheckError::MismatchedType(
                        mismatch_types_vec,
                    ));
                }
                Ok(())
            }
            None => {
                if expected_params_len != 0 {
                    Err(PrototypeEquivalenceCheckError::LessParams((
                        expected_params_len,
                        0,
                    )))
                } else {
                    Ok(())
                }
            }
        }
    }

    fn check_func_call_expr(
        &mut self,
        concrete_symbol: &ConcreteSymbolData<CallableData>,
        params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, AtomStartTypeCheckError> {
        let func_data = &*concrete_symbol.get_core_ref();
        let concrete_types = &concrete_symbol.concrete_types;
        let prototype_result = match concrete_types {
            Some(concrete_types) => {
                // CASE 1
                // let concrete_types = func_data.get_concrete_types(index);
                let concrete_prototype = func_data
                    .prototype
                    .concretize_prototype(None, Some(concrete_types));
                CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(
                    concrete_prototype,
                )
            }
            None => {
                match &func_data.generics {
                    Some(generic_type_decls) => {
                        // CASE 2
                        CallExpressionPrototypeEquivalenceCheckResult::NeedsTypeInference(
                            generic_type_decls,
                        )
                    }
                    // CASE 4
                    None => CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(
                        PrototypeConcretizationResult::UnConcretized(&func_data.prototype),
                    ),
                }
            }
        };
        match prototype_result {
            CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(prototype) => {
                let prototype_ref = prototype.get_prototype_ref();
                let return_ty = prototype_ref.is_received_params_valid(self, params)?;
                Ok(return_ty)
            }
            CallExpressionPrototypeEquivalenceCheckResult::NeedsTypeInference(
                generic_type_decls,
            ) => {
                let concrete_types = self.infer_concrete_types_from_arguments(
                    generic_type_decls,
                    &func_data.prototype,
                    None,
                    params,
                    GenericTypeDeclarationPlaceCategory::InCallable,
                )?;
                let unconcrete_return_ty = &func_data.prototype.return_type;
                let concrete_return_ty = if unconcrete_return_ty.is_concretization_required() {
                    unconcrete_return_ty
                        .concretize(&ConcretizationContext::new(None, Some(&concrete_types)))
                } else {
                    unconcrete_return_ty.clone()
                };
                Ok(concrete_return_ty)
            }
        }
    }

    fn check_variable_call_expr(
        &mut self,
        concrete_symbol_data: &ConcreteSymbolData<VariableData>,
        params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, AtomStartTypeCheckError> {
        debug_assert!(concrete_symbol_data.concrete_types.is_none());
        let lambda_type = &concrete_symbol_data.get_core_ref().data_type;
        match lambda_type.0.as_ref() {
            CoreType::Lambda(lambda_data) => {
                let return_ty = lambda_data.is_received_params_valid(self, params)?;
                Ok(return_ty)
            }
            _ => Err(AtomStartTypeCheckError::IdentifierNotCallable(
                lambda_type.to_string(&self.semantic_state_db.interner),
            )),
        }
    }

    fn check_user_defined_ty_call_expr(
        &mut self,
        name: StrId,
        concrete_symbol_data: &ConcreteSymbolData<UserDefinedTypeData>,
        params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, AtomStartTypeCheckError> {
        let struct_constructor_prototype_check_result = match &*concrete_symbol_data.get_core_ref()
        {
            UserDefinedTypeData::Struct(struct_symbol_data) => {
                let concrete_types = &concrete_symbol_data.concrete_types;
                let constructor_meta_data = &struct_symbol_data.constructor;
                let prototype_result = match concrete_types {
                    Some(concrete_types) => {
                        // CASE 1
                        // let concrete_types = struct_symbol_data.get_concrete_types(index);
                        let concrete_prototype = constructor_meta_data
                            .prototype
                            .concretize_prototype(Some(concrete_types), None);
                        CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(
                            concrete_prototype,
                        )
                    }
                    None => {
                        match &struct_symbol_data.generics {
                            Some(generic_type_decls) => {
                                // CASE 2
                                CallExpressionPrototypeEquivalenceCheckResult::NeedsTypeInference(
                                    generic_type_decls,
                                )
                            }
                            None => {
                                // CASE 4
                                CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(
                                    PrototypeConcretizationResult::UnConcretized(
                                        &constructor_meta_data.prototype,
                                    ),
                                )
                            }
                        }
                    }
                };
                match prototype_result {
                    CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(
                        prototype,
                    ) => {
                        let prototype_ref = prototype.get_prototype_ref();
                        self.check_params_type_and_count(&prototype_ref.params, params)?;
                        let return_ty = Type::new_with_struct(
                            &concrete_symbol_data.symbol_data,
                            concrete_types.clone(), // expensive clone
                        );
                        StructConstructorPrototypeCheckResult::Basic(return_ty)
                    }
                    CallExpressionPrototypeEquivalenceCheckResult::NeedsTypeInference(
                        generic_type_decls,
                    ) => {
                        let concrete_types = self.infer_concrete_types_from_arguments(
                            generic_type_decls,
                            &constructor_meta_data.prototype,
                            None,
                            params,
                            GenericTypeDeclarationPlaceCategory::InStruct,
                        )?;
                        StructConstructorPrototypeCheckResult::Inferred(concrete_types)
                    }
                }
            }
            UserDefinedTypeData::Lambda(_) | UserDefinedTypeData::Generic(_) => {
                return Err(AtomStartTypeCheckError::ConstructorNotFoundForTypeError(
                    name,
                ))
            }
        };
        match struct_constructor_prototype_check_result {
            StructConstructorPrototypeCheckResult::Basic(return_ty) => Ok(return_ty),
            StructConstructorPrototypeCheckResult::Inferred(concrete_types) => Ok(
                Type::new_with_struct(&concrete_symbol_data.symbol_data, Some(concrete_types)),
            ),
        }
    }

    fn check_atom_start_call_expr(&mut self, call_expr: &CallExpressionNode) -> Type {
        let core_call_expr = call_expr.core_ref();
        let func_name = &core_call_expr.function_name;
        let params = &core_call_expr.params;
        if let CoreIdentifierInUseNode::Ok(ok_identifier) = func_name.core_ref() {
            // NOTE: For call expression syntax like `f(...) or f<...>(...)` while resolving
            // we provide the freedom to have no <...> even if `f` expects to have it because it
            // can be inferred. This inference is done here.
            // There are 4 cases:
            //     CASE 1. <...> in decl, <...> in usage => successful resolution and saves the key index.
            //     CASE 2. <...> in decl, <...> not in usage => successful resolution
            //          (entry will be created in `node vs concrete_symbol_data` mapping with None as key).
            //     CASE 3. <...> not in decl, <...> in usage => error while resolving
            //     CASE 4. <...> not in decl, <...> not in usage => no error
            if let Some(symbol_data) = self
                .semantic_state_db
                .get_symbol_data_for_identifier_in_use(ok_identifier)
            {
                let result = match symbol_data {
                    ConcreteSymbolDataEntry::Function(func_symbol_data) => {
                        self.check_func_call_expr(&func_symbol_data, params)
                    }
                    ConcreteSymbolDataEntry::Variable(variable_symbol_data) => {
                        self.check_variable_call_expr(&variable_symbol_data, params)
                    }
                    ConcreteSymbolDataEntry::Interface(_) => unreachable!(),
                    ConcreteSymbolDataEntry::Type(user_defined_type_symbol_data) => {
                        let name = ok_identifier
                            .token_value(&self.code, &mut self.semantic_state_db.interner);
                        self.check_user_defined_ty_call_expr(
                            name,
                            &user_defined_type_symbol_data,
                            params,
                        )
                    }
                };
                match result {
                    Ok(return_ty) => return return_ty,
                    Err(err) => {
                        match err {
                            AtomStartTypeCheckError::ConstructorNotFoundForTypeError(
                                struct_name,
                            ) => {
                                let err = ConstructorNotFoundForTypeError::new(
                                    self.semantic_state_db.interner.lookup(struct_name),
                                    func_name.range(),
                                );
                                self.log_error(Diagnostics::ConstructorNotFoundForType(err));
                            }
                            AtomStartTypeCheckError::IdentifierNotCallable(ty_str) => {
                                let err =
                                    IdentifierNotCallableError::new(ty_str, func_name.range());
                                self.log_error(Diagnostics::IdentifierNotCallable(err));
                            }
                            AtomStartTypeCheckError::PrototypeEquivalenceCheckFailed(
                                prototype_equivalence_err,
                            ) => {
                                self.log_params_type_and_count_check_error(
                                    func_name.range(),
                                    prototype_equivalence_err,
                                );
                            }
                        }
                        return Type::new_with_unknown();
                    }
                }
            }
        }
        Type::new_with_unknown()
    }

    fn check_atom_start_class_method_call(
        &mut self,
        class_method_call: &ClassMethodCallNode,
    ) -> Type {
        let core_class_method = class_method_call.core_ref();
        let class = &core_class_method.class_name;
        let class_method = &core_class_method.class_method_name;
        let params = &core_class_method.params;
        if let CoreIdentifierInUseNode::Ok(ok_identifier) = class.core_ref() {
            let class_name =
                ok_identifier.token_value(&self.code, &mut self.semantic_state_db.interner);
            match self
                .semantic_state_db
                .get_type_symbol_data_for_identifier_in_use(ok_identifier)
            {
                Some(type_symbol_data) => match &*type_symbol_data.get_core_ref() {
                    UserDefinedTypeData::Struct(struct_data) => match class_method.core_ref() {
                        CoreIdentifierInUseNode::Ok(class_method) => {
                            let class_method_name = class_method
                                .token_value(&self.code, &mut self.semantic_state_db.interner);
                            let concrete_types = &type_symbol_data.concrete_types;
                            match struct_data
                                .try_class_method(&class_method_name, concrete_types.as_ref())
                            {
                                Some((partial_concrete_callable_data, _)) => {
                                    let (concrete_types, ty_ranges, _) = self
                                        .extract_angle_bracket_content_from_identifier_in_use(
                                            class_method,
                                        );
                                    let result = partial_concrete_callable_data
                                        .is_received_params_valid(
                                            self,
                                            concrete_types,
                                            ty_ranges,
                                            params,
                                        );
                                    match result {
                                        Ok(return_ty) => return return_ty,
                                        Err(err) => {
                                            match err {
                                                    PartialCallableDataPrototypeCheckError::PrototypeEquivalenceCheckFailed(prototype_check_err) => {
                                                        self.log_params_type_and_count_check_error(
                                                            class_method.range(),
                                                            prototype_check_err,
                                                        );
                                                    }
                                                    PartialCallableDataPrototypeCheckError::GenericTypeArgsCheckFailed(generic_typ_args_check_err) => {
                                                        let err = err_for_generic_type_args(&generic_typ_args_check_err, ok_identifier.core_ref().name.range(), IdentifierKind::Method);
                                                        self.log_error(err);
                                                    }
                                                }
                                            return Type::new_with_unknown();
                                        }
                                    }
                                }
                                None => {
                                    let err = ClassmethodDoesNotExistError::new(
                                        self.semantic_state_db.interner.lookup(class_name),
                                        class_method.range(),
                                    );
                                    self.log_error(Diagnostics::ClassmethodDoesNotExist(err));
                                    return Type::new_with_unknown();
                                }
                            }
                        }
                        _ => return Type::new_with_unknown(),
                    },
                    _ => {
                        let err = PropertyNotSupportedError::new(
                            "classmethod".to_string(),
                            class.range(),
                        );
                        self.log_error(Diagnostics::PropertyNotSupported(err));
                        return Type::new_with_unknown();
                    }
                },
                None => return Type::new_with_unknown(),
            }
        }
        Type::new_with_unknown()
    }

    pub fn check_atom_start(&mut self, atom_start: &AtomStartNode) -> Type {
        let core_atom_start = atom_start.core_ref();
        match core_atom_start {
            CoreAtomStartNode::Identifier(token) => match token.core_ref() {
                CoreIdentifierInUseNode::Ok(ok_identifier) => {
                    match self
                        .semantic_state_db
                        .get_variable_symbol_data_for_identifier_in_use(ok_identifier)
                    {
                        Some(variable_symbol_data) => {
                            return variable_symbol_data.get_core_ref().data_type.clone()
                        }
                        None => Type::new_with_unknown(),
                    }
                }
                _ => Type::new_with_unknown(),
            },
            CoreAtomStartNode::SelfKeyword(self_keyword) => {
                let core_self_keyword = self_keyword.core_ref();
                match core_self_keyword {
                    CoreSelfKeywordNode::Ok(ok_self_keyword) => {
                        match self
                            .semantic_state_db
                            .get_self_keyword_symbol_data_ref(ok_self_keyword)
                        {
                            Some(symbol_data) => {
                                return symbol_data.get_core_ref().data_type.clone()
                            }
                            None => Type::new_with_unknown(),
                        }
                    }
                    _ => Type::new_with_unknown(),
                }
            }
            CoreAtomStartNode::Call(call_expr) => self.check_atom_start_call_expr(call_expr),
            CoreAtomStartNode::ClassMethodCall(class_method_call) => {
                self.check_atom_start_class_method_call(class_method_call)
            }
        }
    }

    fn check_call(&mut self, call: &CallNode) -> (Type, Option<Type>) {
        let core_call = call.core_ref();
        let atom = &core_call.atom;
        let params = &core_call.params;
        let (atom_type_obj, _) = self.check_atom(atom);
        match &atom_type_obj.0.as_ref() {
            CoreType::Lambda(lambda_data) => {
                let result = lambda_data.is_received_params_valid(self, params);
                match result {
                    Ok(return_ty) => (return_ty, Some(atom_type_obj)),
                    Err(err) => {
                        self.log_params_type_and_count_check_error(atom.range(), err);
                        (Type::new_with_unknown(), Some(atom_type_obj))
                    }
                }
            }
            _ => {
                let err = ExpressionNotCallableError::new(atom.range());
                self.log_error(Diagnostics::ExpressionNotCallable(err));
                (Type::new_with_unknown(), Some(atom_type_obj))
            }
        }
    }

    fn check_property_access(
        &mut self,
        property_access: &PropertyAccessNode,
    ) -> (Type, Option<Type>) {
        let core_property_access = property_access.core_ref();
        let atom = &core_property_access.atom;
        let (atom_type_obj, _) = self.check_atom(atom);
        let property = &core_property_access.propertry;
        if let CoreIdentifierInUseNode::Ok(ok_identifier) = property.core_ref() {
            match &ok_identifier.core_ref().generic_type_args {
                Some(_) => {
                    let err = GenericTypeArgsNotExpectedError::new(
                        IdentifierKind::Field,
                        ok_identifier.core_ref().name.range(),
                    );
                    self.log_error(Diagnostics::GenericTypeArgsNotExpected(err));
                }
                None => {
                    let property_name_str =
                        ok_identifier.token_value(&self.code, &mut self.semantic_state_db.interner);
                    let result = match atom_type_obj.0.as_ref() {
                        CoreType::Struct(struct_ty) => {
                            let concrete_types = &struct_ty.concrete_types;
                            let symbol_data = struct_ty.symbol_data.get_core_ref();
                            let struct_data = symbol_data.get_struct_data_ref();
                            match struct_data.try_field(
                                &property_name_str,
                                &ConcretizationContext::new(
                                    match concrete_types {
                                        Some(concrete_types) => Some(concrete_types),
                                        None => None,
                                    },
                                    None,
                                ),
                            ) {
                                Some((type_obj, _)) => Ok(type_obj),
                                None => Err(()),
                            }
                        }
                        CoreType::Generic(_generic_ty) => {
                            // TODO - change this to allow property access for generic types also
                            Err(())
                        }
                        _ => Err(()),
                    };
                    match result {
                        Ok(property_ty) => return (property_ty, Some(atom_type_obj)),
                        Err(_) => {
                            let err = PropertyDoesNotExistError::new(
                                PropertyKind::Field,
                                atom_type_obj.to_string(&self.semantic_state_db.interner),
                                property.range(),
                                atom.range(),
                            );
                            self.log_error(Diagnostics::PropertyDoesNotExist(err));
                            return (Type::new_with_unknown(), Some(atom_type_obj));
                        }
                    }
                }
            }
        }
        (Type::new_with_unknown(), Some(atom_type_obj))
    }

    fn check_method_access_for_struct_ty(
        &mut self,
        struct_ty: &Struct,
        method_name_ok_identifier: &OkIdentifierInUseNode,
        params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, MethodAccessTypeCheckError> {
        // for syntax `<struct_obj>.<property_name>([<params>])` first type-checker tries to find `property_name` in fields
        // (for example: a field with lambda type) and then it goes on to find it in methods.
        // This is in sync with what Python does.
        let method_name =
            method_name_ok_identifier.token_value(&self.code, &mut self.semantic_state_db.interner);
        let concrete_types = &struct_ty.concrete_types;
        let symbol_data = struct_ty.symbol_data.get_core_ref();
        let struct_data = symbol_data.get_struct_data_ref();
        // first check if it's a property
        match struct_data.try_field(
            &method_name,
            &ConcretizationContext::new(
                match concrete_types {
                    Some(concrete_types) => Some(concrete_types),
                    None => None,
                },
                None,
            ),
        ) {
            Some((propetry_ty, _)) => {
                if method_name_ok_identifier
                    .core_ref()
                    .generic_type_args
                    .is_some()
                {
                    Err(MethodAccessTypeCheckError::GenericTypeArgsCheckFailed(
                        GenericTypeArgsCheckError::GenericTypeArgsNotExpected,
                        IdentifierKind::Field,
                    ))
                } else {
                    // check if the `property_ty` is callable
                    match propetry_ty.0.as_ref() {
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
                match struct_data.try_method(&method_name, concrete_types.as_ref()) {
                    Some((partial_concrete_callable_data, _)) => {
                        let (concrete_types, ty_ranges, _) = self
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
        _generic_ty: &Generic,
        _method_name_ok_identifier: &OkIdentifierInUseNode,
        _params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, MethodAccessTypeCheckError> {
        // TODO - change this to allow method access for generic types also
        Err(MethodAccessTypeCheckError::MethodNotFound)
    }

    fn check_method_access_for_array_ty(
        &mut self,
        array_ty: &Array,
        method_name_ok_identifier: &OkIdentifierInUseNode,
        params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, MethodAccessTypeCheckError> {
        let method_name = method_name_ok_identifier.token_value_str(&self.code);
        match array_ty.try_method(&method_name) {
            Some(prototype) => {
                let return_ty = prototype.is_received_params_valid(self, params)?;
                Ok(return_ty)
            }
            None => Err(MethodAccessTypeCheckError::MethodNotFound),
        }
    }

    fn check_method_access_for_hashmap_ty(
        &mut self,
        hashmap_ty: &HashMap,
        method_name_ok_identifier: &OkIdentifierInUseNode,
        params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Result<Type, MethodAccessTypeCheckError> {
        let method_name = method_name_ok_identifier.token_value_str(&self.code);
        match hashmap_ty.try_method(&method_name) {
            Some(prototype) => {
                let return_ty = prototype.is_received_params_valid(self, params)?;
                Ok(return_ty)
            }
            None => Err(MethodAccessTypeCheckError::MethodNotFound),
        }
    }

    fn check_method_access(&mut self, method_access: &MethodAccessNode) -> (Type, Option<Type>) {
        let core_method_access = method_access.core_ref();
        let atom = &core_method_access.atom;
        let (atom_type_obj, _) = self.check_atom(atom);
        let method = &core_method_access.method_name;
        let params = &core_method_access.params;
        if let CoreIdentifierInUseNode::Ok(ok_identifier) = method.core_ref() {
            let result: Result<Type, MethodAccessTypeCheckError> = match &atom_type_obj.0.as_ref() {
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
                Ok(return_ty) => return (return_ty, Some(atom_type_obj)),
                Err(err) => {
                    match err {
                        MethodAccessTypeCheckError::MethodNotFound => {
                            let err = PropertyDoesNotExistError::new(
                                PropertyKind::Method,
                                atom_type_obj.to_string(&self.semantic_state_db.interner),
                                method.range(),
                                atom.range(),
                            );
                            self.log_error(Diagnostics::PropertyDoesNotExist(err));
                        }
                        MethodAccessTypeCheckError::FieldNotCallable(ty) => {
                            let err = FieldNotCallableError::new(
                                ty.to_string(&self.semantic_state_db.interner),
                                ok_identifier.range(),
                            );
                            self.log_error(Diagnostics::FieldNotCallable(err));
                        }
                        MethodAccessTypeCheckError::GenericTypeArgsCheckFailed(
                            generic_type_args_check_err,
                            kind,
                        ) => {
                            let err = err_for_generic_type_args(
                                &generic_type_args_check_err,
                                ok_identifier.core_ref().name.range(),
                                kind,
                            );
                            self.log_error(err);
                        }
                        MethodAccessTypeCheckError::PrototypeEquivalenceCheckFailed(
                            prototype_check_err,
                        ) => {
                            self.log_params_type_and_count_check_error(
                                ok_identifier.range(),
                                prototype_check_err,
                            );
                        }
                    }
                    return (Type::new_with_unknown(), Some(atom_type_obj));
                }
            }
        }
        (Type::new_with_unknown(), Some(atom_type_obj))
    }

    fn check_index_access_for_tuple_ty(
        &self,
        tuple_ty: &Tuple,
        index_expr: &ExpressionNode,
    ) -> Type {
        let sub_types = &tuple_ty.sub_types;
        match index_expr.core_ref() {
            CoreExpressionNode::Unary(index_unary_expr) => {
                match self.is_unary_expr_int_valued(index_unary_expr) {
                    Some(index_value) => {
                        match self.is_valid_index_for_tuple(index_value, sub_types.len()) {
                            TupleIndexCheckResult::Ok(index_value) => {
                                sub_types[index_value].clone()
                            }
                            TupleIndexCheckResult::PositiveIndexOutOfBound => {
                                let err = TupleIndexOutOfBoundError::new(
                                    sub_types.len(),
                                    index_expr.range(),
                                );
                                self.log_error(Diagnostics::TupleIndexOutOfBound(err));
                                Type::new_with_unknown()
                            }
                            TupleIndexCheckResult::NegativeIndexOutOfBound => {
                                let err = TupleIndexOutOfBoundError::new(
                                    sub_types.len(),
                                    index_expr.range(),
                                );
                                self.log_error(Diagnostics::TupleIndexOutOfBound(err));
                                Type::new_with_unknown()
                            }
                        }
                    }
                    None => {
                        let err = UnresolvedIndexExpressionInTupleError::new(index_expr.range());
                        self.log_error(Diagnostics::UnresolvedIndexExpressionInTuple(err));
                        Type::new_with_unknown()
                    }
                }
            }
            CoreExpressionNode::Binary(_) | CoreExpressionNode::Comparison(_) => {
                let err = InvalidIndexExpressionForTupleError::new(index_expr.range());
                self.log_error(Diagnostics::InvalidIndexExpressionForTuple(err));
                Type::new_with_unknown()
            }
        }
    }

    fn check_index_access(&mut self, index_access: &IndexAccessNode) -> (Type, Option<Type>) {
        let core_index_access = index_access.core_ref();
        let atom = &core_index_access.atom;
        let (atom_type_obj, _) = self.check_atom(atom);
        let index_expr = &core_index_access.index;
        let index_type_obj = self.check_expr(index_expr);
        let result = match atom_type_obj.0.as_ref() {
            CoreType::Tuple(tuple) => {
                return (
                    self.check_index_access_for_tuple_ty(tuple, index_expr),
                    Some(atom_type_obj),
                )
            }
            CoreType::Array(array) => {
                if index_type_obj.is_int() {
                    Some(array.element_type.clone())
                } else {
                    None
                }
            }
            CoreType::Atomic(atomic) => match atomic {
                Atomic::String => {
                    if index_type_obj.is_int() {
                        Some(Type::new_with_atomic("str"))
                    } else {
                        None
                    }
                }
                _ => None,
            },
            CoreType::HashMap(hashmap) => {
                // TODO - instead of having `is_hashable` check, replace it with `is_type_bounded_by` `Hash` interface
                if index_type_obj.is_eq(&hashmap.key_type) && index_type_obj.is_hashable() {
                    Some(hashmap.value_type.clone())
                } else {
                    None
                }
            }
            _ => None,
        };
        match result {
            Some(ty) => (ty, Some(atom_type_obj)),
            None => {
                let err = ExpressionIndexingNotValidError::new(
                    atom_type_obj.to_string(&self.semantic_state_db.interner),
                    index_type_obj.to_string(&self.semantic_state_db.interner),
                    atom.range(),
                    index_expr.range(),
                );
                self.log_error(Diagnostics::ExpressionIndexingNotValid(err));
                (Type::new_with_unknown(), Some(atom_type_obj))
            }
        }
    }

    pub fn check_atom(&mut self, atom: &AtomNode) -> (Type, Option<Type>) {
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

    pub fn check_r_assign(&mut self, r_assign: &RAssignmentNode) -> Type {
        let core_r_assign = r_assign.core_ref();
        self.check_expr(&core_r_assign.expr.core_ref().expr)
    }

    pub fn check_r_variable_declaration(
        &mut self,
        r_variable_decl: &RVariableDeclarationNode,
    ) -> Type {
        let core_r_variable_decl = r_variable_decl.core_ref();
        match core_r_variable_decl {
            CoreRVariableDeclarationNode::Expression(expr_stmt) => {
                self.check_expr(&expr_stmt.core_ref().expr)
            }
            CoreRVariableDeclarationNode::Lambda(lambda) => {
                let body = &lambda.core_ref().body.core_ref();
                self.check_callable_body(&body.prototype, &body.block, false);
                self.type_of_lambda(lambda)
            }
        }
    }

    pub fn check_token(&self, token: &TokenNode, kind: AtomicTokenExprKind) -> Type {
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

    pub fn check_array_expr(&mut self, array_expr: &ArrayExpressionNode) -> Type {
        let core_array_expr = array_expr.core_ref();
        match &core_array_expr.initials {
            Some(initials) => {
                let mut initials_iter = initials.iter();
                let first_expr_ty = match initials_iter.next() {
                    Some(expr) => self.check_expr(expr),
                    None => unreachable!(),
                };
                for expr in initials_iter {
                    let ty = self.check_expr(expr);
                    if !ty.is_eq(&first_expr_ty) {
                        let err = IncorrectExpressionTypeError::new(
                            first_expr_ty.to_string(&self.semantic_state_db.interner),
                            ty.to_string(&self.semantic_state_db.interner),
                            expr.range(),
                        );
                        self.log_error(Diagnostics::IncorrectExpressionType(err));
                    }
                }
                Type::new_with_array(first_expr_ty)
            }
            None => {
                //let err = ExpressionTypeCannotBeInferredError::new(array_expr.range());
                //self.log_error(Diagnostics::ExpressionTypeCannotBeInferred(err));
                //Type::new_with_array(Type::new_with_unknown())
                todo!()
            }
        }
    }

    pub fn check_hashmap_expr(&mut self, hashmap_expr: &HashMapExpressionNode) -> Type {
        let core_hashmap_expr = hashmap_expr.core_ref();
        match &core_hashmap_expr.initials {
            Some(initials) => {
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
                    if !key_ty.is_eq(&first_key_ty) {
                        let err = IncorrectExpressionTypeError::new(
                            first_key_ty.to_string(&self.semantic_state_db.interner),
                            key_ty.to_string(&self.semantic_state_db.interner),
                            core_key_value_pair.key_expr.range(),
                        );
                        self.log_error(Diagnostics::IncorrectExpressionType(err));
                    }
                    if !value_ty.is_eq(&first_value_ty) {
                        let err = IncorrectExpressionTypeError::new(
                            first_value_ty.to_string(&self.semantic_state_db.interner),
                            value_ty.to_string(&self.semantic_state_db.interner),
                            core_key_value_pair.value_expr.range(),
                        );
                        self.log_error(Diagnostics::IncorrectExpressionType(err));
                    }
                }
                Type::new_with_hashmap(first_key_ty, first_value_ty)
            }
            None => {
                //let err = ExpressionTypeCannotBeInferredError::new(hashmap_expr.range());
                //self.log_error(Diagnostics::ExpressionTypeCannotBeInferred(err));
                //Type::new_with_array(Type::new_with_unknown())
                todo!()
            }
        }
    }

    pub fn check_tuple_expr(&mut self, tuple_expr: &TupleExpressionNode) -> Type {
        let mut sub_types = vec![];
        for expr in tuple_expr.core_ref().initials.iter() {
            sub_types.push(self.check_expr(expr));
        }
        Type::new_with_tuple(sub_types)
    }

    pub fn check_atomic_expr(&mut self, atomic_expr: &AtomicExpressionNode) -> Type {
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

    pub fn check_only_unary_expr(&mut self, only_unary_expr: &OnlyUnaryExpressionNode) -> Type {
        let core_only_unary_expr = only_unary_expr.core_ref();
        let unary_expr = &core_only_unary_expr.unary_expr;
        let operand_type = self.check_unary_expr(unary_expr);
        let operator = &core_only_unary_expr.operator;
        let operator_kind = &core_only_unary_expr.operator_kind;
        match operator_kind {
            UnaryOperatorKind::Plus | UnaryOperatorKind::Minus => {
                if operand_type.is_numeric() {
                    operand_type
                } else {
                    let err = UnaryOperatorInvalidUseError::new(
                        operand_type.to_string(&self.semantic_state_db.interner),
                        "numeric (`int`, `float`)",
                        "`+` or `-`",
                        unary_expr.range(),
                        operator.range(),
                    );
                    self.log_error(Diagnostics::UnaryOperatorInvalidUse(err));
                    Type::new_with_unknown()
                }
            }
            UnaryOperatorKind::Not => {
                if operand_type.is_bool() {
                    operand_type
                } else {
                    let err = UnaryOperatorInvalidUseError::new(
                        operand_type.to_string(&self.semantic_state_db.interner),
                        "boolean",
                        "`not`",
                        unary_expr.range(),
                        operator.range(),
                    );
                    self.log_error(Diagnostics::UnaryOperatorInvalidUse(err));
                    Type::new_with_unknown()
                }
            }
        }
    }

    pub fn check_unary_expr(&mut self, unary_expr: &UnaryExpressionNode) -> Type {
        let core_unary_expr = unary_expr.core_ref();
        match core_unary_expr {
            CoreUnaryExpressionNode::Atomic(atomic) => self.check_atomic_expr(atomic),
            CoreUnaryExpressionNode::Unary(unary) => self.check_only_unary_expr(unary),
        }
    }

    pub fn check_binary_expr(&mut self, binary_expr: &BinaryExpressionNode) -> Type {
        let core_binary_expr = binary_expr.core_ref();
        let left_expr = &core_binary_expr.left_expr;
        let right_expr = &core_binary_expr.right_expr;
        let l_type = self.check_expr(left_expr);
        let operator = &core_binary_expr.operator;
        let operator_kind = &core_binary_expr.operator_kind;
        let r_type = self.check_expr(right_expr);
        let result = self.is_binary_operation_valid(&l_type, &r_type, operator_kind);
        match result {
            Some(type_obj) => type_obj,
            None => {
                let err = BinaryOperatorInvalidOperandsError::new(
                    l_type.to_string(&self.semantic_state_db.interner),
                    r_type.to_string(&self.semantic_state_db.interner),
                    left_expr.range(),
                    right_expr.range(),
                    operator.range(),
                );
                self.log_error(Diagnostics::BinaryOperatorInvalidOperands(err));
                Type::new_with_unknown()
            }
        }
    }

    pub fn check_comp_expr(&mut self, comp_expr: &ComparisonNode) -> Type {
        let core_comp_expr = comp_expr.core_ref();
        let operands = &core_comp_expr.operands;
        let operators = &core_comp_expr.operators;
        let operands_len = operands.len();
        for index in 1..operands_len {
            let left_expr = &operands[index - 1];
            let right_expr = &operands[index];
            let l_type = self.check_expr(left_expr);
            let r_type = self.check_expr(right_expr);
            let operator = &operators[index - 1];
            let operator_kind = operators[index - 1]
                .is_binary_operator()
                .expect("operator token is always valid");
            debug_assert!(
                operator_kind.is_comparison(),
                "all the operators in `ComparisonNode` should be comparison operators"
            );
            let result = self.is_binary_operation_valid(&l_type, &r_type, &operator_kind);
            match result {
                Some(type_obj) => match type_obj.0.as_ref() {
                    CoreType::Atomic(atomic) => debug_assert!(atomic.is_bool()),
                    CoreType::Unknown => return Type::new_with_unknown(),
                    _ => unreachable!("comparison operator always result into `bool` type"),
                },
                None => {
                    let err = BinaryOperatorInvalidOperandsError::new(
                        l_type.to_string(&self.semantic_state_db.interner),
                        r_type.to_string(&self.semantic_state_db.interner),
                        left_expr.range(),
                        right_expr.range(),
                        operator.range(),
                    );
                    self.log_error(Diagnostics::BinaryOperatorInvalidOperands(err));
                    return Type::new_with_unknown();
                }
            }
        }
        Type::new_with_atomic(BOOL)
    }

    pub fn check_expr(&mut self, expr: &ExpressionNode) -> Type {
        let core_expr = expr.core_ref();
        match core_expr {
            CoreExpressionNode::Unary(unary_expr) => self.check_unary_expr(unary_expr),
            CoreExpressionNode::Binary(binary_expr) => self.check_binary_expr(binary_expr),
            CoreExpressionNode::Comparison(comparison_expr) => {
                self.check_comp_expr(comparison_expr)
            }
        }
    }

    pub fn check_assignment(&mut self, assignment: &AssignmentNode) {
        let core_assignment = assignment.core_ref();
        let (l_type, r_assign, range) = match core_assignment {
            CoreAssignmentNode::Ok(ok_assignment) => {
                let core_ok_assignment = ok_assignment.core_ref();
                let l_expr = &core_ok_assignment.l_atom;
                let (l_type, interior_atom_type) = self.check_atom(l_expr);
                if let CoreAtomNode::IndexAccess(l_index_expr) = l_expr.core_ref() {
                    if let Some(interior_atom_type) = interior_atom_type {
                        if interior_atom_type.is_immutable() {
                            let err = ImmutableTypeNotAssignableError::new(
                                interior_atom_type.to_string(&self.semantic_state_db.interner),
                                l_index_expr.core_ref().atom.range(),
                            );
                            self.log_error(Diagnostics::ImmutableTypeNotAssignable(err));
                        }
                    }
                }
                let r_assign = &core_ok_assignment.r_assign;
                (l_type, r_assign, l_expr.range())
            }
            CoreAssignmentNode::InvalidLValue(invalid_l_value) => {
                let core_invalid_l_value = invalid_l_value.core_ref();
                let expr = &core_invalid_l_value.l_expr;
                let r_assign = &core_invalid_l_value.r_assign;
                let l_type = self.check_expr(expr);
                (l_type, r_assign, expr.range())
            }
        };
        let r_type = self.check_r_assign(r_assign);
        if r_type.is_void() {
            let err = RightSideWithVoidTypeNotAllowedError::new(r_assign.range());
            self.log_error(Diagnostics::RightSideWithVoidTypeNotAllowed(err));
            return;
        }
        if !l_type.is_eq(&r_type) {
            let err = MismatchedTypesOnLeftRightError::new(
                l_type.to_string(&self.semantic_state_db.interner),
                r_type.to_string(&self.semantic_state_db.interner),
                range,
                r_assign.range(),
            );
            self.log_error(Diagnostics::MismatchedTypesOnLeftRight(err));
        }
    }

    pub fn check_variable_decl(&mut self, variable_decl: &VariableDeclarationNode) {
        let core_variable_decl = variable_decl.core_ref();
        let r_variable_decl = &core_variable_decl.r_node;
        let r_type = self.check_r_variable_declaration(r_variable_decl);
        if r_type.is_void() {
            let err = RightSideWithVoidTypeNotAllowedError::new(r_variable_decl.range());
            self.log_error(Diagnostics::RightSideWithVoidTypeNotAllowed(err));
        }
        if let CoreIdentifierInDeclNode::Ok(ok_identifier) = core_variable_decl.name.core_ref() {
            if let Some(symbol_data) = self
                .semantic_state_db
                .get_variable_symbol_data_for_identifier_in_decl(ok_identifier)
            {
                let mut symbol_data_mut_ref = symbol_data.get_core_mut_ref();
                let variable_ty = &symbol_data_mut_ref.data_type;
                if variable_ty.is_unset() {
                    // TODO - check if the `r_type` is ambigious type
                    // enforce availablity of type annotation here!
                    symbol_data_mut_ref.set_data_type(&r_type);
                } else if !variable_ty.is_eq(&r_type) {
                    let err = RightSideExpressionTypeMismatchedWithTypeFromAnnotationError::new(
                        variable_ty.to_string(&self.semantic_state_db.interner),
                        r_type.to_string(&self.semantic_state_db.interner),
                        core_variable_decl.name.range(),
                        r_variable_decl.range(),
                    );
                    self.log_error(
                        Diagnostics::RightSideExpressionTypeMismatchedWithTypeFromAnnotation(err),
                    )
                }
            }
        };
    }

    pub fn check_callable_prototype(&self, callable_prototype: &CallablePrototypeNode) -> Type {
        let core_callable_prototype = callable_prototype.0.as_ref();
        let return_type_node = &core_callable_prototype.return_type;

        match return_type_node {
            Some((_, return_type_expr)) => self.type_obj_from_expression(return_type_expr).0,
            None => Type::new_with_void(),
        }
    }

    pub fn check_callable_body(
        &mut self,
        prototype: &CallablePrototypeNode,
        body: &BlockNode,
        is_constructor: bool,
    ) {
        // let core_callable_body = callable_body.0.as_ref();
        let return_type_obj = self.check_callable_prototype(prototype);
        self.context
            .func_stack
            .push((is_constructor, return_type_obj.clone()));
        let mut has_return_stmt: Option<TextRange> = None;
        for stmt in body.0.as_ref().stmts.as_ref() {
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
        if is_constructor {
            match has_return_stmt {
                Some(return_stmt_range) => {
                    let err =
                        ExplicitReturnStatementFoundInConstructorBodyError::new(return_stmt_range);
                    self.log_error(Diagnostics::ExplicitReturnStatementFoundInConstructorBody(
                        err,
                    ));
                }
                None => {}
            }
        } else if has_return_stmt.is_none() && !return_type_obj.is_void() {
            let (_, return_type_node) = prototype.core_ref().return_type.as_ref().unwrap();
            let err = NoReturnStatementInFunctionError::new(return_type_node.range());
            self.log_error(Diagnostics::NoReturnStatementInFunction(err));
        }
        self.context.func_stack.pop();
    }

    pub fn check_bounded_method(&mut self, bounded_method_wrapper: &BoundedMethodWrapperNode) {
        let core_bounded_method_wrapper = bounded_method_wrapper.0.as_ref();
        let is_constructor = match self
            .semantic_state_db
            .get_bounded_kind_ref(bounded_method_wrapper)
        {
            Some(bounded_kind) => match bounded_kind {
                BoundedMethodKind::Constructor => true,
                _ => false,
            },
            None => false,
        };
        let body = &core_bounded_method_wrapper
            .func_decl
            .core_ref()
            .body
            .core_ref();
        self.check_callable_body(&body.prototype, &body.block, is_constructor);
    }

    pub fn check_return_stmt(&mut self, return_stmt: &ReturnStatementNode) {
        let core_return_stmt = return_stmt.core_ref();
        let func_stack_len = self.context.func_stack.len();
        if func_stack_len == 0 {
            let err = InvalidReturnStatementError::new(return_stmt.range());
            self.log_error(Diagnostics::InvalidReturnStatement(err));
        }
        let expr = &core_return_stmt.expr;
        let expr_type_obj = match expr {
            Some(expr) => self.check_expr(expr),
            _ => Type::new_with_void(),
        };
        let (is_constructor, expected_type_obj) = &self.context.func_stack[func_stack_len - 1];
        if *is_constructor {
            let err = ExplicitReturnStatementFoundInConstructorBodyError::new(return_stmt.range());
            self.log_error(Diagnostics::ExplicitReturnStatementFoundInConstructorBody(
                err,
            ));
        } else if !expr_type_obj.is_eq(expected_type_obj) {
            let err = MismatchedReturnTypeError::new(
                expected_type_obj.to_string(&self.semantic_state_db.interner),
                expr_type_obj.to_string(&self.semantic_state_db.interner),
                core_return_stmt.return_keyword.range(),
            );
            self.log_error(Diagnostics::MismatchedReturnType(err));
        }
    }

    pub fn check_struct_declaration(&mut self, struct_decl: &StructDeclarationNode) {
        let core_struct_decl = struct_decl.core_ref();
        if let CoreIdentifierInDeclNode::Ok(ok_identifier) = core_struct_decl.name.core_ref() {
            if let Some(symbol_data) = self
                .semantic_state_db
                .get_type_symbol_data_for_identifier_in_decl(ok_identifier)
            {
                let symbol_data_ref = symbol_data.get_core_ref();
                let struct_data = symbol_data_ref.get_struct_data_ref();
                let implementing_interfaces = &struct_data.implementing_interfaces;
                if let Some(implementing_interfaces) = implementing_interfaces {
                    let struct_methods = struct_data.get_methods_ref();
                    for (interface_obj, range) in &implementing_interfaces.interfaces {
                        let (_, interface_concrete_symbol_data) = interface_obj.get_core_ref();
                        let concrete_types = &interface_concrete_symbol_data.concrete_types;
                        let interface_data =
                            &*interface_concrete_symbol_data.symbol_data.get_core_ref();
                        let partial_concrete_interface_methods = interface_data
                            .get_partially_concrete_interface_methods(concrete_types.as_ref());
                        if let Err((missing_interface_method_names, errors)) =
                            partial_concrete_interface_methods
                                .is_struct_implements_interface_methods(struct_methods)
                        {
                            let err = InterfaceMethodsInStructCheckError::new(
                                missing_interface_method_names,
                                errors,
                                interface_obj.to_string(&self.semantic_state_db.interner),
                                *range,
                                &self.semantic_state_db.interner,
                            );
                            self.log_error(Diagnostics::InterfaceMethodsInStructCheck(err));
                        }
                    }
                }
            }
        };
        self.walk_block(&core_struct_decl.block);
    }

    fn check_conditional_block(&mut self, conditional_block: &ConditionalBlockNode) {
        let core_conditional_block = conditional_block.core_ref();
        let condition_expr = &core_conditional_block.condition_expr;
        let ty = self.check_expr(condition_expr);
        if !ty.is_bool() {
            let err = IncorrectExpressionTypeError::new(
                "bool".to_string(),
                ty.to_string(&self.semantic_state_db.interner),
                condition_expr.range(),
            );
            self.log_error(Diagnostics::IncorrectExpressionType(err));
        }
        self.walk_block(&core_conditional_block.block);
    }

    pub fn check_conditional_stmt(&mut self, conditional_stmt: &ConditionalStatementNode) {
        let core_conditional_stmt = conditional_stmt.core_ref();
        self.check_conditional_block(&core_conditional_stmt.if_block);
        for elif in &core_conditional_stmt.elifs {
            self.check_conditional_block(elif);
        }
        if let Some((_, _, else_block)) = &core_conditional_stmt.else_block {
            self.walk_block(else_block);
        }
    }

    pub fn check_stmt(&mut self, stmt: &StatementNode) {
        match stmt.core_ref() {
            CoreStatementNode::Expression(expr_stmt) => {
                let core_expr_stmt = expr_stmt.core_ref();
                self.check_expr(&core_expr_stmt.expr);
            }
            CoreStatementNode::Assignment(assignment) => {
                self.check_assignment(assignment);
            }
            CoreStatementNode::VariableDeclaration(variable_decl) => {
                self.check_variable_decl(variable_decl);
            }
            CoreStatementNode::FunctionWrapper(func_wrapper) => {
                let body = &func_wrapper.core_ref().func_decl.core_ref().body.core_ref();
                self.check_callable_body(&body.prototype, &body.block, false);
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
            CoreStatementNode::TypeDeclaration(type_decl) => match type_decl.core_ref() {
                CoreTypeDeclarationNode::Struct(struct_decl) => {
                    self.check_struct_declaration(struct_decl);
                }
                CoreTypeDeclarationNode::Lambda(_)
                | CoreTypeDeclarationNode::Enum(_)
                | CoreTypeDeclarationNode::MissingTokens(_) => {}
            },
            CoreStatementNode::InterfaceDeclaration(interface_decl) => {
                self.walk_block(&interface_decl.core_ref().block);
            }
            CoreStatementNode::InterfaceMethodPrototypeWrapper(
                interface_method_prototype_wrapper,
            ) => {
                let core_interface_method_prototype_wrapper =
                    interface_method_prototype_wrapper.core_ref();
                if let InterfaceMethodTerminalNode::HasDefaultBody(_, optional_default_body) =
                    &core_interface_method_prototype_wrapper.terminal
                {
                    self.check_callable_body(
                        &core_interface_method_prototype_wrapper.prototype,
                        optional_default_body,
                        false,
                    );
                }
            }
            CoreStatementNode::StructPropertyDeclaration(_)
            | CoreStatementNode::EnumVariantDeclaration(_)
            | CoreStatementNode::Break(_)
            | CoreStatementNode::Continue(_) => (),
        }
    }

    pub fn log_params_type_and_count_check_error(
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
                self.log_error(Diagnostics::LessParamsCount(err));
            }
            PrototypeEquivalenceCheckError::MoreParams(expected_params_count) => {
                let err = MoreParamsCountError::new(expected_params_count, range);
                self.log_error(Diagnostics::MoreParamsCount(err));
            }
            PrototypeEquivalenceCheckError::MismatchedType(params_vec) => {
                let err = MismatchedParamTypeError::new(params_vec);
                self.log_error(Diagnostics::MismatchedParamType(err));
            }
            PrototypeEquivalenceCheckError::NotAllConcreteTypesInferred => {
                let err = NotAllConcreteTypesInferredError::new(range);
                self.log_error(Diagnostics::NotAllConcreteTypesInferred(err))
            }
            PrototypeEquivalenceCheckError::TypeInferenceFailed
            | PrototypeEquivalenceCheckError::ConcreteTypesCannotBeInferred => {
                let err = TypeInferenceFailedError::new(range);
                self.log_error(Diagnostics::TypeInferenceFailed(err));
            }
            PrototypeEquivalenceCheckError::InferredTypesNotBoundedByInterfaces(
                err_strs,
                concrete_types,
            ) => {
                let err = InferredTypesNotBoundedByInterfacesError::new(
                    range,
                    err_strs,
                    concrete_types,
                    &self.semantic_state_db.interner,
                );
                self.log_error(Diagnostics::InferredTypesNotBoundedByInterfaces(err));
            }
        }
    }
}

impl Visitor for TypeChecker {
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
