// See `https://www.csd.uwo.ca/~mmorenom/CS447/Lectures/TypeChecking.html/node1.html` for information about various cases that type-checker needs to
// cover and the representation of type expressions in terms of type objects.

use super::resolver::Resolver;
use crate::ast::ast::{
    CoreIdentifierInDeclNode, CoreIdentifierInUseNode, InterfaceMethodTerminalNode,
    OkIdentifierInDeclNode, OkIdentifierInUseNode, StructDeclarationNode,
};
use crate::scope::concrete::core::ConcretizationContext;
use crate::scope::core::{AbstractConcreteTypesHandler, AbstractSymbolMetaData, GenericTypeParams};
use crate::scope::function::PrototypeConcretizationResult;
use crate::scope::handler::ConcreteSymbolDataEntry;
use crate::scope::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::types::core::AbstractNonStructTypes;
use crate::types::lambda::Lambda;
use crate::{
    ast::{
        ast::{
            ASTNode, AssignmentNode, AtomNode, AtomStartNode, AtomicExpressionNode,
            BinaryExpressionNode, BlockNode, BoundedMethodKind, BoundedMethodWrapperNode,
            CallablePrototypeNode, ComparisonNode, CoreAssignmentNode, CoreAtomNode,
            CoreAtomStartNode, CoreAtomicExpressionNode, CoreExpressionNode,
            CoreRVariableDeclarationNode, CoreSelfKeywordNode, CoreStatemenIndentWrapperNode,
            CoreStatementNode, CoreTokenNode, CoreTypeDeclarationNode, CoreUnaryExpressionNode,
            ExpressionNode, LambdaDeclarationNode, NameTypeSpecNode, Node, OnlyUnaryExpressionNode,
            RAssignmentNode, RVariableDeclarationNode, ReturnStatementNode, StatementNode,
            SymbolSeparatedSequenceNode, TokenNode, TypeExpressionNode, TypeResolveKind,
            UnaryExpressionNode, VariableDeclarationNode,
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
            ExpressionNotCallableError, IdentifierNotCallableError,
            ImmutableTypeNotAssignableError, InvalidIndexExpressionForTupleError,
            InvalidReturnStatementError, LessParamsCountError, MismatchedParamTypeError,
            MismatchedReturnTypeError, MismatchedTypesOnLeftRightError, MoreParamsCountError,
            NoReturnStatementInFunctionError, PropertyDoesNotExistError, PropertyNotSupportedError,
            RightSideWithVoidTypeNotAllowedError, StructFieldNotCallableError,
            TupleIndexOutOfBoundError, UnaryOperatorInvalidUseError,
            UnresolvedIndexExpressionInTupleError,
        },
        helper::PropertyKind,
    },
    lexer::token::{BinaryOperatorKind, UnaryOperatorKind},
    scope::{
        function::CallablePrototypeData, handler::NamespaceHandler,
        types::core::UserDefinedTypeData,
    },
    types::{
        atomic::Atomic,
        core::{AbstractType, CoreType, Type},
    },
};
use std::cell::UnsafeCell;
use text_size::TextRange;

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

#[derive(Clone)]
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
pub enum StructConstructorPrototypeCheckResult {
    Basic((Result<(), PrototypeEquivalenceCheckError>, Type)),
    Inferred((Vec<Type>, bool)),
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
    namespace_handler: NamespaceHandler,
}

impl TypeChecker {
    pub fn new(code: JarvilCode, namespace_handler: NamespaceHandler) -> Self {
        TypeChecker {
            code,
            errors: UnsafeCell::new(vec![]),
            context: Context { func_stack: vec![] },
            namespace_handler,
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
    ) -> (NamespaceHandler, JarvilCode) {
        let core_block = ast.0.as_ref();
        for stmt in &*core_block.stmts.as_ref() {
            self.walk_stmt_indent_wrapper(stmt);
        }
        unsafe {
            let errors_ref = &mut *self.errors.get();
            global_errors.append(errors_ref);
        };
        (self.namespace_handler, self.code)
    }

    pub fn is_resolved(&self, node: &OkIdentifierInDeclNode) -> bool {
        self.namespace_handler
            .identifier_in_decl_binding_table
            .get(node)
            .is_some()
    }

    pub fn type_obj_from_expression(&self, type_expr: &TypeExpressionNode) -> Type {
        match type_expr.type_obj_after_resolved(&self.code, &self.namespace_handler) {
            TypeResolveKind::Resolved(type_obj) => {
                type DummyFnType = fn(&mut Resolver, TextRange);
                return Resolver::pre_type_checking::<DummyFnType>(&type_obj, type_expr, None);
            }
            TypeResolveKind::Unresolved(_) => return Type::new_with_unknown(),
            TypeResolveKind::Invalid => Type::new_with_unknown(),
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
                let name = &core_param.name;
                if let CoreIdentifierInDeclNode::Ok(ok_identifier) = name.core_ref() {
                    if self.is_resolved(ok_identifier) {
                        let type_obj = self.type_obj_from_expression(&core_param.data_type);
                        if type_obj.has_generics() {
                            generics_containing_params_indexes.push(params_vec.len());
                        }
                        params_vec.push(type_obj);
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
                .namespace_handler
                .get_variable_symbol_data_for_identifier_in_decl(ok_identifier)
            {
                Some(symbol_data) => return symbol_data.get_core_ref().data_type.clone(),
                None => self.params_and_return_type_obj_from_expr(return_type, params),
            },
            _ => self.params_and_return_type_obj_from_expr(return_type, params),
        };
        let lambda_type_obj = Type::new_with_lambda_unnamed(CallablePrototypeData::new(
            params_vec,
            return_type,
            is_concretization_required,
        ));
        return lambda_type_obj;
    }

    pub fn is_unary_expr_int_valued(&self, unary: &UnaryExpressionNode) -> Option<i32> {
        match unary.core_ref() {
            CoreUnaryExpressionNode::Unary(unary) => {
                let core_unary = unary.core_ref();
                let operator_kind = &core_unary.operator_kind;
                let operand_value = self.is_unary_expr_int_valued(&core_unary.unary_expr);
                match operand_value {
                    Some(value) => match operator_kind {
                        UnaryOperatorKind::Plus => return Some(value),
                        UnaryOperatorKind::Minus => return Some(-value),
                        UnaryOperatorKind::Not => return None,
                    },
                    None => return None,
                }
            }
            CoreUnaryExpressionNode::Atomic(atomic) => match atomic.core_ref() {
                CoreAtomicExpressionNode::Integer(integer_valued_token) => {
                    match integer_valued_token.core_ref() {
                        CoreTokenNode::Ok(ok_token) => {
                            let value = ok_token.token_value(&self.code);
                            match value.parse::<i32>() {
                                Ok(value) => return Some(value),
                                Err(_) => return None,
                            }
                        }
                        _ => return None,
                    }
                }
                _ => return None,
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
                return TupleIndexCheckResult::Ok(index_value as usize);
            } else {
                return TupleIndexCheckResult::PositiveIndexOutOfBound;
            }
        } else {
            if -(tuple_len as i32) <= index_value {
                return TupleIndexCheckResult::Ok((tuple_len as i32 + index_value) as usize);
            } else {
                return TupleIndexCheckResult::NegativeIndexOutOfBound;
            }
        }
    }

    pub fn is_indexable_with_type(&self, other_ty: &Type, index_type: &Type) -> Option<Type> {
        // NOTE - case for `tuple` is already handled in the calling function
        match other_ty.0.as_ref() {
            CoreType::Array(array) => {
                if index_type.is_int() {
                    return Some(array.element_type.clone());
                } else {
                    return None;
                }
            }
            CoreType::Atomic(atomic) => match atomic {
                Atomic::String => {
                    if index_type.is_int() {
                        return Some(Type::new_with_atomic("str"));
                    } else {
                        return None;
                    }
                }
                _ => return None,
            },
            CoreType::HashMap(hashmap) => {
                if index_type.is_eq(&hashmap.key_type) && index_type.is_hashable() {
                    return Some(hashmap.value_type.clone());
                } else {
                    return None;
                }
            }
            _ => return None,
        }
    }

    pub fn is_binary_operation_valid(
        &self,
        l_type: &Type,
        r_type: &Type,
        operator_kind: &BinaryOperatorKind,
    ) -> Option<Type> {
        if l_type.is_unknown() || r_type.is_unknown() {
            return Some(Type::new_with_unknown());
        }
        let result = l_type.check_operator(r_type, operator_kind);
        result
    }

    pub fn infer_concrete_types_from_arguments(
        &self,
        generic_type_decls: &GenericTypeParams,
        expected_prototype: &CallablePrototypeData,
        received_params: &Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
        generic_ty_decl_place: GenericTypeDeclarationPlaceCategory,
    ) -> Result<(Vec<Type>, bool), PrototypeEquivalenceCheckError> {
        // (inferred_concrete_types, params_ty_vec)
        match received_params {
            Some(received_params) => {
                let generic_type_decls_len = generic_type_decls.len();
                let mut inferred_concrete_types: Vec<InferredConcreteTypesEntry> =
                    vec![InferredConcreteTypesEntry::Uninferred; generic_type_decls_len];
                let mut num_inferred_types = 0; // this should be `len_concrete_types` at the end of inference process
                let mut has_generics = false;
                let received_params_iter = received_params.iter();
                let expected_params = &expected_prototype.params;
                let expected_params_len = expected_params.len();
                let mut mismatch_types_vec: Vec<(String, String, usize, TextRange)> = vec![];
                let mut params_len = 0;
                for (index, received_param) in received_params_iter.enumerate() {
                    let param_ty = self.check_expr(&received_param);
                    if index >= expected_params_len {
                        return Err(PrototypeEquivalenceCheckError::MoreParams(
                            expected_params_len,
                        ));
                    }
                    let expected_ty = &expected_params[index];
                    if expected_ty.has_generics() {
                        let inference_result = expected_ty.try_infer_type(
                            &param_ty,
                            &mut inferred_concrete_types,
                            &mut num_inferred_types,
                            generic_ty_decl_place,
                            &mut has_generics,
                        );
                        if let Err(()) = inference_result {
                            return Err(PrototypeEquivalenceCheckError::TypeInferenceFailed);
                        }
                    } else {
                        if !param_ty.is_eq(expected_ty) {
                            mismatch_types_vec.push((
                                expected_ty.to_string(),
                                param_ty.to_string(),
                                index + 1,
                                received_param.range(),
                            ));
                        }
                    }
                    params_len = params_len + 1;
                }
                if expected_params_len > params_len {
                    return Err(PrototypeEquivalenceCheckError::LessParams((
                        expected_params_len,
                        params_len,
                    )));
                } else if mismatch_types_vec.len() > 0 {
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
                    .rev()
                    .collect();
                let mut error_strs: Vec<(String, String)> = vec![]; // Vec of (inferred_ty string, interface_bounds string)
                for (index, inferred_ty) in unpacked_inferred_concrete_types.iter().enumerate() {
                    let interface_bounds = &generic_type_decls.0[index].1;
                    if !inferred_ty.is_type_bounded_by_interfaces(interface_bounds) {
                        error_strs.push((inferred_ty.to_string(), interface_bounds.to_string()));
                    }
                }
                if error_strs.len() > 0 {
                    return Err(
                        PrototypeEquivalenceCheckError::InferredTypesNotBoundedByInterfaces(
                            error_strs,
                            unpacked_inferred_concrete_types
                        ),
                    );
                }
                return Ok((unpacked_inferred_concrete_types, has_generics));
            }
            None => Err(PrototypeEquivalenceCheckError::ConcreteTypesCannotBeInferred),
        }
    }

    pub fn check_params_type_and_count(
        &self,
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
                    let param_type_obj = self.check_expr(&received_param);
                    if index >= expected_params_len {
                        return Err(PrototypeEquivalenceCheckError::MoreParams(
                            expected_params_len,
                        ));
                    }
                    let expected_param_type = &expected_param_data[index];
                    if !param_type_obj.is_eq(expected_param_type) {
                        mismatch_types_vec.push((
                            expected_param_type.to_string(),
                            param_type_obj.to_string(),
                            index + 1,
                            received_param.range(),
                        ));
                    }
                    index = index + 1;
                }
                if index < expected_params_len {
                    return Err(PrototypeEquivalenceCheckError::LessParams((
                        expected_params_len,
                        index,
                    )));
                } else if mismatch_types_vec.len() > 0 {
                    return Err(PrototypeEquivalenceCheckError::MismatchedType(
                        mismatch_types_vec,
                    ));
                }
                return Ok(());
            }
            None => {
                if expected_params_len != 0 {
                    return Err(PrototypeEquivalenceCheckError::LessParams((
                        expected_params_len,
                        0,
                    )));
                } else {
                    return Ok(());
                }
            }
        }
    }

    pub fn check_atom_start(&self, atom_start: &AtomStartNode) -> Type {
        let core_atom_start = atom_start.core_ref();
        match core_atom_start {
            CoreAtomStartNode::Identifier(token) => match token.core_ref() {
                CoreIdentifierInUseNode::Ok(ok_identifier) => {
                    match self
                        .namespace_handler
                        .get_variable_symbol_data_for_identifier_in_use(ok_identifier)
                    {
                        Some(variable_symbol_data) => {
                            return variable_symbol_data.get_core_ref().data_type.clone()
                        }
                        None => return Type::new_with_unknown(),
                    }
                }
                _ => Type::new_with_unknown(),
            },
            CoreAtomStartNode::SelfKeyword(self_keyword) => {
                let core_self_keyword = self_keyword.core_ref();
                match core_self_keyword {
                    CoreSelfKeywordNode::Ok(ok_self_keyword) => {
                        match self
                            .namespace_handler
                            .get_self_keyword_symbol_data_ref(ok_self_keyword)
                        {
                            Some(symbol_data) => {
                                return symbol_data.get_core_ref().data_type.clone()
                            }
                            None => return Type::new_with_unknown(),
                        }
                    }
                    _ => Type::new_with_unknown(),
                }
            }
            CoreAtomStartNode::Call(call_expr) => {
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
                        .namespace_handler
                        .get_symbol_data_for_identifier_in_use(ok_identifier)
                    {
                        let (result, return_type) = match symbol_data {
                            ConcreteSymbolDataEntry::Function(func_symbol_data) => {
                                let func_data = &*func_symbol_data.get_core_ref();
                                let index = func_symbol_data.index;

                                let prototype_result = match index {
                                    Some(index) => {
                                        // CASE 1
                                        let concrete_types = func_data.get_concrete_types(index);
                                        let concrete_prototype = func_data
                                            .prototype
                                            .concretize_prototype(&vec![], &concrete_types.0);
                                        CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(concrete_prototype)
                                    }
                                    None => {
                                        match &func_data.generics.generics_spec {
                                            Some(generic_type_decls) => {
                                                // CASE 2
                                                CallExpressionPrototypeEquivalenceCheckResult::NeedsTypeInference(generic_type_decls)
                                            }
                                            // CASE 4
                                            None => CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(PrototypeConcretizationResult::UnConcretized(
                                                &func_data.prototype,
                                            )),
                                        }
                                    }
                                };
                                match prototype_result {
                                    CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(prototype) => {
                                        let prototype_ref = prototype.get_prototype_ref();
                                        let expected_params = &prototype_ref.params;
                                        let return_type = &prototype_ref.return_type;
                                        let result =
                                            self.check_params_type_and_count(expected_params, params);
                                        (result, return_type.clone())
                                    }
                                    CallExpressionPrototypeEquivalenceCheckResult::NeedsTypeInference(generic_type_decls) => {
                                        let inference_result = self.infer_concrete_types_from_arguments(
                                            generic_type_decls,
                                            &func_data.prototype,
                                            params,
                                            GenericTypeDeclarationPlaceCategory::InCallable,
                                        );
                                        match inference_result {
                                            Ok((concrete_types, _)) => {
                                                let return_type = &func_data.prototype.return_type;
                                                (Ok(()), if return_type.has_generics() { return_type.concretize(&ConcretizationContext::new(&vec![], &concrete_types)) } else { return_type.clone() })
                                            }
                                            Err(err) => {
                                                (Err(err), Type::new_with_unknown())
                                            }
                                        }
                                    }
                                }
                            }
                            ConcreteSymbolDataEntry::Variable(variable_symbol_data) => {
                                assert!(variable_symbol_data.index.is_none());
                                let lambda_type = &variable_symbol_data.get_core_ref().data_type;
                                match lambda_type.0.as_ref() {
                                    CoreType::Lambda(lambda_data) => match &*lambda_data {
                                        Lambda::Named((_, semantic_data)) => {
                                            let symbol_data = semantic_data.get_core_ref();
                                            let data = symbol_data.get_lambda_data_ref();
                                            let expected_params = &data.meta_data.prototype.params;
                                            let return_type =
                                                data.meta_data.prototype.return_type.clone();
                                            let result = self.check_params_type_and_count(
                                                expected_params,
                                                params,
                                            );
                                            (result, return_type)
                                        }
                                        Lambda::Unnamed(unnamed_lambda) => {
                                            let expected_params = &unnamed_lambda.params;
                                            let return_type = unnamed_lambda.return_type.clone();
                                            let result = self.check_params_type_and_count(
                                                expected_params,
                                                params,
                                            );
                                            (result, return_type)
                                        }
                                    },
                                    _ => {
                                        let err = IdentifierNotCallableError::new(
                                            lambda_type.to_string(),
                                            ok_identifier.range(),
                                        );
                                        self.log_error(Diagnostics::IdentifierNotCallable(err));
                                        return Type::new_with_unknown();
                                    }
                                }
                            }
                            ConcreteSymbolDataEntry::Interface(_) => unreachable!(),
                            ConcreteSymbolDataEntry::Type(user_defined_type_symbol_data) => {
                                let name = ok_identifier.token_value(&self.code);
                                let struct_constructor_prototype_check_result =
                                    match &*user_defined_type_symbol_data.get_core_ref() {
                                        UserDefinedTypeData::Struct(struct_symbol_data) => {
                                            let index = user_defined_type_symbol_data.index;
                                            let constructor_meta_data =
                                                &struct_symbol_data.constructor;
                                            let prototype_result = match index {
                                                Some(index) => {
                                                    // CASE 1
                                                    let concrete_types = struct_symbol_data
                                                        .get_concrete_types(index);
                                                    let concrete_prototype = constructor_meta_data
                                                        .prototype
                                                        .concretize_prototype(
                                                            &concrete_types.0,
                                                            &vec![],
                                                        );
                                                    CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(concrete_prototype)
                                                }
                                                None => {
                                                    match &struct_symbol_data.generics.generics_spec
                                                    {
                                                        Some(generic_type_decls) => {
                                                            // CASE 2
                                                            CallExpressionPrototypeEquivalenceCheckResult::NeedsTypeInference(generic_type_decls)
                                                        }
                                                        None => {
                                                            // CASE 4
                                                            CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(PrototypeConcretizationResult::UnConcretized(
                                                            &constructor_meta_data.prototype,
                                                        ))
                                                        }
                                                    }
                                                }
                                            };
                                            match prototype_result {
                                            CallExpressionPrototypeEquivalenceCheckResult::HasConcretePrototype(prototype) => {
                                                let prototype_ref = prototype.get_prototype_ref();
                                                let result = self.check_params_type_and_count(
                                                    &prototype_ref.params,
                                                    params,
                                                );
                                                StructConstructorPrototypeCheckResult::Basic((
                                                    result,
                                                    Type::new_with_struct(
                                                        name.to_string(),
                                                        &user_defined_type_symbol_data.symbol_data,
                                                        index,
                                                    ),
                                                ))
                                            }
                                            CallExpressionPrototypeEquivalenceCheckResult::NeedsTypeInference(generic_type_decls) => {
                                                let inference_result = self.infer_concrete_types_from_arguments(
                                                    generic_type_decls,
                                                    &constructor_meta_data.prototype,
                                                    params,
                                                    GenericTypeDeclarationPlaceCategory::InStruct,
                                                );
                                                match inference_result {
                                                    Ok((concrete_types, has_generics)) => {
                                                        StructConstructorPrototypeCheckResult::Inferred((concrete_types, has_generics))
                                                    }
                                                    Err(err) => {
                                                        StructConstructorPrototypeCheckResult::Basic((Err(err), Type::new_with_unknown()))
                                                    }
                                                }
                                            }
                                        }
                                        }
                                        UserDefinedTypeData::Lambda(_)
                                        | UserDefinedTypeData::Generic(_) => {
                                            let err = ConstructorNotFoundForTypeError::new(
                                                name,
                                                ok_identifier.range(),
                                            );
                                            self.log_error(
                                                Diagnostics::ConstructorNotFoundForType(err),
                                            );
                                            return Type::new_with_unknown();
                                        }
                                    };
                                match struct_constructor_prototype_check_result {
                                    StructConstructorPrototypeCheckResult::Basic((
                                        result,
                                        return_type,
                                    )) => (result, return_type),
                                    StructConstructorPrototypeCheckResult::Inferred((
                                        concrete_types,
                                        has_generics,
                                    )) => {
                                        let index = user_defined_type_symbol_data
                                            .get_core_mut_ref()
                                            .register_concrete_types(concrete_types, has_generics);
                                        (
                                            Ok(()),
                                            Type::new_with_struct(
                                                name,
                                                &user_defined_type_symbol_data.symbol_data,
                                                Some(index),
                                            ),
                                        )
                                    }
                                }
                            }
                        };
                        match result {
                            Ok(()) => return return_type,
                            Err(err) => {
                                self.log_params_type_and_count_check_error(func_name.range(), err);
                            }
                        }
                    }
                }
                Type::new_with_unknown()
            }
            CoreAtomStartNode::ClassMethodCall(class_method) => {
                let core_class_method = class_method.core_ref();
                let class = &core_class_method.class_name;
                let class_method = &core_class_method.class_method_name;
                let params = &core_class_method.params;
                if let CoreIdentifierInUseNode::Ok(ok_identifier) = class.core_ref() {
                    let class_name = ok_identifier.token_value(&self.code);
                    match self
                        .namespace_handler
                        .get_type_symbol_data_for_identifier_in_use(ok_identifier)
                    {
                        Some(type_symbol_data) => match &*type_symbol_data.get_core_ref() {
                            UserDefinedTypeData::Struct(struct_data) => {
                                let class_method_name = match class_method.core_ref() {
                                    CoreIdentifierInUseNode::Ok(class_method) => {
                                        // TODO - check if <...> is correct
                                        // if <> is present use them to form concrete arguments if not and is expected by the
                                        // identifier symbol_data then infer the types from params and then repeat the above step.
                                        class_method.token_value(&self.code)
                                    }
                                    _ => return Type::new_with_unknown(),
                                };
                                match struct_data.class_methods.get(&class_method_name) {
                                    // use above two types of concrete types to form `ConcretizationContext`
                                    // to do the `params_type_and_count` check and get the return type
                                    Some((func_data, _)) => {
                                        let expected_params = &func_data.prototype.params;
                                        let return_type = &func_data.prototype.return_type;
                                        let result = self
                                            .check_params_type_and_count(expected_params, params);
                                        match result {
                                            Ok(()) => return return_type.clone(),
                                            Err(err) => {
                                                self.log_params_type_and_count_check_error(
                                                    class_method.range(),
                                                    err,
                                                );
                                                return Type::new_with_unknown();
                                            }
                                        }
                                    }
                                    None => {
                                        let err = ClassmethodDoesNotExistError::new(
                                            class_name,
                                            class_method.range(),
                                        );
                                        self.log_error(Diagnostics::ClassmethodDoesNotExist(err));
                                        return Type::new_with_unknown();
                                    }
                                }
                            }
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
        }
    }

    pub fn check_struct_property(
        &self,
        atom_type_obj: &Type,
        property_name: &OkIdentifierInUseNode,
    ) -> StructPropertyCheckResult {
        let property_name_str = property_name.token_value(&self.code);
        match atom_type_obj.0.as_ref() {
            CoreType::Struct(struct_ty) => {
                let symbol_data = struct_ty.semantic_data.symbol_data.get_core_ref();
                let struct_data = symbol_data.get_struct_data_ref();
                match struct_data.try_field(&property_name_str) {
                    Some((type_obj, _)) => {
                        return StructPropertyCheckResult::PropertyExist(type_obj)
                    }
                    None => return StructPropertyCheckResult::PropertyDoesNotExist,
                }
            }
            _ => return StructPropertyCheckResult::NonStructType,
        }
    }

    pub fn check_atom(&self, atom: &AtomNode) -> (Type, Option<Type>) {
        let core_atom = atom.core_ref();
        match core_atom {
            CoreAtomNode::AtomStart(atom_start) => (self.check_atom_start(atom_start), None),
            CoreAtomNode::Call(call) => {
                let core_call = call.core_ref();
                let atom = &core_call.atom;
                let params = &core_call.params;
                let (atom_type_obj, _) = self.check_atom(atom);
                match &atom_type_obj.0.as_ref() {
                    CoreType::Lambda(lambda_data) => {
                        // check if the type is lambda
                        match &*lambda_data {
                            Lambda::Named((_, semantic_data)) => {
                                let symbol_data = semantic_data.get_core_ref();
                                let data = symbol_data.get_lambda_data_ref();
                                let expected_param_types = &data.meta_data.prototype.params;
                                let return_type = &data.meta_data.prototype.return_type;
                                let result =
                                    self.check_params_type_and_count(expected_param_types, params);
                                match result {
                                    Ok(()) => {
                                        return (return_type.clone(), Some(atom_type_obj.clone()))
                                    }
                                    Err(err) => {
                                        self.log_params_type_and_count_check_error(
                                            atom.range(),
                                            err,
                                        );
                                        return (
                                            Type::new_with_unknown(),
                                            Some(atom_type_obj.clone()),
                                        );
                                    }
                                }
                            }
                            Lambda::Unnamed(unnamed_lambda) => {
                                let expected_param_types = &unnamed_lambda.params;
                                let return_type = &unnamed_lambda.return_type;
                                let result =
                                    self.check_params_type_and_count(expected_param_types, params);
                                match result {
                                    Ok(()) => {
                                        return (return_type.clone(), Some(atom_type_obj.clone()))
                                    }
                                    Err(err) => {
                                        self.log_params_type_and_count_check_error(
                                            atom.range(),
                                            err,
                                        );
                                        return (
                                            Type::new_with_unknown(),
                                            Some(atom_type_obj.clone()),
                                        );
                                    }
                                }
                            }
                        }
                    }
                    _ => {
                        let err = ExpressionNotCallableError::new(atom.range());
                        self.log_error(Diagnostics::ExpressionNotCallable(err));
                        return (Type::new_with_unknown(), Some(atom_type_obj));
                    }
                }
            }
            CoreAtomNode::PropertyAccess(property_access) => {
                let core_property_access = property_access.core_ref();
                let atom = &core_property_access.atom;
                let (atom_type_obj, _) = self.check_atom(atom);
                let property = &core_property_access.propertry;
                if let CoreIdentifierInUseNode::Ok(ok_identifier) = property.core_ref() {
                    // TODO - check that <...> should not exist as it's a variable!
                    let result = self.check_struct_property(&atom_type_obj, ok_identifier);
                    match result {
                        StructPropertyCheckResult::PropertyExist(type_obj) => {
                            return (type_obj, Some(atom_type_obj))
                        }
                        StructPropertyCheckResult::PropertyDoesNotExist => {
                            let err = PropertyDoesNotExistError::new(
                                PropertyKind::Field,
                                atom_type_obj.to_string(),
                                ok_identifier.range(),
                                atom.range(),
                            );
                            self.log_error(Diagnostics::PropertyDoesNotExist(err));
                            return (Type::new_with_unknown(), Some(atom_type_obj));
                        }
                        StructPropertyCheckResult::NonStructType => {
                            let err = PropertyDoesNotExistError::new(
                                PropertyKind::Field,
                                atom_type_obj.to_string(),
                                property.range(),
                                atom.range(),
                            );
                            self.log_error(Diagnostics::PropertyDoesNotExist(err));
                            return (Type::new_with_unknown(), Some(atom_type_obj));
                        }
                    }
                }
                (Type::new_with_unknown(), Some(atom_type_obj))
            }
            CoreAtomNode::MethodAccess(method_access) => {
                let core_method_access = method_access.core_ref();
                let atom = &core_method_access.atom;
                let (atom_type_obj, _) = self.check_atom(atom);
                let method = &core_method_access.method_name;
                let params = &core_method_access.params;
                if let CoreIdentifierInUseNode::Ok(ok_identifier) = method.core_ref() {
                    // for syntax `<struct_obj>.<property_name>([<params>])` first type-checker tries to find `property_name` in fields
                    // (for example: a field with lambda type) and then it goes on to find it in methods.
                    // This is in sync with what Python does.

                    // TODO - check if <...> is correct
                    let result = self.check_struct_property(&atom_type_obj, ok_identifier);
                    let method_name = ok_identifier.token_value(&self.code);
                    match result {
                        StructPropertyCheckResult::PropertyExist(type_obj) => {
                            match &type_obj.0.as_ref() {
                                CoreType::Lambda(lambda_data) => match &*lambda_data {
                                    Lambda::Named((_, semantic_data)) => {
                                        let symbol_data = semantic_data.get_core_ref();
                                        let data = symbol_data.get_lambda_data_ref();
                                        let expected_param_types = &data.meta_data.prototype.params;
                                        let return_type = &data.meta_data.prototype.return_type;
                                        let result = self.check_params_type_and_count(
                                            expected_param_types,
                                            params,
                                        );
                                        match result {
                                            Ok(()) => {
                                                return (return_type.clone(), Some(atom_type_obj))
                                            }
                                            Err(err) => {
                                                self.log_params_type_and_count_check_error(
                                                    ok_identifier.range(),
                                                    err,
                                                );
                                                return (
                                                    Type::new_with_unknown(),
                                                    Some(atom_type_obj),
                                                );
                                            }
                                        }
                                    }
                                    Lambda::Unnamed(unnamed_lambda) => {
                                        let expected_param_types = &unnamed_lambda.params;
                                        let return_type = &unnamed_lambda.return_type;
                                        let result = self.check_params_type_and_count(
                                            expected_param_types,
                                            params,
                                        );
                                        match result {
                                            Ok(()) => {
                                                return (return_type.clone(), Some(atom_type_obj))
                                            }
                                            Err(err) => {
                                                self.log_params_type_and_count_check_error(
                                                    ok_identifier.range(),
                                                    err,
                                                );
                                                return (
                                                    Type::new_with_unknown(),
                                                    Some(atom_type_obj),
                                                );
                                            }
                                        }
                                    }
                                },
                                _ => {
                                    let err = StructFieldNotCallableError::new(
                                        type_obj.to_string(),
                                        ok_identifier.range(),
                                    );
                                    self.log_error(Diagnostics::StructFieldNotCallable(err));
                                    return (Type::new_with_unknown(), Some(atom_type_obj));
                                }
                            }
                        }
                        StructPropertyCheckResult::PropertyDoesNotExist => {
                            match atom_type_obj.0.as_ref() {
                                CoreType::Struct(struct_type) => {
                                    let symbol_data = struct_type.semantic_data.get_core_ref();
                                    let struct_data = symbol_data.get_struct_data_ref();
                                    match struct_data.try_method(&method_name) {
                                        Some((func_data, _)) => {
                                            let expected_params = &func_data.prototype.params;
                                            let return_type = &func_data.prototype.return_type;
                                            let result = self.check_params_type_and_count(
                                                expected_params,
                                                params,
                                            );
                                            match result {
                                                Ok(()) => {
                                                    return (
                                                        return_type.clone(),
                                                        Some(atom_type_obj.clone()),
                                                    )
                                                }
                                                Err(err) => {
                                                    self.log_params_type_and_count_check_error(
                                                        method.range(),
                                                        err,
                                                    );
                                                    return (
                                                        Type::new_with_unknown(),
                                                        Some(atom_type_obj.clone()),
                                                    );
                                                }
                                            }
                                        }
                                        None => {
                                            let err = PropertyDoesNotExistError::new(
                                                PropertyKind::Method,
                                                atom_type_obj.to_string(),
                                                method.range(),
                                                atom.range(),
                                            );
                                            self.log_error(Diagnostics::PropertyDoesNotExist(err));
                                            return (
                                                Type::new_with_unknown(),
                                                Some(atom_type_obj.clone()),
                                            );
                                        }
                                    }
                                }
                                _ => unreachable!(),
                            }
                        }
                        StructPropertyCheckResult::NonStructType => {
                            let result: Result<CallablePrototypeData, ()> = match &atom_type_obj
                                .0
                                .as_ref()
                            {
                                CoreType::Array(array) => match array.try_method(&method_name) {
                                    Some(prototype) => Ok(prototype),
                                    None => Err(()),
                                },
                                CoreType::HashMap(hashmap) => {
                                    match hashmap.try_method(&method_name) {
                                        Some(prototype) => Ok(prototype),
                                        None => Err(()),
                                    }
                                }
                                _ => Err(()),
                            };
                            match result {
                                Ok(prototype) => {
                                    let expected_params = &prototype.params;
                                    let return_type = &prototype.return_type;
                                    let result =
                                        self.check_params_type_and_count(expected_params, params);
                                    match result {
                                        Ok(()) => {
                                            return (
                                                return_type.clone(),
                                                Some(atom_type_obj.clone()),
                                            )
                                        }
                                        Err(err) => {
                                            self.log_params_type_and_count_check_error(
                                                method.range(),
                                                err,
                                            );
                                            return (
                                                Type::new_with_unknown(),
                                                Some(atom_type_obj.clone()),
                                            );
                                        }
                                    }
                                }
                                Err(_) => {
                                    let err = PropertyDoesNotExistError::new(
                                        PropertyKind::Method,
                                        atom_type_obj.to_string(),
                                        method.range(),
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
            CoreAtomNode::IndexAccess(index_access) => {
                let core_index_access = index_access.core_ref();
                let atom = &core_index_access.atom;
                let (atom_type_obj, _) = self.check_atom(atom);
                let index_expr = &core_index_access.index;
                let index_type_obj = self.check_expr(index_expr);
                match atom_type_obj.0.as_ref() {
                    CoreType::Tuple(tuple) => {
                        let sub_types = &tuple.sub_types;
                        match index_expr.core_ref() {
                            CoreExpressionNode::Unary(index_unary_expr) => {
                                match self.is_unary_expr_int_valued(index_unary_expr) {
                                    Some(index_value) => {
                                        match self
                                            .is_valid_index_for_tuple(index_value, sub_types.len())
                                        {
                                            TupleIndexCheckResult::Ok(index_value) => {
                                                return (
                                                    sub_types[index_value].clone(),
                                                    Some(atom_type_obj),
                                                )
                                            }
                                            TupleIndexCheckResult::PositiveIndexOutOfBound => {
                                                let err = TupleIndexOutOfBoundError::new(
                                                    sub_types.len(),
                                                    index_expr.range(),
                                                );
                                                self.log_error(Diagnostics::TupleIndexOutOfBound(
                                                    err,
                                                ));
                                                return (
                                                    Type::new_with_unknown(),
                                                    Some(atom_type_obj),
                                                );
                                            }
                                            TupleIndexCheckResult::NegativeIndexOutOfBound => {
                                                let err = TupleIndexOutOfBoundError::new(
                                                    sub_types.len(),
                                                    index_expr.range(),
                                                );
                                                self.log_error(Diagnostics::TupleIndexOutOfBound(
                                                    err,
                                                ));
                                                return (
                                                    Type::new_with_unknown(),
                                                    Some(atom_type_obj),
                                                );
                                            }
                                        }
                                    }
                                    None => {
                                        let err = UnresolvedIndexExpressionInTupleError::new(
                                            index_expr.range(),
                                        );
                                        self.log_error(
                                            Diagnostics::UnresolvedIndexExpressionInTuple(err),
                                        );
                                        return (Type::new_with_unknown(), Some(atom_type_obj));
                                    }
                                }
                            }
                            CoreExpressionNode::Binary(_) | CoreExpressionNode::Comparison(_) => {
                                let err =
                                    InvalidIndexExpressionForTupleError::new(index_expr.range());
                                self.log_error(Diagnostics::InvalidIndexExpressionForTuple(err));
                                return (Type::new_with_unknown(), Some(atom_type_obj));
                            }
                        }
                    }
                    _ => {
                        // check for types other than tuple
                        match self.is_indexable_with_type(&atom_type_obj, &index_type_obj) {
                            Some(element_type) => {
                                return (element_type.clone(), Some(atom_type_obj))
                            }
                            _ => {
                                let err = ExpressionIndexingNotValidError::new(
                                    atom_type_obj.to_string(),
                                    index_type_obj.to_string(),
                                    atom.range(),
                                    index_expr.range(),
                                );
                                self.log_error(Diagnostics::ExpressionIndexingNotValid(err));
                                return (Type::new_with_unknown(), Some(atom_type_obj));
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn check_r_assign(&self, r_assign: &RAssignmentNode) -> Type {
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
                return self.type_of_lambda(lambda);
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

    pub fn check_atomic_expr(&self, atomic_expr: &AtomicExpressionNode) -> Type {
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
            CoreAtomicExpressionNode::Atom(atom) => self.check_atom(atom).0,
            CoreAtomicExpressionNode::MissingTokens(_) => Type::new_with_unknown(),
        }
    }

    pub fn check_only_unary_expr(&self, only_unary_expr: &OnlyUnaryExpressionNode) -> Type {
        let core_only_unary_expr = only_unary_expr.core_ref();
        let unary_expr = &core_only_unary_expr.unary_expr;
        let operand_type = self.check_unary_expr(&unary_expr);
        let operator = &core_only_unary_expr.operator;
        let operator_kind = &core_only_unary_expr.operator_kind;
        match operator_kind {
            UnaryOperatorKind::Plus | UnaryOperatorKind::Minus => {
                if operand_type.is_numeric() {
                    return operand_type;
                } else {
                    let err = UnaryOperatorInvalidUseError::new(
                        operand_type.to_string(),
                        "numeric (`int`, `float`)",
                        "`+` or `-`",
                        unary_expr.range(),
                        operator.range(),
                    );
                    self.log_error(Diagnostics::UnaryOperatorInvalidUse(err));
                    return Type::new_with_unknown();
                }
            }
            UnaryOperatorKind::Not => {
                if operand_type.is_bool() {
                    return operand_type;
                } else {
                    let err = UnaryOperatorInvalidUseError::new(
                        operand_type.to_string(),
                        "boolean",
                        "`not`",
                        unary_expr.range(),
                        operator.range(),
                    );
                    self.log_error(Diagnostics::UnaryOperatorInvalidUse(err));
                    return Type::new_with_unknown();
                }
            }
        }
    }

    pub fn check_unary_expr(&self, unary_expr: &UnaryExpressionNode) -> Type {
        let core_unary_expr = unary_expr.core_ref();
        match core_unary_expr {
            CoreUnaryExpressionNode::Atomic(atomic) => self.check_atomic_expr(atomic),
            CoreUnaryExpressionNode::Unary(unary) => self.check_only_unary_expr(unary),
        }
    }

    pub fn check_binary_expr(&self, binary_expr: &BinaryExpressionNode) -> Type {
        let core_binary_expr = binary_expr.core_ref();
        let left_expr = &core_binary_expr.left_expr;
        let right_expr = &core_binary_expr.right_expr;
        let l_type = self.check_expr(left_expr);
        let operator = &core_binary_expr.operator;
        let operator_kind = &core_binary_expr.operator_kind;
        let r_type = self.check_expr(right_expr);
        let result = self.is_binary_operation_valid(&l_type, &r_type, operator_kind);
        match result {
            Some(type_obj) => return type_obj,
            None => {
                let err = BinaryOperatorInvalidOperandsError::new(
                    l_type.to_string(),
                    r_type.to_string(),
                    left_expr.range(),
                    right_expr.range(),
                    operator.range(),
                );
                self.log_error(Diagnostics::BinaryOperatorInvalidOperands(err));
                return Type::new_with_unknown();
            }
        }
    }

    pub fn check_comp_expr(&self, comp_expr: &ComparisonNode) -> Type {
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
            assert!(
                operator_kind.is_comparison(),
                "all the operators in `ComparisonNode` should be comparison operators"
            );
            let result = self.is_binary_operation_valid(&l_type, &r_type, &operator_kind);
            match result {
                Some(type_obj) => match type_obj.0.as_ref() {
                    CoreType::Atomic(atomic) => assert!(atomic.is_bool()),
                    CoreType::Unknown => return Type::new_with_unknown(),
                    _ => unreachable!("comparison operator always result into `bool` type"),
                },
                None => {
                    let err = BinaryOperatorInvalidOperandsError::new(
                        l_type.to_string(),
                        r_type.to_string(),
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

    pub fn check_expr(&self, expr: &ExpressionNode) -> Type {
        let core_expr = expr.core_ref();
        match core_expr {
            CoreExpressionNode::Unary(unary_expr) => self.check_unary_expr(unary_expr),
            CoreExpressionNode::Binary(binary_expr) => self.check_binary_expr(binary_expr),
            CoreExpressionNode::Comparison(comparison_expr) => {
                self.check_comp_expr(comparison_expr)
            }
        }
    }

    pub fn check_assignment(&self, assignment: &AssignmentNode) {
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
                                &interior_atom_type,
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
                l_type.to_string(),
                r_type.to_string(),
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
                .namespace_handler
                .get_variable_symbol_data_for_identifier_in_decl(ok_identifier)
            {
                symbol_data.get_core_mut_ref().set_data_type(&r_type);
            }
        };
    }

    pub fn check_callable_prototype(&self, callable_prototype: &CallablePrototypeNode) -> Type {
        let core_callable_prototype = callable_prototype.0.as_ref();
        let return_type_node = &core_callable_prototype.return_type;
        let return_type_obj = match return_type_node {
            Some((_, return_type_expr)) => self.type_obj_from_expression(return_type_expr),
            None => Type::new_with_void(),
        };
        return_type_obj
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
        for stmt in &*body.0.as_ref().stmts.as_ref() {
            let stmt = match stmt.core_ref() {
                CoreStatemenIndentWrapperNode::CorrectlyIndented(stmt) => stmt,
                CoreStatemenIndentWrapperNode::IncorrectlyIndented(stmt) => {
                    let core_stmt = stmt.core_ref();
                    &core_stmt.stmt
                }
                _ => continue,
            };
            self.walk_stmt(&stmt);
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
        } else {
            if !has_return_stmt.is_some() && !return_type_obj.is_void() {
                let (_, return_type_node) = prototype.core_ref().return_type.as_ref().unwrap();
                let err = NoReturnStatementInFunctionError::new(return_type_node.range());
                self.log_error(Diagnostics::NoReturnStatementInFunction(err));
            }
        }
        self.context.func_stack.pop();
    }

    pub fn check_bounded_method(&mut self, bounded_method_wrapper: &BoundedMethodWrapperNode) {
        let core_bounded_method_wrapper = &*bounded_method_wrapper.0.as_ref();
        let is_constructor = match self
            .namespace_handler
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

    pub fn check_return_stmt(&self, return_stmt: &ReturnStatementNode) {
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
        } else {
            if !expr_type_obj.is_eq(&expected_type_obj) {
                let err = MismatchedReturnTypeError::new(
                    expected_type_obj.to_string(),
                    expr_type_obj.to_string(),
                    core_return_stmt.return_keyword.range(),
                );
                self.log_error(Diagnostics::MismatchedReturnType(err));
            }
        }
    }

    pub fn check_struct_declaration(&mut self, struct_decl: &StructDeclarationNode) {
        let core_struct_decl = struct_decl.core_ref();
        // TODO - check the implementing_interfaces => first whether <...> is correct
        // then whether the methods expected by the interfaces are there or not
        self.walk_block(&core_struct_decl.block);
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
            CoreStatementNode::TypeDeclaration(type_decl) => match type_decl.core_ref() {
                CoreTypeDeclarationNode::Struct(struct_decl) => {
                    // self.walk_block(&struct_decl.core_ref().block);
                    self.check_struct_declaration(struct_decl);
                }
                CoreTypeDeclarationNode::Lambda(_) | CoreTypeDeclarationNode::MissingTokens(_) => {
                    return
                }
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
            CoreStatementNode::StructPropertyDeclaration(_) => return,
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
            PrototypeEquivalenceCheckError::NotAllConcreteTypesInferred => todo!(), // TODO - raise error
            PrototypeEquivalenceCheckError::TypeInferenceFailed => todo!(),
            PrototypeEquivalenceCheckError::ConcreteTypesCannotBeInferred => todo!(),
            PrototypeEquivalenceCheckError::InferredTypesNotBoundedByInterfaces(err_strs, concrete_types) => {
                todo!()
            }
        }
    }
}

impl Visitor for TypeChecker {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match node {
            ASTNode::Statement(stmt) => {
                self.check_stmt(stmt);
                return None;
            }
            _ => Some(()),
        }
    }
}
