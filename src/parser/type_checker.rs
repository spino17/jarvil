// See `https://www.csd.uwo.ca/~mmorenom/CS447/Lectures/TypeChecking.html/node1.html` for information about various cases that type-checker needs to
// cover and the representation of type expressions in terms of type objects.

use crate::{
    ast::{
        ast::{
            ASTNode, AssignmentNode, AtomNode, AtomStartNode, AtomicExpressionNode,
            BinaryExpressionNode, BlockNode, CallableBodyNode, CallablePrototypeNode,
            ComparisonNode, CoreAssignmentNode, CoreAtomNode, CoreAtomStartNode,
            CoreAtomicExpressionNode, CoreCallableBodyNode, CoreExpressionNode, CoreIdentifierNode,
            CoreRAssignmentNode, CoreRVariableDeclarationNode, CoreSelfKeywordNode,
            CoreStatemenIndentWrapperNode, CoreStatementNode, CoreTokenNode,
            CoreTypeDeclarationNode, CoreUnaryExpressionNode, ExpressionNode,
            LambdaDeclarationNode, NameTypeSpecsNode, Node, OkIdentifierNode,
            OnlyUnaryExpressionNode, ParamsNode, RAssignmentNode, RVariableDeclarationNode,
            ReturnStatementNode, StatementNode, TokenNode, TypeExpressionNode, TypeResolveKind,
            UnaryExpressionNode, VariableDeclarationNode,
        },
        walk::Visitor,
    },
    code::JarvilCode,
    constants::common::{BOOL, FLOAT, INT, STRING},
    error::{
        constants::{
            LAMBDA_NAME_NOT_BINDED_WITH_LAMBDA_VARIANT_SYMBOL_DATA_MSG,
            STRUCT_NAME_NOT_BINDED_WITH_STRUCT_VARIANT_SYMBOL_DATA_MSG,
        },
        diagnostics::{
            BinaryOperatorInvalidOperandsError, ClassmethodDoesNotExistError,
            ConstructorNotFoundForTypeError, Diagnostics, ExpressionIndexingNotValidError,
            ExpressionNotCallableError, IdentifierNotCallableError,
            ImmutableTypeNotAssignableError, InvalidIndexExpressionForTupleError,
            InvalidReturnStatementError, LessParamsCountError, MismatchedParamTypeError,
            MismatchedReturnTypeError, MismatchedTypesOnLeftRightError, MoreParamsCountError,
            NoReturnStatementInFunctionError, NoValidStatementInsideFunctionBody,
            PropertyDoesNotExistError, PropertyNotSupportedError,
            RightSideWithVoidTypeNotAllowedError, StructFieldNotCallableError,
            TupleIndexOutOfBoundError, UnaryOperatorInvalidUseError,
            UnresolvedIndexExpressionInTupleError,
        },
        helper::PropertyKind,
    },
    lexer::token::{BinaryOperatorKind, UnaryOperatorKind},
    parser::resolver::ErrorLoggingTypeKind,
    scope::{
        core::{IdentifierKind, SymbolData},
        user_defined_types::{LambdaTypeData, StructData, UserDefinedTypeData},
    },
    types::{
        atomic::Atomic,
        core::{AbstractType, CoreType, Type},
    },
};
use std::rc::Rc;
use text_size::TextRange;

use super::resolver::Resolver;

#[derive(Debug)]
struct Context {
    func_stack: Vec<Type>,
}

pub enum AtomicTokenExprKind {
    BOOL,
    INTEGER,
    FLOAT,
    LITERAL,
}

pub enum StructPropertyCheckResult {
    PROPERTY_EXIST((StructData, Type)),
    PROPERTY_DOES_NOT_EXIST(StructData),
    NON_STRUCT_TYPE,
}

pub enum ParamsTypeNCountResult {
    OK,
    MORE_PARAMS(usize),
    LESS_PARAMS((usize, usize)), // (expected_params_num, received_params_num)
    MISMATCHED_TYPE(Vec<(String, String, usize, TextRange)>), // (expected_type, received_type, index_of_param, span)
}

#[derive(Debug, Clone)]
pub enum TupleIndexCheckResult {
    OK(usize),
    POSITIVE_INDEX_OUT_OF_BOUND,
    NEGATIVE_INDEX_OUT_OF_BOUND,
}

pub struct TypeChecker {
    code: JarvilCode,
    errors: Vec<Diagnostics>,
    context: Context,
}

pub enum CallableParamsData {
    LAMBDA(Rc<Vec<Type>>),
    OTHER(Rc<Vec<(Rc<String>, Type)>>),
}

impl CallableParamsData {
    fn len(&self) -> usize {
        match self {
            CallableParamsData::LAMBDA(lambda_params_data) => lambda_params_data.len(),
            CallableParamsData::OTHER(other_params_data) => other_params_data.len(),
        }
    }
}

impl TypeChecker {
    pub fn new(code: &JarvilCode) -> Self {
        TypeChecker {
            code: code.clone(),
            errors: vec![],
            context: Context { func_stack: vec![] },
        }
    }

    pub fn check_ast(mut self, ast: &BlockNode) -> Vec<Diagnostics> {
        let core_block = ast.0.as_ref().borrow();
        for stmt in &core_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        self.errors
    }

    pub fn type_obj_from_expression(&self, type_expr: &TypeExpressionNode) -> Type {
        match type_expr.type_obj_after_resolved(&self.code) {
            TypeResolveKind::RESOLVED(type_obj) => {
                type DummyFnType = fn(&mut Resolver, TextRange, ErrorLoggingTypeKind);
                return Resolver::pre_type_checking::<DummyFnType>(&type_obj, type_expr, None);
            }
            TypeResolveKind::UNRESOLVED(_) => return Type::new_with_unknown(),
            TypeResolveKind::INVALID => Type::new_with_unknown(),
        }
    }

    pub fn params_and_return_type_obj_from_expr(
        &self,
        return_type: &Option<TypeExpressionNode>,
        params: &Option<NameTypeSpecsNode>,
    ) -> (Vec<Type>, Type) {
        let mut params_vec: Vec<Type> = vec![];
        let return_type: Type = match return_type {
            Some(return_type_expr) => {
                let type_obj = self.type_obj_from_expression(return_type_expr);
                type_obj
            }
            None => Type::new_with_void(),
        };
        if let Some(params) = params {
            let params_iter = params.iter();
            for param in params_iter {
                let core_param = param.core_ref();
                let name = &core_param.name;
                if let CoreIdentifierNode::OK(ok_identifier) = name.core_ref() {
                    if ok_identifier.is_resolved() {
                        let type_obj = self.type_obj_from_expression(&core_param.data_type);
                        params_vec.push(type_obj);
                    }
                }
            }
        }
        (params_vec, return_type)
    }

    pub fn type_of_lambda(&self, lambda_decl: &LambdaDeclarationNode) -> Type {
        let core_lambda_decl = lambda_decl.0.as_ref();
        let lambda_name = &core_lambda_decl.name;
        match &core_lambda_decl.body.core_ref() {
            CoreCallableBodyNode::OK(ok_callable_decl) => {
                let core_ok_callable_decl = ok_callable_decl.core_ref();
                let prototype = &core_ok_callable_decl.prototype.core_ref();
                let params = &prototype.params;
                let return_type = &prototype.return_type;
                let (params_vec, return_type) = match lambda_name.core_ref() {
                    CoreIdentifierNode::OK(ok_identifier) => match ok_identifier
                        .variable_symbol_data(
                            "lambda name should be resolved to `SymbolData<VariableData>`",
                        ) {
                        Some(symbol_data) => {
                            return symbol_data.0.as_ref().borrow().data_type.clone()
                        }
                        None => self.params_and_return_type_obj_from_expr(return_type, params),
                    },
                    _ => self.params_and_return_type_obj_from_expr(return_type, params),
                };
                let symbol_data =
                    UserDefinedTypeData::LAMBDA(LambdaTypeData::new(params_vec, return_type));
                let lambda_type_obj = Type::new_with_lambda(
                    None,
                    &SymbolData::new(symbol_data, prototype.lparen.range(), true),
                );
                return lambda_type_obj;
            }
            CoreCallableBodyNode::MISSING_TOKENS(_) => return Type::new_with_unknown(),
        }
    }

    pub fn is_callable(&mut self, type_obj: &Type) -> Option<(Rc<Vec<Type>>, Type)> {
        match type_obj.0.as_ref() {
            CoreType::LAMBDA(lambda) => {
                let lambda_data = lambda
                    .symbol_data
                    .0
                    .as_ref()
                    .borrow()
                    .lambda_data(LAMBDA_NAME_NOT_BINDED_WITH_LAMBDA_VARIANT_SYMBOL_DATA_MSG)
                    .clone();
                return Some((lambda_data.param_types, lambda_data.return_type));
            }
            _ => None,
        }
    }

    pub fn is_unary_expr_int_valued(&self, unary: &UnaryExpressionNode) -> Option<i32> {
        match unary.core_ref() {
            CoreUnaryExpressionNode::UNARY(unary) => {
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
            CoreUnaryExpressionNode::ATOMIC(atomic) => match atomic.core_ref() {
                CoreAtomicExpressionNode::INTEGER(integer_valued_token) => {
                    match integer_valued_token.core_ref() {
                        CoreTokenNode::OK(ok_token) => {
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
            CoreUnaryExpressionNode::MISSING_TOKENS(_) => None,
        }
    }

    pub fn is_valid_index_for_tuple(
        &self,
        index_value: i32,
        tuple_len: usize,
    ) -> TupleIndexCheckResult {
        if index_value >= 0 {
            if index_value < tuple_len as i32 {
                return TupleIndexCheckResult::OK(index_value as usize);
            } else {
                return TupleIndexCheckResult::POSITIVE_INDEX_OUT_OF_BOUND;
            }
        } else {
            if -(tuple_len as i32) <= index_value {
                return TupleIndexCheckResult::OK((tuple_len as i32 + index_value) as usize);
            } else {
                return TupleIndexCheckResult::NEGATIVE_INDEX_OUT_OF_BOUND;
            }
        }
    }

    pub fn is_indexable_with_type(&mut self, base_type: &Type, index_type: &Type) -> Option<Type> {
        // NOTE - case for `tuple` is already handled in the calling function
        match base_type.0.as_ref() {
            CoreType::ARRAY(array) => {
                if index_type.is_int() {
                    return Some(array.element_type.clone());
                } else {
                    return None;
                }
            }
            CoreType::ATOMIC(atomic) => match atomic {
                Atomic::STRING => {
                    if index_type.is_int() {
                        return Some(Type::new_with_atomic("str"));
                    } else {
                        return None;
                    }
                }
                _ => return None,
            },
            CoreType::HASHMAP(hashmap) => {
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
        &mut self,
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

    pub fn check_params_type_and_count(
        &mut self,
        expected_param_data: CallableParamsData,
        received_params: &Option<ParamsNode>,
    ) -> ParamsTypeNCountResult {
        let expected_params_len = expected_param_data.len();
        match received_params {
            Some(received_params) => {
                let received_params_iter = received_params.iter();
                let mut index = 0;
                let mut mismatch_types_vec: Vec<(String, String, usize, TextRange)> = vec![]; // (expected_type, received_type, index_of_param)
                for received_param in received_params_iter {
                    let param_type_obj = self.check_expr(&received_param);
                    if index >= expected_params_len {
                        return ParamsTypeNCountResult::MORE_PARAMS(expected_params_len);
                    }
                    let expected_params_type_obj = match &expected_param_data {
                        CallableParamsData::LAMBDA(lambda_data) => {
                            lambda_data.as_ref()[index].clone()
                        }
                        CallableParamsData::OTHER(other_data) => {
                            other_data.as_ref()[index].1.clone()
                        }
                    };
                    if !param_type_obj.is_eq(&expected_params_type_obj) {
                        mismatch_types_vec.push((
                            expected_params_type_obj.to_string(),
                            param_type_obj.clone().to_string(),
                            index + 1,
                            received_param.range(),
                        ));
                    }
                    index = index + 1;
                }
                if index < expected_params_len {
                    return ParamsTypeNCountResult::LESS_PARAMS((expected_params_len, index));
                } else if mismatch_types_vec.len() > 0 {
                    return ParamsTypeNCountResult::MISMATCHED_TYPE(mismatch_types_vec);
                } else {
                    return ParamsTypeNCountResult::OK;
                }
            }
            None => {
                if expected_params_len != 0 {
                    return ParamsTypeNCountResult::LESS_PARAMS((expected_params_len, 0));
                } else {
                    return ParamsTypeNCountResult::OK;
                }
            }
        }
    }

    pub fn check_atom_start(&mut self, atom_start: &AtomStartNode) -> Type {
        let core_atom_start = atom_start.core_ref();
        match core_atom_start {
            CoreAtomStartNode::IDENTIFIER(token) => match token.core_ref() {
                CoreIdentifierNode::OK(ok_identifier) => {
                    match ok_identifier.variable_symbol_data(
                        "variable name should be resolved to `SymbolData<VariableData>`",
                    ) {
                        Some(variable_symbol_data) => {
                            return variable_symbol_data.0.as_ref().borrow().data_type.clone()
                        }
                        None => return Type::new_with_unknown(),
                    }
                }
                _ => Type::new_with_unknown(),
            },
            CoreAtomStartNode::SELF_KEYWORD(self_keyword) => {
                let core_self_keyword = self_keyword.core_ref();
                match core_self_keyword {
                    CoreSelfKeywordNode::OK(ok_self_keyword) => {
                        match ok_self_keyword.symbol_data() {
                            Some(symbol_data) => {
                                return symbol_data.0.as_ref().borrow().data_type.clone()
                            }
                            None => return Type::new_with_unknown(),
                        }
                    }
                    _ => Type::new_with_unknown(),
                }
            }
            CoreAtomStartNode::CALL(call_expr) => {
                let core_call_expr = call_expr.core_ref();
                let func_name = &core_call_expr.function_name;
                let params = &core_call_expr.params;
                if let CoreIdentifierNode::OK(ok_identifier) = func_name.core_ref() {
                    if let Some(symbol_data) = ok_identifier.symbol_data() {
                        let (expected_params_data, return_type) = match symbol_data.0 {
                            IdentifierKind::FUNCTION(func_symbol_data) => {
                                let func_data = func_symbol_data.0.as_ref().borrow().clone();
                                let expected_params = func_data.params;
                                let return_type = func_data.return_type;
                                (CallableParamsData::OTHER(expected_params), return_type)
                            }
                            IdentifierKind::VARIABLE(variable_symbol_data) => {
                                let lambda_type =
                                    variable_symbol_data.0.as_ref().borrow().data_type.clone();
                                match lambda_type.0.as_ref() {
                                    CoreType::LAMBDA(lambda_data) => {
                                        let lambda_data = lambda_data.symbol_data.0.as_ref().borrow().lambda_data(
                                            LAMBDA_NAME_NOT_BINDED_WITH_LAMBDA_VARIANT_SYMBOL_DATA_MSG
                                        ).clone();
                                        (
                                            CallableParamsData::LAMBDA(lambda_data.param_types),
                                            lambda_data.return_type,
                                        )
                                    }
                                    _ => {
                                        let err = IdentifierNotCallableError::new(
                                            lambda_type,
                                            ok_identifier.range(),
                                        );
                                        self.errors.push(Diagnostics::IdentifierNotCallable(err));
                                        return Type::new_with_unknown();
                                    }
                                }
                            }
                            IdentifierKind::USER_DEFINED_TYPE(user_defined_type_symbol_Data) => {
                                let type_decl_range = user_defined_type_symbol_Data.1;
                                let name = ok_identifier.token_value(&self.code);
                                match &*user_defined_type_symbol_Data.0.as_ref().borrow() {
                                    UserDefinedTypeData::STRUCT(struct_symbol_data) => {
                                        let constructor_meta_data =
                                            struct_symbol_data.constructor.clone();
                                        let return_type = Type::new_with_struct(
                                            name.to_string(),
                                            &SymbolData::new(
                                                UserDefinedTypeData::STRUCT(
                                                    struct_symbol_data.clone(),
                                                ),
                                                type_decl_range,
                                                true,
                                            ),
                                        );
                                        (
                                            CallableParamsData::OTHER(constructor_meta_data.params),
                                            return_type,
                                        )
                                    }
                                    UserDefinedTypeData::LAMBDA(_) => {
                                        let type_name = ok_identifier.token_value(&self.code);
                                        let err = ConstructorNotFoundForTypeError::new(
                                            type_name,
                                            ok_identifier.range(),
                                        );
                                        self.errors
                                            .push(Diagnostics::ConstructorNotFoundForType(err));
                                        return Type::new_with_unknown();
                                    }
                                }
                            }
                        };
                        let result = self.check_params_type_and_count(expected_params_data, params);
                        match result {
                            ParamsTypeNCountResult::OK => return return_type,
                            _ => {
                                self.log_params_type_and_count_check_error(
                                    func_name.range(),
                                    result,
                                );
                            }
                        }
                    }
                }
                Type::new_with_unknown()
            }
            CoreAtomStartNode::CLASS_METHOD_CALL(class_method) => {
                let core_class_method = class_method.core_ref();
                let class = &core_class_method.class_name;
                let class_method = &core_class_method.class_method_name;
                let params = &core_class_method.params;
                if let CoreIdentifierNode::OK(ok_identifier) = class.core_ref() {
                    let class_name = ok_identifier.token_value(&self.code);
                    match ok_identifier.user_defined_type_symbol_data(
                        "classname should be resolved to `SymbolData<UserDefinedTypeData>`",
                    ) {
                        Some(type_symbol_data) => match &*type_symbol_data.0.as_ref().borrow() {
                            UserDefinedTypeData::STRUCT(struct_data) => {
                                let class_method_name = match class_method.core_ref() {
                                    CoreIdentifierNode::OK(class_method) => {
                                        class_method.token_value(&self.code)
                                    }
                                    _ => return Type::new_with_unknown(),
                                };
                                match struct_data.class_methods.as_ref().get(&class_method_name) {
                                    Some((func_data, _)) => {
                                        let expected_params = func_data.params.clone();
                                        let return_type = func_data.return_type.clone();
                                        let result = self.check_params_type_and_count(
                                            CallableParamsData::OTHER(expected_params),
                                            params,
                                        );
                                        match result {
                                            ParamsTypeNCountResult::OK => return return_type,
                                            _ => {
                                                self.log_params_type_and_count_check_error(
                                                    class_method.range(),
                                                    result,
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
                                        self.errors.push(Diagnostics::ClassmethodDoesNotExist(err));
                                        return Type::new_with_unknown();
                                    }
                                }
                            }
                            _ => {
                                let err = PropertyNotSupportedError::new(
                                    "classmethod".to_string(),
                                    class.range(),
                                );
                                self.errors.push(Diagnostics::PropertyNotSupported(err));
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
        &mut self,
        atom_type_obj: &Type,
        property_name: &OkIdentifierNode,
    ) -> StructPropertyCheckResult {
        let property_name_str = Rc::new(property_name.token_value(&self.code));
        match atom_type_obj.0.as_ref() {
            CoreType::STRUCT(struct_type) => {
                let struct_data = struct_type
                    .symbol_data
                    .0
                    .as_ref()
                    .borrow()
                    .struct_data(STRUCT_NAME_NOT_BINDED_WITH_STRUCT_VARIANT_SYMBOL_DATA_MSG)
                    .clone();
                match struct_data.try_field(&property_name_str) {
                    Some((type_obj, _)) => {
                        return StructPropertyCheckResult::PROPERTY_EXIST((struct_data, type_obj))
                    }
                    None => {
                        return StructPropertyCheckResult::PROPERTY_DOES_NOT_EXIST(
                            struct_data.clone(),
                        )
                    }
                }
            }
            _ => return StructPropertyCheckResult::NON_STRUCT_TYPE,
        }
    }

    pub fn check_atom(&mut self, atom: &AtomNode) -> (Type, Option<Type>) {
        let core_atom = atom.core_ref();
        match core_atom {
            CoreAtomNode::ATOM_START(atom_start) => (self.check_atom_start(atom_start), None),
            CoreAtomNode::CALL(call) => {
                let core_call = call.core_ref();
                let atom = &core_call.atom;
                let params = &core_call.params;
                let (atom_type_obj, _) = self.check_atom(atom);
                match self.is_callable(&atom_type_obj) {
                    Some((expected_param_types, return_type)) => {
                        let result = self.check_params_type_and_count(
                            CallableParamsData::LAMBDA(expected_param_types),
                            params,
                        );
                        match result {
                            ParamsTypeNCountResult::OK => {
                                return (return_type, Some(atom_type_obj))
                            }
                            _ => {
                                self.log_params_type_and_count_check_error(atom.range(), result);
                                return (Type::new_with_unknown(), Some(atom_type_obj));
                            }
                        }
                    }
                    None => {
                        let err = ExpressionNotCallableError::new(atom.range());
                        self.errors.push(Diagnostics::ExpressionNotCallable(err));
                        return (Type::new_with_unknown(), Some(atom_type_obj));
                    }
                }
            }
            CoreAtomNode::PROPERTRY_ACCESS(property_access) => {
                let core_property_access = property_access.core_ref();
                let atom = &core_property_access.atom;
                let (atom_type_obj, _) = self.check_atom(atom);
                let property = &core_property_access.propertry;
                if let CoreIdentifierNode::OK(ok_identifier) = property.core_ref() {
                    let result = self.check_struct_property(&atom_type_obj, ok_identifier);
                    match result {
                        StructPropertyCheckResult::PROPERTY_EXIST((_, type_obj)) => {
                            return (type_obj, Some(atom_type_obj))
                        }
                        StructPropertyCheckResult::PROPERTY_DOES_NOT_EXIST(_) => {
                            let err = PropertyDoesNotExistError::new(
                                PropertyKind::FIELD,
                                atom_type_obj.clone(),
                                ok_identifier.range(),
                                atom.range(),
                            );
                            self.errors.push(Diagnostics::PropertyDoesNotExist(err));
                            return (Type::new_with_unknown(), Some(atom_type_obj));
                        }
                        StructPropertyCheckResult::NON_STRUCT_TYPE => {
                            let err = PropertyDoesNotExistError::new(
                                PropertyKind::FIELD,
                                atom_type_obj.clone(),
                                property.range(),
                                atom.range(),
                            );
                            self.errors.push(Diagnostics::PropertyDoesNotExist(err));
                            return (Type::new_with_unknown(), Some(atom_type_obj));
                        }
                    }
                }
                (Type::new_with_unknown(), Some(atom_type_obj))
            }
            CoreAtomNode::METHOD_ACCESS(method_access) => {
                let core_method_access = method_access.core_ref();
                let atom = &core_method_access.atom;
                let (atom_type_obj, _) = self.check_atom(atom);
                let method = &core_method_access.method_name;
                let params = &core_method_access.params;
                if let CoreIdentifierNode::OK(ok_identifier) = method.core_ref() {
                    // for syntax `<struct_obj>.<property_name>([<params>])` first type-checker tries to find `property_name` in fields
                    // (for example: a field with lambda type) and then it goes on to find it in methods.
                    // This is sync with what Python does.
                    let result = self.check_struct_property(&atom_type_obj, ok_identifier);
                    let method_name = ok_identifier.token_value(&self.code);
                    match result {
                        StructPropertyCheckResult::PROPERTY_EXIST((_, type_obj)) => {
                            match self.is_callable(&type_obj) {
                                Some((expected_param_types, return_type)) => {
                                    let result = self.check_params_type_and_count(
                                        CallableParamsData::LAMBDA(expected_param_types),
                                        params,
                                    );
                                    match result {
                                        ParamsTypeNCountResult::OK => {
                                            return (return_type, Some(atom_type_obj))
                                        }
                                        _ => {
                                            self.log_params_type_and_count_check_error(
                                                ok_identifier.range(),
                                                result,
                                            );
                                            return (Type::new_with_unknown(), Some(atom_type_obj));
                                        }
                                    }
                                }
                                None => {
                                    let err = StructFieldNotCallableError::new(
                                        type_obj,
                                        ok_identifier.range(),
                                    );
                                    self.errors.push(Diagnostics::StructFieldNotCallable(err));
                                    return (Type::new_with_unknown(), Some(atom_type_obj));
                                }
                            }
                        }
                        StructPropertyCheckResult::PROPERTY_DOES_NOT_EXIST(struct_data) => {
                            match struct_data.try_method(&Rc::new(method_name)) {
                                Some((func_data, _)) => {
                                    let expected_params = &func_data.params;
                                    let return_type = &func_data.return_type;
                                    let result = self.check_params_type_and_count(
                                        CallableParamsData::OTHER(expected_params.clone()),
                                        params,
                                    );
                                    match result {
                                        ParamsTypeNCountResult::OK => {
                                            return (return_type.clone(), Some(atom_type_obj))
                                        }
                                        _ => {
                                            self.log_params_type_and_count_check_error(
                                                method.range(),
                                                result,
                                            );
                                            return (Type::new_with_unknown(), Some(atom_type_obj));
                                        }
                                    }
                                }
                                None => {
                                    let err = PropertyDoesNotExistError::new(
                                        PropertyKind::METHOD,
                                        atom_type_obj.clone(),
                                        method.range(),
                                        atom.range(),
                                    );
                                    self.errors.push(Diagnostics::PropertyDoesNotExist(err));
                                    return (Type::new_with_unknown(), Some(atom_type_obj));
                                }
                            }
                        }
                        StructPropertyCheckResult::NON_STRUCT_TYPE => {
                            let err = PropertyDoesNotExistError::new(
                                PropertyKind::METHOD,
                                atom_type_obj.clone(),
                                method.range(),
                                atom.range(),
                            );
                            self.errors.push(Diagnostics::PropertyDoesNotExist(err));
                            return (Type::new_with_unknown(), Some(atom_type_obj));
                        }
                    }
                }
                (Type::new_with_unknown(), Some(atom_type_obj))
            }
            CoreAtomNode::INDEX_ACCESS(index_access) => {
                let core_index_access = index_access.core_ref();
                let atom = &core_index_access.atom;
                let (atom_type_obj, _) = self.check_atom(atom);
                let index_expr = &core_index_access.index;
                let index_type_obj = self.check_expr(index_expr);
                match atom_type_obj.0.as_ref() {
                    CoreType::TUPLE(tuple) => {
                        let sub_types = &tuple.sub_types;
                        match index_expr.core_ref() {
                            CoreExpressionNode::UNARY(index_unary_expr) => {
                                match self.is_unary_expr_int_valued(index_unary_expr) {
                                    Some(index_value) => {
                                        match self
                                            .is_valid_index_for_tuple(index_value, sub_types.len())
                                        {
                                            TupleIndexCheckResult::OK(index_value) => {
                                                return (
                                                    sub_types[index_value].clone(),
                                                    Some(atom_type_obj),
                                                )
                                            }
                                            TupleIndexCheckResult::POSITIVE_INDEX_OUT_OF_BOUND => {
                                                let err = TupleIndexOutOfBoundError::new(
                                                    sub_types.len(),
                                                    index_expr.range(),
                                                );
                                                self.errors
                                                    .push(Diagnostics::TupleIndexOutOfBound(err));
                                                return (
                                                    Type::new_with_unknown(),
                                                    Some(atom_type_obj),
                                                );
                                            }
                                            TupleIndexCheckResult::NEGATIVE_INDEX_OUT_OF_BOUND => {
                                                let err = TupleIndexOutOfBoundError::new(
                                                    sub_types.len(),
                                                    index_expr.range(),
                                                );
                                                self.errors
                                                    .push(Diagnostics::TupleIndexOutOfBound(err));
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
                                        self.errors.push(
                                            Diagnostics::UnresolvedIndexExpressionInTuple(err),
                                        );
                                        return (Type::new_with_unknown(), Some(atom_type_obj));
                                    }
                                }
                            }
                            CoreExpressionNode::BINARY(_)
                            | CoreExpressionNode::COMPARISON(_)
                            | CoreExpressionNode::MISSING_TOKENS(_) => {
                                let err =
                                    InvalidIndexExpressionForTupleError::new(index_expr.range());
                                self.errors
                                    .push(Diagnostics::InvalidIndexExpressionForTuple(err));
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
                                    atom_type_obj.clone(),
                                    index_type_obj,
                                    atom.range(),
                                    index_expr.range(),
                                );
                                self.errors
                                    .push(Diagnostics::ExpressionIndexingNotValid(err));
                                return (Type::new_with_unknown(), Some(atom_type_obj));
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn check_r_assign(&mut self, r_assign: &RAssignmentNode) -> Type {
        let core_r_assign = r_assign.core_ref();
        match core_r_assign {
            CoreRAssignmentNode::EXPRESSION(expr_stmt) => {
                self.check_expr(&expr_stmt.core_ref().expr)
            }
            CoreRAssignmentNode::MISSING_TOKENS(_) => Type::new_with_unknown(),
        }
    }

    pub fn check_r_variable_declaration(
        &mut self,
        r_variable_decl: &RVariableDeclarationNode,
    ) -> Type {
        let core_r_variable_decl = r_variable_decl.core_ref();
        match core_r_variable_decl {
            CoreRVariableDeclarationNode::EXPRESSION(expr_stmt) => {
                self.check_expr(&expr_stmt.core_ref().expr)
            }
            CoreRVariableDeclarationNode::LAMBDA(lambda) => {
                self.check_callable_body(&lambda.core_ref().body);
                return self.type_of_lambda(lambda);
            }
            CoreRVariableDeclarationNode::MISSING_TOKENS(_) => Type::new_with_unknown(),
        }
    }

    pub fn check_token(&mut self, token: &TokenNode, kind: AtomicTokenExprKind) -> Type {
        match token.core_ref() {
            CoreTokenNode::OK(_) => match kind {
                AtomicTokenExprKind::INTEGER => Type::new_with_atomic(INT),
                AtomicTokenExprKind::BOOL => Type::new_with_atomic(BOOL),
                AtomicTokenExprKind::FLOAT => Type::new_with_atomic(FLOAT),
                AtomicTokenExprKind::LITERAL => Type::new_with_atomic(STRING),
            },
            _ => Type::new_with_unknown(),
        }
    }

    pub fn check_atomic_expr(&mut self, atomic_expr: &AtomicExpressionNode) -> Type {
        let core_atomic_expr = atomic_expr.core_ref();
        match core_atomic_expr {
            CoreAtomicExpressionNode::BOOL_VALUE(token) => {
                self.check_token(token, AtomicTokenExprKind::BOOL)
            }
            CoreAtomicExpressionNode::INTEGER(token) => {
                self.check_token(token, AtomicTokenExprKind::INTEGER)
            }
            CoreAtomicExpressionNode::FLOATING_POINT_NUMBER(token) => {
                self.check_token(token, AtomicTokenExprKind::FLOAT)
            }
            CoreAtomicExpressionNode::LITERAL(token) => {
                self.check_token(token, AtomicTokenExprKind::LITERAL)
            }
            CoreAtomicExpressionNode::PARENTHESISED_EXPRESSION(parenthesised_expr) => {
                self.check_expr(&parenthesised_expr.core_ref().expr)
            }
            CoreAtomicExpressionNode::ATOM(atom) => self.check_atom(atom).0,
            CoreAtomicExpressionNode::MISSING_TOKENS(_) => Type::new_with_unknown(),
        }
    }

    pub fn check_only_unary_expr(&mut self, only_unary_expr: &OnlyUnaryExpressionNode) -> Type {
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
                        operand_type,
                        "numeric (`int`, `float`)",
                        "`+` or `-`",
                        unary_expr.range(),
                        operator.range(),
                    );
                    self.errors.push(Diagnostics::UnaryOperatorInvalidUse(err));
                    return Type::new_with_unknown();
                }
            }
            UnaryOperatorKind::Not => {
                if operand_type.is_bool() {
                    return operand_type;
                } else {
                    let err = UnaryOperatorInvalidUseError::new(
                        operand_type,
                        "boolean",
                        "`not`",
                        unary_expr.range(),
                        operator.range(),
                    );
                    self.errors.push(Diagnostics::UnaryOperatorInvalidUse(err));
                    return Type::new_with_unknown();
                }
            }
        }
    }

    pub fn check_unary_expr(&mut self, unary_expr: &UnaryExpressionNode) -> Type {
        let core_unary_expr = unary_expr.core_ref();
        match core_unary_expr {
            CoreUnaryExpressionNode::ATOMIC(atomic) => self.check_atomic_expr(atomic),
            CoreUnaryExpressionNode::UNARY(unary) => self.check_only_unary_expr(unary),
            _ => Type::new_with_unknown(),
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
            Some(type_obj) => return type_obj,
            None => {
                let err = BinaryOperatorInvalidOperandsError::new(
                    l_type,
                    r_type,
                    left_expr.range(),
                    right_expr.range(),
                    operator.range(),
                );
                self.errors
                    .push(Diagnostics::BinaryOperatorInvalidOperands(err));
                return Type::new_with_unknown();
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
            assert!(
                operator_kind.is_comparison(),
                "all the operators in `ComparisonNode` should be comparison operators"
            );
            let result = self.is_binary_operation_valid(&l_type, &r_type, &operator_kind);
            match result {
                Some(type_obj) => match type_obj.0.as_ref() {
                    CoreType::ATOMIC(atomic) => assert!(atomic.is_bool()),
                    CoreType::UNKNOWN => return Type::new_with_unknown(),
                    _ => unreachable!("comparison operator always result into `bool` type"),
                },
                None => {
                    let err = BinaryOperatorInvalidOperandsError::new(
                        l_type,
                        r_type,
                        left_expr.range(),
                        right_expr.range(),
                        operator.range(),
                    );
                    self.errors
                        .push(Diagnostics::BinaryOperatorInvalidOperands(err));
                    return Type::new_with_unknown();
                }
            }
        }
        Type::new_with_atomic(BOOL)
    }

    pub fn check_expr(&mut self, expr: &ExpressionNode) -> Type {
        let core_expr = expr.core_ref();
        match core_expr {
            CoreExpressionNode::UNARY(unary_expr) => self.check_unary_expr(unary_expr),
            CoreExpressionNode::BINARY(binary_expr) => self.check_binary_expr(binary_expr),
            CoreExpressionNode::COMPARISON(comparison_expr) => {
                self.check_comp_expr(comparison_expr)
            }
            _ => Type::new_with_unknown(),
        }
    }

    pub fn check_assignment(&mut self, assignment: &AssignmentNode) {
        let core_assignment = assignment.core_ref();
        let (l_type, r_assign, range) = match core_assignment {
            CoreAssignmentNode::OK(ok_assignment) => {
                let core_ok_assignment = ok_assignment.core_ref();
                let l_expr = &core_ok_assignment.l_atom;
                let (l_type, interior_atom_type) = self.check_atom(l_expr);
                if let CoreAtomNode::INDEX_ACCESS(l_index_expr) = l_expr.core_ref() {
                    if let Some(interior_atom_type) = interior_atom_type {
                        if interior_atom_type.is_immutable() {
                            let err = ImmutableTypeNotAssignableError::new(
                                &interior_atom_type,
                                l_index_expr.core_ref().atom.range(),
                            );
                            self.errors
                                .push(Diagnostics::ImmutableTypeNotAssignable(err));
                        }
                    }
                }
                let r_assign = &core_ok_assignment.r_assign;
                (l_type, r_assign, l_expr.range())
            }
            CoreAssignmentNode::INVALID_L_VALUE(invalid_l_value) => {
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
            self.errors
                .push(Diagnostics::RightSideWithVoidTypeNotAllowed(err));
            return;
        }
        if !l_type.is_eq(&r_type) {
            let err = MismatchedTypesOnLeftRightError::new(l_type, r_type, range, r_assign.range());
            self.errors
                .push(Diagnostics::MismatchedTypesOnLeftRight(err));
        }
    }

    pub fn check_variable_decl(&mut self, variable_decl: &VariableDeclarationNode) {
        let core_variable_decl = variable_decl.core_ref();
        let r_variable_decl = &core_variable_decl.r_node;
        let r_type = self.check_r_variable_declaration(r_variable_decl);
        if r_type.is_void() {
            let err = RightSideWithVoidTypeNotAllowedError::new(r_variable_decl.range());
            self.errors
                .push(Diagnostics::RightSideWithVoidTypeNotAllowed(err));
        }
        if let CoreIdentifierNode::OK(ok_identifier) = core_variable_decl.name.core_ref() {
            if let Some(symbol_data) = ok_identifier.variable_symbol_data(
                "variable name should be resolved to `SymbolData<VariableData>`",
            ) {
                symbol_data.0.as_ref().borrow_mut().set_data_type(&r_type);
            }
        };
    }

    pub fn check_callable_prototype(&mut self, callable_prototype: &CallablePrototypeNode) -> Type {
        let core_callable_prototype = callable_prototype.0.as_ref();
        let return_type_node = &core_callable_prototype.return_type;
        let return_type_obj = match return_type_node {
            Some(return_type_expr) => self.type_obj_from_expression(return_type_expr),
            None => Type::new_with_void(),
        };
        return_type_obj
    }

    pub fn check_callable_body(&mut self, callable_body: &CallableBodyNode) {
        let core_callable_body = callable_body.0.as_ref();
        match core_callable_body {
            CoreCallableBodyNode::OK(ok_callable_body) => {
                let core_ok_callable_body = ok_callable_body.core_ref();
                let return_type_obj =
                    self.check_callable_prototype(&core_ok_callable_body.prototype);
                self.context.func_stack.push(return_type_obj.clone());
                let mut has_return_stmt = false;
                let mut has_atleast_one_stmt = false;
                for stmt in &core_ok_callable_body.block.0.as_ref().borrow().stmts {
                    let stmt = match stmt.core_ref() {
                        CoreStatemenIndentWrapperNode::CORRECTLY_INDENTED(stmt) => stmt.clone(),
                        CoreStatemenIndentWrapperNode::INCORRECTLY_INDENTED(stmt) => {
                            let core_stmt = stmt.core_ref();
                            core_stmt.stmt.clone()
                        }
                        _ => continue,
                    };
                    self.walk_stmt(&stmt);
                    has_atleast_one_stmt = true;
                    if let CoreStatementNode::RETURN(_) = stmt.core_ref() {
                        has_return_stmt = true;
                        // TODO - we can break here as any statement following return statement is dead code
                    }
                }
                if !has_atleast_one_stmt {
                    let err = NoValidStatementInsideFunctionBody::new(
                        core_ok_callable_body.colon.range(),
                    );
                    self.errors
                        .push(Diagnostics::NoValidStatementInsideFunctionBody(err));
                } else {
                    if !has_return_stmt && !return_type_obj.is_void() {
                        let return_type_node = ok_callable_body
                            .core_ref()
                            .prototype
                            .core_ref()
                            .return_type
                            .as_ref()
                            .unwrap();
                        let err = NoReturnStatementInFunctionError::new(return_type_node.range());
                        self.errors
                            .push(Diagnostics::NoReturnStatementInFunction(err));
                    }
                }
                self.context.func_stack.pop();
            }
            CoreCallableBodyNode::MISSING_TOKENS(_) => return,
        }
    }

    pub fn check_return_stmt(&mut self, return_stmt: &ReturnStatementNode) {
        let core_return_stmt = return_stmt.core_ref();
        let func_stack_len = self.context.func_stack.len();
        if func_stack_len == 0 {
            let err = InvalidReturnStatementError::new(return_stmt.range());
            self.errors.push(Diagnostics::InvalidReturnStatement(err));
        }
        let expr = &core_return_stmt.expr;
        let expr_type_obj = match expr {
            Some(expr) => self.check_expr(expr),
            _ => Type::new_with_void(),
        };
        let expected_type_obj = self.context.func_stack[func_stack_len - 1].clone();
        if !expr_type_obj.is_eq(&expected_type_obj) {
            let err = MismatchedReturnTypeError::new(
                expected_type_obj,
                expr_type_obj,
                core_return_stmt.return_keyword.range(),
            );
            self.errors.push(Diagnostics::MismatchedReturnType(err));
        }
    }

    pub fn check_stmt(&mut self, stmt: &StatementNode) {
        match stmt.core_ref() {
            CoreStatementNode::EXPRESSION(expr_stmt) => {
                let core_expr_stmt = expr_stmt.core_ref();
                self.check_expr(&core_expr_stmt.expr);
            }
            CoreStatementNode::ASSIGNMENT(assignment) => {
                self.check_assignment(assignment);
            }
            CoreStatementNode::VARIABLE_DECLARATION(variable_decl) => {
                self.check_variable_decl(variable_decl);
            }
            CoreStatementNode::FUNCTION_WRAPPER(func_wrapper) => {
                self.check_callable_body(&func_wrapper.core_ref().func_decl.core_ref().body);
            }
            CoreStatementNode::BOUNDED_METHOD_WRAPPER(bounded_method_wrapper) => {
                self.check_callable_body(
                    &bounded_method_wrapper
                        .0
                        .as_ref()
                        .borrow()
                        .func_decl
                        .core_ref()
                        .body,
                );
            }
            CoreStatementNode::RETURN(return_stmt) => {
                self.check_return_stmt(return_stmt);
            }
            CoreStatementNode::TYPE_DECLARATION(type_decl) => match type_decl.core_ref() {
                CoreTypeDeclarationNode::STRUCT(struct_decl) => {
                    self.walk_block(&struct_decl.core_ref().block);
                }
                CoreTypeDeclarationNode::LAMBDA(_) | CoreTypeDeclarationNode::MISSING_TOKENS(_) => {
                    return
                }
            },
            CoreStatementNode::STRUCT_PROPERTY_DECLARATION(_)
            | CoreStatementNode::MISSING_TOKENS(_) => return,
        }
    }

    pub fn log_params_type_and_count_check_error(
        &mut self,
        range: TextRange,
        result: ParamsTypeNCountResult,
    ) {
        match result {
            ParamsTypeNCountResult::OK => return,
            ParamsTypeNCountResult::LESS_PARAMS((expected_params_count, received_params_count)) => {
                let err =
                    LessParamsCountError::new(expected_params_count, received_params_count, range);
                self.errors.push(Diagnostics::LessParamsCount(err));
            }
            ParamsTypeNCountResult::MORE_PARAMS(expected_params_count) => {
                let err = MoreParamsCountError::new(expected_params_count, range);
                self.errors.push(Diagnostics::MoreParamsCount(err));
            }
            ParamsTypeNCountResult::MISMATCHED_TYPE(params_vec) => {
                let err = MismatchedParamTypeError::new(params_vec);
                self.errors.push(Diagnostics::MismatchedParamType(err));
            }
        }
    }
}

impl Visitor for TypeChecker {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match node {
            ASTNode::STATEMENT(stmt) => {
                self.check_stmt(stmt);
                return None;
            }
            _ => Some(()),
        }
    }
}
