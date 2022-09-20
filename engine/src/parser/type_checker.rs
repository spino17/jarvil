// See `https://www.csd.uwo.ca/~mmorenom/CS447/Lectures/TypeChecking.html/node1.html` for information about various cases that type-checker needs to
// cover and the representation of type expressions in terms of type objects.

use crate::{
    ast::{
        ast::{
            ASTNode, AssignmentNode, AtomNode, AtomStartNode, AtomicExpressionNode,
            BinaryExpressionNode, BlockKind, BlockNode, ComparisonNode, CoreAssignmentNode,
            CoreAtomNode, CoreAtomStartNode, CoreAtomicExpressionNode, CoreExpressionNode,
            CoreFunctionDeclarationNode, CoreIdentifierNode, CoreRAssignmentNode,
            CoreStatemenIndentWrapperNode, CoreStatementNode, CoreTokenNode,
            CoreUnaryExpressionNode, ExpressionNode, FunctionDeclarationNode, FunctionKind,
            NameTypeSpecsNode, Node, OkFunctionDeclarationNode, OnlyUnaryExpressionNode,
            ParamsNode, RAssignmentNode, ReturnStatementNode, StatementNode, TokenNode,
            TypeDeclarationNode, TypeExpressionNode, TypeResolveKind, UnaryExpressionNode,
            VariableDeclarationNode,
        },
        walk::Visitor,
    },
    code::Code,
    constants::common::{BOOL, FLOAT, INT, STRING},
    error::{
        constants::{
            LAMBDA_NAME_NOT_BINDED_WITH_LAMBDA_VARIANT_SYMBOL_DATA_MSG, SCOPE_NOT_SET_TO_BLOCK_MSG,
            STRUCT_NAME_NOT_BINDED_WITH_STRUCT_VARIANT_SYMBOL_DATA_MSG,
        },
        diagnostics::{
            BinaryOperatorInvalidOperandsError, ClassmethodDoesNotExistError, Diagnostics,
            ExpressionIndexingNotValidError, ExpressionNotCallableError,
            IdentifierNotCallableError, InvalidReturnStatementError, LessParamsCountError,
            MismatchedParamTypeError, MismatchedReturnTypeError, MismatchedTypesOnLeftRightError,
            MoreParamsCountError, NoReturnStatementInFunctionError, PropertyDoesNotExistError,
            PropertyNotSupportedError, UnaryOperatorInvalidUseError,
        },
        helper::PropertyKind,
    },
    lexer::token::{BinaryOperatorKind, UnaryOperatorKind},
    scope::{
        core::{IdentifierKind, Namespace, SymbolData},
        function::FunctionData,
        user_defined_types::{LambdaTypeData, UserDefinedTypeData},
    },
    types::{
        atomic,
        core::{AbstractType, CoreType, Type},
        operators::{self, check_operator},
    },
};
use std::rc::Rc;
use text_size::TextRange;

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

pub enum ParamsTypeNCountResult {
    OK,
    MORE_PARAMS(usize),
    LESS_PARAMS((usize, usize)), // (expected_params_num, received_params_num)
    MISMATCHED_TYPE(Vec<(String, String, usize, TextRange)>), // (expected_type, received_type, index_of_param, span)
}

pub struct TypeChecker {
    namespace: Namespace,
    code: Code,
    errors: Vec<Diagnostics>,
    context: Context,
}

impl TypeChecker {
    pub fn new(code: &Code, scope: &Namespace) -> Self {
        TypeChecker {
            namespace: scope.clone(),
            code: code.clone(),
            errors: vec![],
            context: Context { func_stack: vec![] },
        }
    }

    pub fn open_scope(&mut self, block: &BlockNode) {
        self.namespace = block.scope().expect(SCOPE_NOT_SET_TO_BLOCK_MSG);
    }

    pub fn close_scope(&mut self) {
        self.namespace.close_scope();
    }

    pub fn check_ast(&mut self, ast: &BlockNode) -> Vec<Diagnostics> {
        let core_block = ast.0.as_ref().borrow();
        for stmt in &core_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        std::mem::take(&mut self.errors)
    }

    pub fn type_obj_from_expression(&self, type_expr: &TypeExpressionNode) -> Type {
        match type_expr.type_obj(&self.namespace, &self.code) {
            TypeResolveKind::RESOLVED(type_obj) => type_obj,
            TypeResolveKind::UNRESOLVED(_) => return Type::new_with_unknown(),
            TypeResolveKind::INVALID => Type::new_with_unknown(),
        }
    }

    pub fn params_and_return_type_obj_from_expr(
        &self,
        return_type: &Option<TypeExpressionNode>,
        params: &Option<NameTypeSpecsNode>,
    ) -> (Vec<(Rc<String>, Type)>, Type) {
        let mut params_vec: Vec<(Rc<String>, Type)> = vec![];
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
                        let variable_name = Rc::new(ok_identifier.token_value(&self.code));
                        let type_obj = self.type_obj_from_expression(&core_param.data_type);
                        params_vec.push((variable_name, type_obj));
                    }
                }
            }
        }
        (params_vec, return_type)
    }

    pub fn type_of_lambda(&self, func_decl: &OkFunctionDeclarationNode) -> Type {
        let core_func_decl = func_decl.core_ref();
        assert!(
            core_func_decl.kind == FunctionKind::LAMBDA,
            "construction of type is only valid for lambda declaration"
        );
        let func_name = &core_func_decl.name;
        let params = &core_func_decl.params;
        let return_type = &core_func_decl.return_type;
        let (params_vec, return_type) = match func_name {
            Some(func_name) => match func_name.core_ref() {
                CoreIdentifierNode::OK(ok_identifier) => match ok_identifier.variable_symbol_data(
                    "lambda name should be resolved to `SymbolData<VariableData>`",
                ) {
                    Some(symbol_data) => return symbol_data.0.as_ref().borrow().data_type.clone(),
                    None => self.params_and_return_type_obj_from_expr(return_type, params),
                },
                _ => self.params_and_return_type_obj_from_expr(return_type, params),
            },
            None => self.params_and_return_type_obj_from_expr(return_type, params),
        };
        let symbol_data = UserDefinedTypeData::LAMBDA(LambdaTypeData::new(params_vec, return_type));
        let lambda_type_obj = Type::new_with_lambda(
            None,
            &SymbolData::new(symbol_data, core_func_decl.lparen.range()),
        );
        lambda_type_obj
    }

    pub fn is_callable(&mut self, atom: &AtomNode) -> Option<FunctionData> {
        let atom_type_obj = self.check_atom(atom);
        match atom_type_obj.0.as_ref() {
            CoreType::LAMBDA(lambda) => Some(
                lambda
                    .symbol_data
                    .0
                    .as_ref()
                    .borrow()
                    .lambda_data(LAMBDA_NAME_NOT_BINDED_WITH_LAMBDA_VARIANT_SYMBOL_DATA_MSG)
                    .func_data
                    .clone(),
            ),
            _ => None,
        }
    }

    pub fn is_indexable_with_type(&mut self, base_type: &Type, index_type: &Type) -> Option<Type> {
        // TODO - type can be (array, int), (hashmap, any type given in the definition)
        match base_type.0.as_ref() {
            CoreType::ARRAY(array) => {
                if index_type.is_int() {
                    return Some(array.element_type.clone());
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
        let result = check_operator(l_type, r_type, operator_kind);
        result
    }

    pub fn check_params_type_and_count(
        &mut self,
        expected_params: &Rc<Vec<(Rc<String>, Type)>>,
        received_params: &Option<ParamsNode>,
    ) -> ParamsTypeNCountResult {
        let expected_params_len = expected_params.len();
        match received_params {
            Some(received_params) => {
                let expected_params = expected_params.as_ref();
                let received_params_iter = received_params.iter();
                let mut index = 0;
                let mut mismatch_types_vec: Vec<(String, String, usize, TextRange)> = vec![]; // (expected_type, received_type, index_of_param)
                for received_param in received_params_iter {
                    let param_type_obj = self.check_expr(&received_param);
                    if index >= expected_params_len {
                        return ParamsTypeNCountResult::MORE_PARAMS(expected_params_len);
                    }
                    let expected_params_type_obj = &expected_params[index].1;
                    if !param_type_obj.is_eq(expected_params_type_obj) {
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
            CoreAtomStartNode::CALL(call_expr) => {
                let core_call_expr = call_expr.core_ref();
                let func_name = &core_call_expr.function_name;
                let params = &core_call_expr.params;
                if let CoreIdentifierNode::OK(ok_identifier) = func_name.core_ref() {
                    if let Some(symbol_data) = ok_identifier.symbol_data() {
                        let (expected_params, return_type) = match symbol_data.0 {
                            IdentifierKind::FUNCTION(func_symbol_data) => {
                                let func_data = func_symbol_data.0.as_ref().borrow().clone();
                                let expected_params = func_data.params;
                                let return_type = func_data.return_type;
                                (expected_params, return_type)
                            },
                            IdentifierKind::VARIABLE(variable_symbol_data) => {
                                let lambda_type = variable_symbol_data.0.as_ref().borrow().data_type.clone();
                                match lambda_type.0.as_ref() {
                                    CoreType::LAMBDA(lambda_data) => {
                                        let func_data = lambda_data.symbol_data.0.as_ref().borrow().lambda_data(
                                            LAMBDA_NAME_NOT_BINDED_WITH_LAMBDA_VARIANT_SYMBOL_DATA_MSG
                                        ).func_data.clone();
                                        let expected_params = func_data.params;
                                        let return_type = func_data.return_type;
                                        (expected_params, return_type)
                                    },
                                    _ => {
                                        let err = IdentifierNotCallableError::new(
                                            lambda_type, func_name.range()
                                        );
                                        self.errors.push(Diagnostics::IdentifierNotCallable(err));
                                        return Type::new_with_unknown()
                                    }
                                }
                            },  // TODO - handle case when the call is constructor call
                            _ => unreachable!("function name should be resolved to `SymbolData<FunctionData>` or `SymbolData<VariableData>`")
                        };
                        let result = self.check_params_type_and_count(&expected_params, params);
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
                                match struct_data
                                    .class_methods
                                    .as_ref()
                                    .borrow()
                                    .get(&class_method_name)
                                {
                                    Some(func_data) => {
                                        let expected_params = func_data.params.clone();
                                        let return_type = func_data.return_type.clone();
                                        let result = self
                                            .check_params_type_and_count(&expected_params, params);
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

    pub fn check_atom(&mut self, atom: &AtomNode) -> Type {
        let core_atom = atom.core_ref();
        match core_atom {
            CoreAtomNode::ATOM_START(atom_start) => self.check_atom_start(atom_start),
            CoreAtomNode::CALL(call) => {
                let core_call = call.core_ref();
                let atom = &core_call.atom;
                let params = &core_call.params;
                match self.is_callable(atom) {
                    Some(func_data) => {
                        let expected_params = func_data.params;
                        let return_type = func_data.return_type;
                        let result = self.check_params_type_and_count(&expected_params, params);
                        match result {
                            ParamsTypeNCountResult::OK => return return_type,
                            _ => {
                                self.log_params_type_and_count_check_error(atom.range(), result);
                                return Type::new_with_unknown();
                            }
                        }
                    }
                    None => {
                        let err = ExpressionNotCallableError::new(atom.range());
                        self.errors.push(Diagnostics::ExpressionNotCallable(err));
                        return Type::new_with_unknown();
                    }
                }
            }
            CoreAtomNode::PROPERTRY_ACCESS(property_access) => {
                let core_property_access = property_access.core_ref();
                let atom = &core_property_access.atom;
                let atom_type_obj = self.check_atom(atom);
                let property = &core_property_access.propertry;
                if let CoreIdentifierNode::OK(ok_identifier) = property.core_ref() {
                    let property_name = Rc::new(ok_identifier.token_value(&self.code));
                    match atom_type_obj.0.as_ref() {
                        CoreType::STRUCT(struct_type) => {
                            match struct_type
                                .symbol_data
                                .0
                                .as_ref()
                                .borrow()
                                .struct_data(
                                    STRUCT_NAME_NOT_BINDED_WITH_STRUCT_VARIANT_SYMBOL_DATA_MSG,
                                )
                                .try_field(&property_name)
                            {
                                Some((type_obj, _)) => return type_obj,
                                None => {
                                    let err = PropertyDoesNotExistError::new(
                                        PropertyKind::FIELD,
                                        atom_type_obj.clone(),
                                        property.range(),
                                        atom.range(),
                                    );
                                    self.errors.push(Diagnostics::PropertyDoesNotExist(err));
                                    return Type::new_with_unknown();
                                }
                            }
                        }
                        _ => {
                            let err = PropertyDoesNotExistError::new(
                                PropertyKind::FIELD,
                                atom_type_obj,
                                property.range(),
                                atom.range(),
                            );
                            self.errors.push(Diagnostics::PropertyDoesNotExist(err));
                            return Type::new_with_unknown();
                        }
                    }
                }
                Type::new_with_unknown()
            }
            CoreAtomNode::METHOD_ACCESS(method_access) => {
                // TODO - check for possiblitiy of a field access with type lambda which will have similar syntax
                let core_method_access = method_access.core_ref();
                let atom = &core_method_access.atom;
                let atom_type_obj = self.check_atom(atom);
                let method = &core_method_access.method_name;
                let params = &core_method_access.params;
                if let CoreIdentifierNode::OK(ok_identifier) = method.core_ref() {
                    let method_name = ok_identifier.token_value(&self.code);
                    match atom_type_obj.0.as_ref() {
                        CoreType::STRUCT(struct_type) => {
                            match struct_type
                                .symbol_data
                                .0
                                .as_ref()
                                .borrow()
                                .struct_data(
                                    STRUCT_NAME_NOT_BINDED_WITH_STRUCT_VARIANT_SYMBOL_DATA_MSG,
                                )
                                .try_method(&method_name)
                            {
                                Some(func_data) => {
                                    let expected_params = &func_data.params;
                                    let return_type = &func_data.return_type;
                                    let result =
                                        self.check_params_type_and_count(&expected_params, params);
                                    match result {
                                        ParamsTypeNCountResult::OK => return return_type.clone(),
                                        _ => {
                                            self.log_params_type_and_count_check_error(
                                                method.range(),
                                                result,
                                            );
                                            return Type::new_with_unknown();
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
                                    return Type::new_with_unknown();
                                }
                            }
                        }
                        _ => {
                            let err = PropertyDoesNotExistError::new(
                                PropertyKind::METHOD,
                                atom_type_obj,
                                method.range(),
                                atom.range(),
                            );
                            self.errors.push(Diagnostics::PropertyDoesNotExist(err));
                            return Type::new_with_unknown();
                        }
                    }
                }
                Type::new_with_unknown()
            }
            CoreAtomNode::INDEX_ACCESS(index_access) => {
                let core_index_access = index_access.core_ref();
                let atom = &core_index_access.atom;
                let atom_type_obj = self.check_atom(atom);
                let index = &core_index_access.index;
                let index_type_obj = self.check_expr(index);
                match self.is_indexable_with_type(&atom_type_obj, &index_type_obj) {
                    Some(element_type) => return element_type.clone(),
                    _ => {
                        let err = ExpressionIndexingNotValidError::new(
                            atom_type_obj,
                            index_type_obj,
                            atom.range(),
                            index.range(),
                        );
                        self.errors
                            .push(Diagnostics::ExpressionIndexingNotValid(err));
                        return Type::new_with_unknown();
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
            CoreRAssignmentNode::LAMBDA(lambda) => {
                let core_lambda = lambda.core_ref();
                match core_lambda {
                    CoreFunctionDeclarationNode::OK(ok_func_decl) => {
                        self.check_func_decl(ok_func_decl);
                        return self.type_of_lambda(ok_func_decl);
                    }
                    _ => Type::new_with_unknown(),
                }
            }
            _ => Type::new_with_unknown(),
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
            CoreAtomicExpressionNode::ATOM(atom) => self.check_atom(atom),
            _ => Type::new_with_unknown(),
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
                let l_type = self.check_atom(l_expr);
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
        if !l_type.is_eq(&r_type) {
            let err = MismatchedTypesOnLeftRightError::new(l_type, r_type, range, r_assign.range());
            self.errors
                .push(Diagnostics::MismatchedTypesOnLeftRight(err));
        }
    }

    pub fn check_variable_decl(&mut self, variable_decl: &VariableDeclarationNode) {
        let core_variable_decl = variable_decl.core_ref();
        let r_type = self.check_r_assign(&core_variable_decl.r_assign);
        if let CoreIdentifierNode::OK(ok_identifier) = core_variable_decl.name.core_ref() {
            if !r_type.is_lambda() {
                // variable with lambda type is already set in resolving phase => see `resolve_function` method
                if let Some(symbol_data) = ok_identifier.variable_symbol_data(
                    "variable name should be resolved to `SymbolData<VariableData>`",
                ) {
                    symbol_data.0.as_ref().borrow_mut().set_data_type(&r_type);
                }
            }
        };
    }

    pub fn check_func_decl(&mut self, ok_func_decl: &OkFunctionDeclarationNode) {
        let core_ok_func_decl = ok_func_decl.core_ref();
        let return_type_node = &core_ok_func_decl.return_type;
        let return_type_obj = match return_type_node {
            Some(return_type_expr) => self.type_obj_from_expression(return_type_expr),
            None => Type::new_with_void(),
        };
        self.open_scope(&core_ok_func_decl.block);
        self.context.func_stack.push(return_type_obj.clone());
        let mut has_return_stmt = false;
        for stmt in &core_ok_func_decl.block.0.as_ref().borrow().stmts {
            let stmt = match stmt.core_ref() {
                CoreStatemenIndentWrapperNode::CORRECTLY_INDENTED(stmt) => stmt.clone(),
                CoreStatemenIndentWrapperNode::INCORRECTLY_INDENTED(stmt) => {
                    let core_stmt = stmt.core_ref();
                    core_stmt.stmt.clone()
                }
                _ => continue,
            };
            self.walk_stmt(&stmt);
            if let CoreStatementNode::RETURN(_) = stmt.core_ref() {
                has_return_stmt = true;
                // TODO - we can break here as any statement following return statement is dead code
            }
        }
        if !has_return_stmt && !return_type_obj.is_void() {
            let return_type_node = return_type_node.as_ref().unwrap();
            let err = NoReturnStatementInFunctionError::new(return_type_node.range());
            self.errors
                .push(Diagnostics::NoReturnStatementInFunction(err));
        }
        self.close_scope();
        self.context.func_stack.pop();
    }

    pub fn check_return_stmt(&mut self, return_stmt: &ReturnStatementNode) {
        let core_return_stmt = return_stmt.core_ref();
        let func_stack_len = self.context.func_stack.len();
        if func_stack_len == 0 {
            let err = InvalidReturnStatementError::new(return_stmt.range());
            self.errors.push(Diagnostics::InvalidReturnStatement(err));
        }
        let expr = &core_return_stmt.expr;
        let expr_type_obj = self.check_expr(expr);
        let expected_type_obj = self.context.func_stack[func_stack_len - 1].clone();
        if !expr_type_obj.is_eq(&expected_type_obj) {
            let err =
                MismatchedReturnTypeError::new(expected_type_obj, expr_type_obj, expr.range());
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
            CoreStatementNode::FUNCTION_DECLARATION(func_decl) => {
                if let CoreFunctionDeclarationNode::OK(ok_func_decl) = func_decl.core_ref() {
                    self.check_func_decl(ok_func_decl);
                }
            }
            CoreStatementNode::RETURN(return_stmt) => {
                self.check_return_stmt(return_stmt);
            }
            _ => return,
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
