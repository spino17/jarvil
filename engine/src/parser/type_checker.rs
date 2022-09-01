// See `https://www.csd.uwo.ca/~mmorenom/CS447/Lectures/TypeChecking.html/node1.html` for information about various cases that type-checker needs to
// cover and the representation of type expressions in terms of type objects.

use std::rc::Rc;

use crate::{
    ast::{
        ast::{
            ASTNode, AssignmentNode, AtomNode, AtomStartNode, AtomicExpressionNode,
            BinaryExpressionNode, BlockKind, BlockNode, ComparisonNode, CoreAssignmentNode,
            CoreAtomNode, CoreAtomicExpressionNode, CoreExpressionNode,
            CoreFunctionDeclarationNode, CoreIdentifierNode, CoreRAssignmentNode,
            CoreStatemenIndentWrapperNode, CoreStatementNode, CoreTokenNode,
            CoreUnaryExpressionNode, ExpressionNode, FunctionDeclarationNode, NameTypeSpecsNode,
            Node, OkFunctionDeclarationNode, OnlyUnaryExpressionNode, ParamsNode, RAssignmentNode,
            ReturnStatementNode, StatementNode, TokenNode, TypeDeclarationNode, TypeExpressionNode,
            TypeResolveKind, UnaryExpressionNode, UnaryOperatorKind, VariableDeclarationNode,
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
        core::{JarvilError, JarvilErrorKind},
    },
    scope::{
        core::{IdentifierKind, Namespace, SymbolData},
        function::FunctionData,
        user_defined_types::{LambdaTypeData, UserDefinedTypeData},
    },
    types::{
        atomic,
        core::{AbstractType, CoreType, Type},
    },
};
use text_size::TextRange;

struct Context {
    func_stack: Vec<Type>,
}

pub enum AtomicTokenExprKind {
    BOOL,
    INTEGER,
    FLOAT,
    LITERAL,
}

pub struct TypeChecker {
    namespace: Namespace,
    code: Code,
    errors: Vec<JarvilError>,
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
    ) -> (Vec<(String, Type)>, Type) {
        let mut params_vec: Vec<(String, Type)> = vec![];
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
                        let variable_name = ok_identifier.token_value(&self.code);
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
            core_func_decl.is_lambda,
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
            &SymbolData::new(symbol_data, core_func_decl.lparen.start_line_number()),
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
        todo!()
    }

    pub fn check_params_type_and_count(
        &mut self,
        expected_params: &Rc<Vec<(String, Type)>>,
        received_params: &Option<ParamsNode>,
    ) {
        let expected_params_len = expected_params.len();
        match received_params {
            Some(received_params) => {
                let expected_params = expected_params.as_ref();
                let received_params_iter = received_params.iter();
                let mut index = 0;
                for received_param in received_params_iter {
                    let param_type_obj = self.check_expr(&received_param);
                    if index >= expected_params_len {
                        // TODO - return `more than expected arguments were passed`
                    }
                    let expected_params_type_obj = &expected_params[index].1;
                    if !param_type_obj.is_eq(expected_params_type_obj) {
                        // TODO - return `type mismatch\nexpected type for argument {index} is {}, got {}`
                    }
                    index = index + 1;
                }
                if index < expected_params_len {
                    // TODO - return `expected {} arguments, {} were passed`
                }
            }
            None => {
                if expected_params_len != 0 {
                    // TODO - return unequal params
                } else {
                    // TODO - return success
                }
            }
        }
    }

    pub fn check_ast(&mut self, ast: &BlockNode) -> Vec<JarvilError> {
        let core_block = ast.0.as_ref().borrow();
        for stmt in &core_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        std::mem::take(&mut self.errors)
    }

    pub fn check_atom_start(&mut self, atom_start: &AtomStartNode) -> Type {
        todo!()
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
                        let params_vec = func_data.params;
                        let return_type = func_data.return_type;
                        self.check_params_type_and_count(&params_vec, params);
                        // TODO - check params type and number are correct
                        return return_type;
                    }
                    None => {
                        // TODO - raise error `expression is not callable`
                        return Type::new_with_unknown();
                    }
                }
            }
            CoreAtomNode::PROPERTRY_ACCESS(property_access) => {
                let core_property_access = property_access.core_ref();
                let atom_type_obj = self.check_atom(&core_property_access.atom);
                let property_name = &core_property_access.propertry;
                if let CoreIdentifierNode::OK(ok_identifier) = property_name.core_ref() {
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
                                Some(type_obj) => return type_obj,
                                None => {
                                    // TODO - raise error `no property named `{}` exist for expression with type `{}``
                                    return Type::new_with_unknown();
                                }
                            }
                        }
                        _ => {
                            // TODO - raise error `no property named `{}` exist for expression with type `{}``
                            return Type::new_with_unknown();
                        }
                    }
                }
                Type::new_with_unknown()
            }
            CoreAtomNode::METHOD_ACCESS(method_access) => {
                // TODO - check for possiblitiy of a field access with type lambda which will have similar node
                let core_method_access = method_access.core_ref();
                let atom_type_obj = self.check_atom(&core_method_access.atom);
                let method_name = &core_method_access.method_name;
                let params = &core_method_access.params;
                if let CoreIdentifierNode::OK(ok_identifier) = method_name.core_ref() {
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
                                    self.check_params_type_and_count(expected_params, params);
                                    return return_type.clone();
                                }
                                None => {
                                    // TODO - raise error `no method named `{}` exist for expression with type `{}``
                                    return Type::new_with_unknown();
                                }
                            }
                        }
                        _ => {
                            // TODO - raise error `no method named `{}` exist for expression with type `{}``
                            return Type::new_with_unknown();
                        }
                    }
                }
                Type::new_with_unknown()
            }
            CoreAtomNode::INDEX_ACCESS(index_access) => {
                let core_index_access = index_access.core_ref();
                let atom_type_obj = self.check_atom(&core_index_access.atom);
                let index_type_obj = self.check_expr(&core_index_access.index);
                match self.is_indexable_with_type(&atom_type_obj, &index_type_obj) {
                    Some(element_type) => return element_type.clone(),
                    _ => {
                        // TODO - raise error `expression with type {} is not indexable with value of type {}`
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
                        let core_ok_func_decl = ok_func_decl.core_ref();
                        self.walk_block(&core_ok_func_decl.block);
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
        let operator_kind = &core_only_unary_expr.operator_kind;
        match operator_kind {
            UnaryOperatorKind::PLUS | UnaryOperatorKind::MINUS => {
                if operand_type.is_numeric() {
                    return operand_type;
                } else {
                    let err_message = format!(
                        "unary expression with operator `+` or `-` is valid only for numeric (`int`, `float`) operand, got operand with type `{}`", 
                        operand_type
                    );
                    self.log_error(
                        unary_expr.range(),
                        unary_expr.start_line_number(),
                        err_message
                    );
                    return Type::new_with_unknown();
                }
            }
            UnaryOperatorKind::NOT => {
                if operand_type.is_bool() {
                    let err_message = format!(
                        "unary expression with operator `not` is valid only for boolean operand, got operand with type `{}`", 
                        operand_type
                    );
                    self.log_error(
                        unary_expr.range(),
                        unary_expr.start_line_number(),
                        err_message
                    );
                    return operand_type;
                } else {
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
        let l_type = self.check_expr(&core_binary_expr.left_expr);
        // TODO - check the operator and depending on the operator check whether binary operation is valid for l_type and r_type
        let r_type = self.check_expr(&core_binary_expr.right_expr);
        todo!()
    }

    pub fn check_comp_expr(&mut self, comp_expr: &ComparisonNode) -> Type {
        todo!()
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
        let (l_type, r_assign) = match core_assignment {
            CoreAssignmentNode::OK(ok_assignment) => {
                let core_ok_assignment = ok_assignment.core_ref();
                let l_type = self.check_atom(&core_ok_assignment.l_atom);
                let r_assign = &core_ok_assignment.r_assign;
                (l_type, r_assign)
            }
            CoreAssignmentNode::INVALID_L_VALUE(invalid_l_value) => {
                let core_invalid_l_value = invalid_l_value.core_ref();
                let expr = &core_invalid_l_value.l_expr;
                let r_assign = &core_invalid_l_value.r_assign;
                let l_type = self.check_expr(expr);
                (l_type, r_assign)
            }
        };
        let r_type = self.check_r_assign(r_assign);
        if !l_type.is_eq(&r_type) {
            let err_message = format!(
                "mismatched types\nleft side has type `{}`, right side has type `{}`",
                l_type, 
                r_type
            );
            self.log_error(
                assignment.range(),
                assignment.start_line_number(),
                err_message
            )
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

    pub fn check_func_decl(&mut self, func_decl: &FunctionDeclarationNode) {
        if let CoreFunctionDeclarationNode::OK(ok_func_decl) = func_decl.core_ref() {
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
                let err_message = format!("function body has no `return` statement");
                self.log_error(
                    return_type_node.range(),
                    return_type_node.start_line_number(),
                    err_message
                );
            }
            self.close_scope();
            self.context.func_stack.pop();
        }
    }

    pub fn check_return_stmt(&mut self, return_stmt: &ReturnStatementNode) {
        let core_return_stmt = return_stmt.core_ref();
        let func_stack_len = self.context.func_stack.len();
        if func_stack_len == 0 {
            let err_message = format!("invalid `return` statement");
            self.log_error(
                return_stmt.range(),
                return_stmt.start_line_number(),
                err_message
            );
        }
        let expr = &core_return_stmt.expr;
        let expr_type_obj = self.check_expr(expr);
        let expected_type_obj = self.context.func_stack[func_stack_len - 1].clone();
        if !expr_type_obj.is_eq(&expected_type_obj) {
            let err_message = format!(
                "mismatched types\nexpected return value type `{}`, got `{}`",
                expected_type_obj, 
                expr_type_obj
            );
            self.log_error(
                expr.range(),
                expr.start_line_number(),
                err_message
            );
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
                self.check_func_decl(func_decl);
            }
            CoreStatementNode::RETURN(return_stmt) => {
                self.check_return_stmt(return_stmt);
            }
            _ => return,
        }
    }

    pub fn log_error(&mut self, error_range: TextRange, start_line_number: usize, err_message: String) {
        let start_err_index: usize = error_range.start().into();
        let end_err_index: usize = error_range.end().into();
        let err = JarvilError::form_error(
            start_err_index,
            end_err_index,
            start_line_number,
            &self.code,
            err_message,
            JarvilErrorKind::SEMANTIC_ERROR,
        );
        self.errors.push(err);
    }

    /*
    pub fn log_return_statement_not_inside_function_error(
        &mut self,
        error_range: TextRange,
        line_number: usize,
    ) {
        let start_err_index: usize = error_range.start().into();
        let end_err_index: usize = error_range.end().into();
        let err_str = format!("invalid `return` statement");
        let err = JarvilError::form_error(
            start_err_index,
            end_err_index,
            line_number,
            &self.code,
            err_str,
            JarvilErrorKind::SEMANTIC_ERROR,
        );
        self.errors.push(err);
    }

    pub fn log_expected_return_statement_error(
        &mut self,
        error_range: TextRange,
        line_number: usize,
    ) {
        let start_err_index: usize = error_range.start().into();
        let end_err_index: usize = error_range.end().into();
        let (code_line, line_start_index, line_number, start_err_index) =
            self.code.line_data(line_number, start_err_index);
        let err_str = format!("function body has no `return` statement");
        let err = JarvilError::form_single_line_error(
            start_err_index,
            end_err_index,
            line_number,
            line_start_index,
            code_line,
            err_str,
            JarvilErrorKind::SEMANTIC_ERROR,
        );
        self.errors.push(err);
    }

    pub fn log_mismatch_type_of_return_value_error(
        &mut self,
        error_range: TextRange,
        start_line_number: usize,
        expected_type: &Type,
        received_type: &Type,
    ) {
        let start_err_index: usize = error_range.start().into();
        let end_err_index: usize = error_range.end().into();
        let err_str = format!(
            "mismatched types\nexpected return value type `{}`, got `{}`",
            expected_type, received_type
        );
        let err = JarvilError::form_error(
            start_err_index,
            end_err_index,
            start_line_number,
            &self.code,
            err_str,
            JarvilErrorKind::SEMANTIC_ERROR,
        );
        self.errors.push(err);
    }

    pub fn log_mismatch_type_of_left_and_right_side_error(
        &mut self,
        error_range: TextRange,
        start_line_number: usize,
        expected_type: &Type,
        received_type: &Type,
    ) {
        let start_err_index: usize = error_range.start().into();
        let end_err_index: usize = error_range.end().into();
        let err_str = format!(
            "mismatched types\nleft side has type `{}`, right side has type `{}`",
            expected_type, received_type
        );
        let err = JarvilError::form_error(
            start_err_index,
            end_err_index,
            start_line_number,
            &self.code,
            err_str,
            JarvilErrorKind::SEMANTIC_ERROR,
        );
        self.errors.push(err);
    }

    pub fn log_invalid_type_of_unary_operand_error(
        &mut self,
        error_range: TextRange,
        start_line_number: usize,
        received_type: &Type,
        operator_kind: &UnaryOperatorKind,
    ) {
        let start_err_index: usize = error_range.start().into();
        let end_err_index: usize = error_range.end().into();
        let err_str = match operator_kind {
            UnaryOperatorKind::PLUS | UnaryOperatorKind::MINUS => {
                format!(
                    "unary expression with operator `+` or `-` is valid only for numeric (`int`, `float`) operand, got operand with type `{}`", received_type
                )
            }
            UnaryOperatorKind::NOT => {
                format!("unary expression with operator `not` is valid only for boolean operand, got operand with type `{}`", received_type)
            }
        };
        let err = JarvilError::form_error(
            start_err_index,
            end_err_index,
            start_line_number,
            &self.code,
            err_str,
            JarvilErrorKind::SEMANTIC_ERROR,
        );
        self.errors.push(err);
    }
     */
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
