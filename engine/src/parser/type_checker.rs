// See `https://www.csd.uwo.ca/~mmorenom/CS447/Lectures/TypeChecking.html/node1.html` for information about various cases that type-checker needs to
// cover and the representation of type expressions in terms of type objects.

use crate::{
    ast::{
        ast::{
            ASTNode, AssignmentNode, BlockKind, BlockNode, CoreFunctionDeclarationNode,
            CoreIdentifierNode, CoreStatemenIndentWrapperNode, CoreStatementNode, ExpressionNode,
            FunctionDeclarationNode, Node, OkFunctionDeclarationNode, ReturnStatementNode,
            StatementNode, TypeDeclarationNode, TypeExpressionNode, TypeResolveKind,
            VariableDeclarationNode,
        },
        walk::Visitor,
    },
    code::Code,
    error::core::{JarvilError, JarvilErrorKind},
    scope::{
        core::{IdentifierKind, Namespace, SymbolData},
        user_defined_types::{LambdaTypeData, UserDefinedTypeData},
    },
    types::core::{AbstractType, Type},
};
use text_size::TextRange;

struct Context {
    func_stack: Vec<Type>,
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
        self.namespace = block
            .scope()
            .expect("scope should be set to the `BlockNode` in the resolver phase");
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

    pub fn type_of_lambda(&self, func_decl: &OkFunctionDeclarationNode) -> Type {
        let core_func_decl = func_decl.core_ref();
        let func_name = &core_func_decl.name;
        assert!(
            func_name.is_none(),
            "construction of type for lambda is valid only for unnamed function declarations"
        );
        let params = &core_func_decl.params;
        let return_type = &core_func_decl.return_type;
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
                    let variable_name = ok_identifier.token_value(&self.code);
                    let type_obj = self.type_obj_from_expression(&core_param.data_type);
                    if let Some(_) = ok_identifier.symbol_data() {
                        params_vec.push((variable_name, type_obj));
                    }
                }
            }
        }
        let symbol_data = UserDefinedTypeData::LAMBDA(LambdaTypeData::new(params_vec, return_type));
        let lambda_type_obj = Type::new_with_lambda(
            None,
            &SymbolData::new(symbol_data, core_func_decl.lparen.start_line_number()),
        );
        lambda_type_obj
    }

    pub fn check_ast(&mut self, ast: &BlockNode) -> Vec<JarvilError> {
        let core_block = ast.0.as_ref().borrow();
        for stmt in &core_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        std::mem::take(&mut self.errors)
    }

    pub fn check_expr(&mut self, expr: &ExpressionNode) -> Type {
        // TODO - check the binary and unary operands and atom chaining
        // 1. operators, operands validity
        // 2. call expr should have correct number and type of params
        // 3. atom chaining should be valid according to types
        todo!()
    }

    pub fn check_assignment(&mut self, assignment: &AssignmentNode) {
        // TODO - check types on both sides are same
        todo!()
    }

    pub fn check_variable_decl(&mut self, variable_decl: &VariableDeclarationNode) {
        // TODO - get the type of left side and set the data_type in scope table entry
        todo!()
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
                self.log_expected_return_statement_error(
                    return_type_node.range(),
                    return_type_node.start_line_number(),
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
            self.log_return_statement_not_inside_function_error(
                return_stmt.range(),
                return_stmt.start_line_number(),
            );
        }
        let expr = &core_return_stmt.expr;
        let expr_type_obj = self.check_expr(expr);
        let expected_type_obj = self.context.func_stack[func_stack_len - 1].clone();
        if !expr_type_obj.is_eq(&expected_type_obj) {
            self.log_mismatch_type_of_return_value_error(
                expr.range(),
                expr.start_line_number(),
                &expr_type_obj,
                &expected_type_obj,
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
