// See `https://www.csd.uwo.ca/~mmorenom/CS447/Lectures/TypeChecking.html/node1.html` for information about various cases that type-checker needs to 
// cover and the representation of type expressions in terms of type objects.

use text_size::TextRange;

use crate::{scope::core::Namespace, code::Code, ast::{ast::{BlockNode, ASTNode, StatementNode, CoreStatementNode, ExpressionNode, AssignmentNode, 
    VariableDeclarationNode, FunctionDeclarationNode, TypeDeclarationNode, ReturnStatementNode, BlockKind, CoreFunctionDeclarationNode, TypeResolveKind, 
    CoreStatemenIndentWrapperNode, Node}, walk::Visitor}, error::core::{JarvilError, JarvilErrorKind}, types::core::Type};
use std::rc::Rc;

struct Context {
    func_stack: Vec<Option<Type>>,
}

pub struct TypeChecker {
    namespace: Namespace,
    code: Code,
    errors: Vec<JarvilError>,
    context: Context,
}
impl TypeChecker {
    pub fn new(code: &Code, scope: &Namespace) -> Self {
        TypeChecker{
            namespace: scope.clone(),
            code: code.clone(),
            errors: vec![],
            context: Context {
                func_stack: vec![],
            }
        }
    }

    pub fn open_scope(&mut self, block: &BlockNode) {
        self.namespace = block.scope().expect("scope should be set to the `BlockNode` in the resolver phase");
    }

    pub fn close_scope(&mut self) {
        self.namespace.close_scope();
    }

    pub fn check_ast(&mut self, ast: &BlockNode) -> Vec<JarvilError> {
        let core_block = ast.0.as_ref().borrow();
        for stmt in &core_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        std::mem::take(&mut self.errors)
    }

    pub fn check_expr(&mut self, expr: &ExpressionNode) {
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
        // TODO - set the new entry in the context for checking return type
        if let CoreFunctionDeclarationNode::OK(ok_func_decl) = func_decl.core_ref() {
            let core_ok_func_decl = ok_func_decl.core_ref();
            let return_type_node = &core_ok_func_decl.return_type;
            let return_type_obj = match return_type_node {
                Some(return_type_expr) => {
                    match return_type_expr.type_obj(&self.namespace, &self.code) {
                        TypeResolveKind::RESOLVED(type_obj) => Some(type_obj),
                        TypeResolveKind::UNRESOLVED(_) => Some(Type::new_with_unknown()),
                        TypeResolveKind::INVALID => Some(Type::new_with_unknown()),
                    }
                },
                None => None
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
                    },
                    _ => continue
                };
                self.walk_stmt(&stmt);
                if let CoreStatementNode::RETURN(_) = stmt.core_ref() {
                    has_return_stmt = true;
                    // TODO - we can break here as any statement following return statement is dead code
                }
            }
            if !has_return_stmt && return_type_obj.is_some() {
                let return_type_node = return_type_node.as_ref().unwrap();
                self.log_expected_return_statement_error(
                    return_type_node.range(), return_type_node.start_line_number()
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
                core_return_stmt.return_keyword.range(), 
                core_return_stmt.return_keyword.start_line_number()
            );
        }
        // TODO - get the type of the return expr and compare it with self.context.func_stack[func_stack_len - 1] entry
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
            _ => return
        }
    }

    pub fn log_return_statement_not_inside_function_error(&mut self, error_range: TextRange, line_number: usize) {
        let start_err_index: usize = error_range.start().into();
        let end_err_index: usize = error_range.end().into();
        let (code_line, line_start_index, line_number, start_err_index) =
            self.code.line_data(line_number, start_err_index);
        let err_str = format!("invalid `return` statement");
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

    pub fn log_expected_return_statement_error(&mut self, error_range: TextRange, line_number: usize) {
        let start_err_index: usize = error_range.start().into();
        let end_err_index: usize = error_range.end().into();
        let (code_line, line_start_index, line_number, start_err_index) =
            self.code.line_data(line_number, start_err_index);
        let err_str = format!("function body has `return` statement");
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
}
impl Visitor for TypeChecker {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match node {
            ASTNode::STATEMENT(stmt) => {
                self.check_stmt(stmt);
                return None
            },
            _ => Some(())
        }
    }
}