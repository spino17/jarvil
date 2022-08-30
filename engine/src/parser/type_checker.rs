// See `https://www.csd.uwo.ca/~mmorenom/CS447/Lectures/TypeChecking.html/node1.html` for information about various cases that type-checker needs to 
// cover and the representation of type expressions in terms of type objects.

use crate::{scope::core::Namespace, code::Code, ast::{ast::{BlockNode, ASTNode, StatementNode, CoreStatementNode, ExpressionNode, AssignmentNode, 
    VariableDeclarationNode, FunctionDeclarationNode, TypeDeclarationNode, ReturnStatementNode}, walk::Visitor}, error::core::JarvilError};

pub struct TypeChecker {
    namespace: Namespace,
    code: Code,
    errors: Vec<JarvilError>,
}
impl TypeChecker {
    pub fn new(code: &Code, scope: &Namespace) -> Self {
        TypeChecker{
            namespace: scope.clone(),
            code: code.clone(),
            errors: vec![],
        }
    }

    pub fn open_scope(&mut self, block: &BlockNode) {
        self.namespace = block.scope().expect("scope should be set to the `BlockNode` in the resolver phase");
        // TODO - set the context also
    }

    pub fn close_scope(&mut self) {
        self.namespace.close_scope();
        // TODO - unset the context to previous one
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
        todo!()
    }

    pub fn check_return_stmt(&mut self, return_stmt: &ReturnStatementNode) {
        // TODO - check the type of return type according to the current context
        todo!()
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