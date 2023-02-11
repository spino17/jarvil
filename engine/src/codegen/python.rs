use crate::{
    ast::{
        ast::{
            ASTNode, AssignmentNode, CoreAssignmentNode, CoreFunctionDeclarationNode,
            CoreStatementNode, CoreTypeDeclarationNode, ExpressionNode, ExpressionStatementNode,
            FunctionDeclarationNode, LambdaDeclarationNode, OkAssignmentNode,
            OkFunctionDeclarationNode, ReturnStatementNode, StatementNode, StructDeclarationNode,
            StructStatementNode, TypeDeclarationNode, VariableDeclarationNode,
        },
        walk::Visitor,
    },
    types::core::CoreType,
};

pub struct PythonCodeGenerator {
    // internal state to track printing of the python code
    indent_level: u64,
}

impl PythonCodeGenerator {
    pub fn open_block(&mut self) {
        self.indent_level = self.indent_level + 1;
    }

    pub fn close_block(&mut self) {
        self.indent_level = self.indent_level - 1;
    }

    pub fn print_expr(&mut self, expr: &ExpressionNode) {
        todo!()
    }

    pub fn print_assign_stmt(&mut self, assign_stmt: &OkAssignmentNode) {
        todo!()
    }

    pub fn print_variable_decl_stmt(&mut self, variable_decl_stmt: &VariableDeclarationNode) {
        todo!()
    }

    pub fn print_return_stmt(&mut self, return_stmt: &ReturnStatementNode) {
        todo!()
    }

    pub fn print_func_decl(&mut self, func_decl: &OkFunctionDeclarationNode) {
        todo!()
    }

    pub fn print_struct_decl_stmt(&mut self, type_decl: &StructDeclarationNode) {
        todo!()
    }

    pub fn print_lambda_decl_stmt(&mut self, lambda_decl_stmt: &LambdaDeclarationNode) {
        todo!()
    }

    pub fn print_struct_stmt(&mut self, struct_stmt: &StructStatementNode) {
        todo!()
    }

    pub fn print_stmt(&mut self, stmt: &StatementNode) {
        match stmt.core_ref() {
            CoreStatementNode::EXPRESSION(expr_stmt) => {
                let core_expr = &expr_stmt.core_ref().expr;
                self.print_expr(core_expr);
            }
            CoreStatementNode::ASSIGNMENT(assign_stmt) => {
                let core_assign_stmt = assign_stmt.core_ref();
                match core_assign_stmt {
                    CoreAssignmentNode::OK(ok_assign_stmt) => {
                        self.print_assign_stmt(ok_assign_stmt)
                    }
                    CoreAssignmentNode::INVALID_L_VALUE(_) => unreachable!(),
                }
            }
            CoreStatementNode::VARIABLE_DECLARATION(variable_decl_stmt) => {
                self.print_variable_decl_stmt(variable_decl_stmt);
            }
            CoreStatementNode::RETURN(return_stmt) => {
                self.print_return_stmt(return_stmt);
            }
            CoreStatementNode::FUNCTION_DECLARATION(func_decl_stmt) => {
                let core_func_decl = func_decl_stmt.core_ref();
                match core_func_decl {
                    CoreFunctionDeclarationNode::OK(ok_func_decl) => {
                        self.print_func_decl(ok_func_decl)
                    }
                    CoreFunctionDeclarationNode::MISSING_TOKENS(_) => unreachable!(),
                }
            }
            CoreStatementNode::TYPE_DECLARATION(type_decl_stmt) => {
                let core_type_decl_stmt = type_decl_stmt.core_ref();
                match core_type_decl_stmt {
                    CoreTypeDeclarationNode::STRUCT(struct_decl_stmt) => {
                        self.print_struct_decl_stmt(struct_decl_stmt)
                    }
                    CoreTypeDeclarationNode::LAMBDA(lambda_decl_stmt) => {
                        self.print_lambda_decl_stmt(lambda_decl_stmt)
                    }
                    CoreTypeDeclarationNode::MISSING_TOKENS(_) => unreachable!(),
                }
            }
            CoreStatementNode::STRUCT_STATEMENT(struct_stmt) => {
                self.print_struct_stmt(struct_stmt);
            }
            CoreStatementNode::MISSING_TOKENS(_) => unreachable!(),
        }
    }
}

impl Visitor for PythonCodeGenerator {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match node {
            ASTNode::BLOCK(block) => {
                self.open_block();
                let core_block = block.0.as_ref().borrow();
                for stmt in &core_block.stmts {
                    self.walk_stmt_indent_wrapper(stmt);
                }
                self.close_block();
                return None;
            }
            ASTNode::STATEMENT(stmt) => {
                self.print_stmt(stmt);
                return None;
            }
            _ => Some(()),
        }
    }
}
