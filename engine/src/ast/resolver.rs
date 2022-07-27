use crate::scope::core::Scope;
use crate::ast::ast::BlockNode;

use super::ast::{StatemenIndentWrapper, StatementNode, StatementNodeKind, FunctionDeclarationNode, VariableDeclarationNode, TypeDeclarationNode, FunctionDeclarationKind};

pub struct Resolver {
    scope: Scope,
}

impl Resolver {
    pub fn new() -> Self {
        Resolver{
            scope: Scope::new(),
        }
    }

    pub fn curr_scope(&self) -> Scope {
        self.scope.clone()
    }

    pub fn set_scope(&mut self, scope: &Scope) {
        self.scope = scope.clone();
    }

    pub fn resolve(&mut self, ast: &BlockNode) {
        self.resolve_block(&ast);
    }

    pub fn resolve_block(&mut self, block: &BlockNode) {
        for stmt in &*block.0.as_ref().borrow().stmts.as_ref().borrow() {
            match stmt {
                StatemenIndentWrapper::CORRECTLY_INDENTED(stmt_node) => self.resolve_statement(stmt_node),
                StatemenIndentWrapper::INCORRECTLY_INDENTED((stmt_node, _)) => self.resolve_statement(stmt_node),
                _ => {},
            }
        }
    }

    pub fn resolve_statement(&mut self, stmt: &StatementNode) {
        match &stmt.0.as_ref().borrow().kind {
            StatementNodeKind::FUNCTION_DECLARATION(func_decl_node)     => self.resolve_func_decl(
                func_decl_node
            ),
            StatementNodeKind::VARIABLE_DECLARATION(variable_decl_node) => self.resolve_variable_decl(
                variable_decl_node
            ),
            StatementNodeKind::TYPE_DECLARATION(type_decl_node)             => self.resolve_type_decl(
                type_decl_node
            ),
            _ => {}
        }
    }

    pub fn resolve_func_decl(&mut self, func_decl: &FunctionDeclarationNode) {
        match &func_decl.0.as_ref().borrow().kind {
            FunctionDeclarationKind::OK(func_decl) => {
                let core_func_decl = &func_decl.0.as_ref().borrow();
                let func_name = &core_func_decl.name;
                let args = &core_func_decl.args;
                let return_type = &core_func_decl.return_type;
                let block = &core_func_decl.block;
                match func_name {
                    Some(func_name) => {
                        // TODO - add function name, args and return type to the scope
                    },
                    None => {},
                }
                let saved_scope = self.curr_scope();
                let scope = Scope::new_with_parent_scope(&saved_scope);
                // TODO - add args to this scope
                self.set_scope(&scope);
                self.resolve_block(&block);
                self.set_scope(&saved_scope);
            },
            _ => return,
        };
    }

    pub fn resolve_variable_decl(&mut self, variable_decl: &VariableDeclarationNode) {
        todo!()
    }

    pub fn resolve_type_decl(&mut self, type_decl: &TypeDeclarationNode) {
        todo!()
    }
}