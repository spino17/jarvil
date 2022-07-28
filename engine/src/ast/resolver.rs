use crate::types::core::Type;
use crate::{scope::core::Scope, code::Code};
use crate::ast::ast::BlockNode;
use std::rc::Rc;

use super::ast::{StatemenIndentWrapper, StatementNode, StatementNodeKind, FunctionDeclarationNode, 
    VariableDeclarationNode, TypeDeclarationNode, FunctionDeclarationKind};

pub struct Resolver {
    scope: Scope,
    code: Code,
}

impl Resolver {
    pub fn new(code: &Code) -> Self {
        Resolver{
            scope: Scope::new(),
            code: code.clone(),
        }
    }

    pub fn curr_scope(&self) -> Scope {
        self.scope.clone()
    }

    pub fn set_scope(&mut self, scope: &Scope) {
        self.scope = scope.clone();
    }

    pub fn set_new_nested_scope(&mut self) {
        let scope = Scope::new_with_parent_scope(&self.scope);
        self.set_scope(&scope);
    }

    pub fn set_identifier_to_scope(&mut self, name: &Rc<String>, type_obj: &Type) {
        todo!()
    }

    pub fn set_params_to_scope(&mut self, args: Rc<Vec<(Option<Rc<String>>, Option<Type>)>>) {
        for arg in args.as_ref() {
            let param_name = match &arg.0 {
                Some(param_name) => param_name,
                None => continue,
            };
            let param_type = match &arg.1 {
                Some(param_type) => param_type,
                None => continue,
            };
            self.set_identifier_to_scope(param_name, param_type);
        }
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
        block.set_scope(&self.curr_scope());
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
                let args_objs = match args {
                    Some(args) => {
                        let args_objs = args.get_name_type_spec_objs(&self.code);
                        if args_objs.len() > 0 {
                            Some(Rc::new(args_objs))
                        } else {
                            None
                        }
                    },
                    None => None,
                };
                match func_name {
                    Some(func_name) => {
                        let func_name = func_name.get_ok();
                        // TODO - if None then return from match
                        let return_type = match return_type {
                            Some(return_type) => return_type.get_type_obj(&self.code),
                            None => None
                        };
                        // TODO - add function name, args and return type to the scope
                        // All OK values required
                    },
                    None => {},
                };
                let saved_scope = self.curr_scope();
                self.set_new_nested_scope();
                match args_objs {
                    Some(args) => self.set_params_to_scope(args),
                    None => {},
                }
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