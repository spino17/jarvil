use rustc_hash::FxHashSet;

use crate::{
    ast::{
        ast::{
            ASTNode, BlockNode, BoundedMethodKind, BoundedMethodWrapperNode, CallablePrototypeNode,
            ClassMethodCallNode, CoreAssignmentNode, CoreCallableBodyNode,
            CoreFunctionDeclarationNode, CoreIdentifierNode, CoreLambdaDeclarationNode,
            CoreRVariableDeclarationNode, CoreStatemenIndentWrapperNode, CoreStatementNode,
            CoreTokenNode, CoreTypeDeclarationNode, ExpressionStatementNode,
            FunctionDeclarationNode, IdentifierNode, LambdaTypeDeclarationNode, OkAssignmentNode,
            OkIdentifierNode, ReturnStatementNode, StatementNode, StructDeclarationNode,
            StructPropertyDeclarationNode, TokenNode, TypeDeclarationNode, VariableDeclarationNode,
        },
        walk::Visitor,
    },
    code::Code,
    context,
    lexer::token::{CoreToken, Token},
    scope::core::IdentifierKind,
};
use std::{cell::RefCell, convert::TryInto, rc::Rc};

// Utility functions
pub fn get_whitespaces_from_indent_level(indent_level: usize) -> String {
    let expected_indent_spaces = context::indent_spaces() * indent_level;
    return " "
        .to_string()
        .repeat(expected_indent_spaces.try_into().unwrap());
}

pub fn get_newline() -> &'static str {
    return "\n";
}

pub fn get_trivia_from_token_node(token: &TokenNode) -> Option<Rc<Vec<Token>>> {
    match token.core_ref() {
        CoreTokenNode::OK(ok_token_node) => match &ok_token_node.core_ref().token.trivia {
            Some(trivia) => return Some(trivia.clone()),
            None => return None,
        },
        _ => unreachable!(),
    }
}

pub fn get_suffix_str_for_identifier(identifier: &OkIdentifierNode) -> &'static str {
    let suffix_str = match &identifier.0.as_ref().borrow().decl {
        Some((ident_kind, _)) => match ident_kind {
            IdentifierKind::VARIABLE(_) => "_var",
            IdentifierKind::FUNCTION(_) => "_func",
            IdentifierKind::USER_DEFINED_TYPE(_) => "_ty",
        },
        None => "",
    };
    suffix_str
}

pub struct PythonCodeGenerator {
    indent_level: usize,
    generate_code: String,
    code: Code,
}

impl PythonCodeGenerator {
    pub fn new(code: &Code) -> PythonCodeGenerator {
        PythonCodeGenerator {
            indent_level: 0,
            generate_code: "".to_string(),
            code: code.clone(),
        }
    }

    pub fn open_block(&mut self) {
        self.indent_level = self.indent_level + 1;
    }

    pub fn close_block(&mut self) {
        self.indent_level = self.indent_level - 1;
    }

    pub fn generate_python_code(mut self, ast: &BlockNode) -> String {
        let code_block = ast.0.as_ref().borrow();
        self.add_str_to_python_code("def main():\n");
        for stmt in &code_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        self.add_str_to_python_code("\n\nif __name__ == \"__main__\":\n    main()");
        self.generate_code
    }

    pub fn add_str_to_python_code(&mut self, str: &str) {
        self.generate_code.push_str(str);
    }

    pub fn get_non_locals(
        &self,
        block: &BlockNode,
    ) -> (
        Rc<RefCell<FxHashSet<Rc<String>>>>,
        Rc<RefCell<FxHashSet<Rc<String>>>>,
    ) {
        let block_scope = match &block.0.as_ref().borrow().scope {
            Some(scope) => scope.clone(),
            None => unreachable!(),
        };
        let variable_non_locals = block_scope.variables.0.as_ref().borrow().get_non_locals();
        let func_non_locals = block_scope.functions.0.as_ref().borrow().get_non_locals();
        (variable_non_locals, func_non_locals)
    }

    pub fn print_token(&mut self, token: &Token) {
        let trivia = &token.trivia;
        self.print_trivia(trivia);
        let token_value = token.token_value(&self.code);
        match token.core_token {
            CoreToken::SINGLE_LINE_COMMENT => {
                if token_value.chars().next().unwrap() == '/' {
                    let mut modified_str = "#".to_string();
                    modified_str.push_str(&token_value[2..]);
                    self.add_str_to_python_code(&modified_str);
                } else {
                    self.add_str_to_python_code(&token_value);
                }
            }
            CoreToken::BLOCK_COMMENT => {
                let len = token_value.len();
                let mut critical_section = token_value[2..(len - 2)].to_string();
                critical_section.push_str("\"\"\"");
                let mut final_str = "\"\"\"".to_string();
                final_str.push_str(&critical_section);
                self.add_str_to_python_code(&final_str);
            }
            _ => {
                self.add_str_to_python_code(&token_value);
            }
        }
    }

    pub fn print_trivia(&mut self, trivia: &Option<Rc<Vec<Token>>>) {
        if let Some(trivia) = trivia {
            for trivia_entry in trivia.as_ref() {
                self.print_token(trivia_entry);
            }
        }
    }

    pub fn print_token_node(&mut self, token: &TokenNode) {
        match token.core_ref() {
            CoreTokenNode::OK(ok_token_node) => {
                self.walk_ok_token(&ok_token_node);
            }
            CoreTokenNode::MISSING_TOKENS(_) => unreachable!(),
        }
    }

    pub fn print_identifier(&mut self, identifier: &IdentifierNode) {
        let identifier = match identifier.core_ref() {
            CoreIdentifierNode::OK(ok_identifier) => ok_identifier,
            _ => unreachable!(),
        };
        let suffix_str = get_suffix_str_for_identifier(identifier);
        let mut token_value = identifier.token_value(&self.code);
        token_value.push_str(suffix_str);
        let token = identifier
            .0
            .as_ref()
            .borrow()
            .token
            .core_ref()
            .token
            .clone();
        let trivia = &token.trivia;
        self.print_trivia(trivia);
        self.add_str_to_python_code(&token_value);
    }

    pub fn print_identifier_without_trivia(&mut self, identifier: &IdentifierNode) {
        let identifier = match identifier.core_ref() {
            CoreIdentifierNode::OK(ok_identifier) => ok_identifier,
            _ => unreachable!(),
        };
        let suffix_str = get_suffix_str_for_identifier(identifier);
        let mut token_value = identifier.token_value(&self.code);
        token_value.push_str(suffix_str);
        self.add_str_to_python_code(&token_value);
    }

    pub fn print_variable_decl(&mut self, variable_decl: &VariableDeclarationNode) {
        let core_variable_decl = variable_decl.core_ref();
        let let_keyword = &core_variable_decl.let_keyword;
        let name = &core_variable_decl.name;
        let equal = &core_variable_decl.equal;
        let r_node = &core_variable_decl.r_node;
        let trivia = get_trivia_from_token_node(let_keyword);
        self.print_trivia(&trivia);
        match r_node.core_ref() {
            CoreRVariableDeclarationNode::EXPRESSION(expr_stmt) => {
                self.print_identifier_without_trivia(name);
                self.print_token_node(equal);
                self.walk_expr_stmt(expr_stmt);
            }
            CoreRVariableDeclarationNode::LAMBDA(lambda_decl) => {
                let callable_body = &lambda_decl.core_ref().body;
                self.add_str_to_python_code("def");
                self.print_identifier(name);
                self.walk_callable_body(callable_body);
            }
            CoreRVariableDeclarationNode::MISSING_TOKENS(_) => unreachable!(),
        }
    }

    pub fn print_callable_prototype(&mut self, callable_prototype: &CallablePrototypeNode) {
        let core_callable_prototype = callable_prototype.core_ref();
        let lparen = &core_callable_prototype.lparen;
        let params = &core_callable_prototype.params;
        let rparen = &core_callable_prototype.rparen;
        self.print_token_node(lparen);
        if let Some(params) = params {
            self.walk_name_type_specs(params);
        }
        self.print_token_node(rparen);
    }

    pub fn print_type_decl(&mut self, type_decl: &TypeDeclarationNode) {
        let core_type_decl = type_decl.core_ref();
        match core_type_decl {
            CoreTypeDeclarationNode::STRUCT(struct_decl) => {
                let core_struct_decl = struct_decl.core_ref();
                let struct_name = &core_struct_decl.name;
                let type_keyword = &core_struct_decl.type_keyword;
                let colon = &core_struct_decl.colon;
                let block = &core_struct_decl.block;
                let trivia = get_trivia_from_token_node(type_keyword);
                self.print_trivia(&trivia);
                self.add_str_to_python_code("class");
                self.print_identifier(struct_name);
                self.print_token_node(colon);
                self.walk_block(block);
            }
            CoreTypeDeclarationNode::LAMBDA(_) => {
                self.add_str_to_python_code("\n");
            }
            CoreTypeDeclarationNode::MISSING_TOKENS(_) => unreachable!(),
        }
    }

    pub fn print_class_method_call(&mut self, class_method_call: &ClassMethodCallNode) {
        let core_class_method_call = class_method_call.core_ref();
        let lparen = &core_class_method_call.lparen;
        let rparen = &core_class_method_call.rparen;
        let class_name = &core_class_method_call.class_name;
        let class_method_name = &core_class_method_call.class_method_name;
        let params = &core_class_method_call.params;
        self.print_identifier(class_name);
        self.add_str_to_python_code(".");
        self.print_identifier(class_method_name);
        self.print_token_node(lparen);
        if let Some(params) = params {
            self.walk_params(params);
        }
        self.print_token_node(rparen);
    }

    pub fn print_bounded_method_wrapper(
        &mut self,
        bounded_method_wrapper: &BoundedMethodWrapperNode,
    ) {
        let bounded_kind = match &bounded_method_wrapper.0.as_ref().borrow().bounded_kind {
            Some(bounded_kind) => bounded_kind.clone(),
            None => unreachable!(),
        };
        match bounded_kind {
            BoundedMethodKind::CLASS_METHOD => {
                self.walk_func_decl(&bounded_method_wrapper.0.as_ref().borrow().func_decl);
                return;
            }
            BoundedMethodKind::METHOD | BoundedMethodKind::CONSTRUCTOR => {
                let core_func_decl = &bounded_method_wrapper
                    .0
                    .as_ref()
                    .borrow()
                    .func_decl
                    .core_ref()
                    .clone();
                let def_keyword = &core_func_decl.def_keyword;
                let name = &core_func_decl.name;
                let body = match core_func_decl.body.core_ref() {
                    CoreCallableBodyNode::OK(ok_callable_body) => ok_callable_body.core_ref(),
                    _ => unreachable!(),
                };
                let colon = &body.colon;
                let block = &body.block;
                let prototype = body.prototype.core_ref();
                let lparen = &prototype.lparen;
                let rparen = &prototype.rparen;
                let params = &prototype.params;

                self.print_token_node(def_keyword);
                self.print_identifier(name);
                self.print_token_node(lparen);
                self.add_str_to_python_code("self");
                if let Some(params) = params {
                    self.add_str_to_python_code(", ");
                    self.walk_name_type_specs(params);
                }
                self.print_token_node(rparen);
                self.print_token_node(colon);
                self.walk_block(block);
            }
        };
    }
}

impl Visitor for PythonCodeGenerator {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match node {
            ASTNode::BLOCK(block) => {
                // TODO - add nonlocal statement by analyzing from scope
                self.open_block();
                let core_block = block.0.as_ref().borrow();
                self.print_token_node(&core_block.newline);
                let (variable_non_locals, func_non_locals) = self.get_non_locals(block);
                for entry in variable_non_locals.as_ref().borrow().iter() {
                    let mut variable_name = entry.to_string();
                    variable_name.push_str("_var");
                    self.add_str_to_python_code(&get_whitespaces_from_indent_level(
                        self.indent_level + 1,
                    ));
                    self.add_str_to_python_code(&format!("nonlocal {}\n", variable_name));
                }
                for entry in func_non_locals.as_ref().borrow().iter() {
                    let mut func_name = entry.to_string();
                    func_name.push_str("_func");
                    self.add_str_to_python_code(&get_whitespaces_from_indent_level(
                        self.indent_level + 1,
                    ));
                    self.add_str_to_python_code(&format!("nonlocal {}\n", func_name));
                }
                for stmt in &core_block.stmts {
                    self.walk_stmt_indent_wrapper(stmt);
                }
                self.close_block();
                return None;
            }
            ASTNode::STATEMENT_INDENT_WRAPPER(stmt_wrapper) => {
                let core_stmt_wrapper = stmt_wrapper.core_ref();
                match core_stmt_wrapper {
                    CoreStatemenIndentWrapperNode::CORRECTLY_INDENTED(ok_stmt) => {
                        self.add_str_to_python_code(&get_whitespaces_from_indent_level(1));
                        self.walk_stmt(ok_stmt);
                    }
                    CoreStatemenIndentWrapperNode::EXTRA_NEWLINES(extra_newlines) => {
                        let core_extra_newlines = extra_newlines.core_ref();
                        for extra_newline in &core_extra_newlines.skipped_tokens {
                            let core_token = &extra_newline.core_ref().skipped_token;
                            self.add_str_to_python_code(&get_whitespaces_from_indent_level(1));
                            self.print_token(core_token);
                        }
                    }
                    CoreStatemenIndentWrapperNode::INCORRECTLY_INDENTED(_) => unreachable!(),
                    CoreStatemenIndentWrapperNode::LEADING_SKIPPED_TOKENS(_) => unreachable!(),
                    CoreStatemenIndentWrapperNode::TRAILING_SKIPPED_TOKENS(_) => unreachable!(),
                }
                return None;
            }
            ASTNode::VARIABLE_DECLARATION(variable_decl) => {
                self.print_variable_decl(variable_decl);
                return None;
            }
            ASTNode::STRUCT_PROPERTY_DECLARATION(_) => {
                self.add_str_to_python_code("\n");
                return None;
            }
            ASTNode::BOUNDED_METHOD_WRAPPER(bounded_method_wrapper) => {
                self.print_bounded_method_wrapper(bounded_method_wrapper);
                return None;
            }
            ASTNode::CALLABLE_PROTOTYPE(callable_prototype) => {
                self.print_callable_prototype(callable_prototype);
                return None;
            }
            ASTNode::NAME_TYPE_SPEC(name_type_spec) => {
                let core_name_type_spec = name_type_spec.core_ref();
                let name = &core_name_type_spec.name;
                self.print_identifier(name);
                return None;
            }
            ASTNode::TYPE_DECLARATION(type_decl) => {
                self.print_type_decl(type_decl);
                return None;
            }
            ASTNode::CLASS_METHOD_CALL(class_method_call) => {
                self.print_class_method_call(class_method_call);
                return None;
            }
            ASTNode::IDENTIFIER(identifier) => {
                self.print_identifier(identifier);
                return None;
            }
            ASTNode::TOKEN(token) => {
                self.print_token_node(token);
                return None;
            }
            ASTNode::OK_TOKEN(token) => {
                self.print_token(&token.core_ref().token);
                return None;
            }
            _ => Some(()),
        }
    }
}
