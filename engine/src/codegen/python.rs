use std::{borrow::Borrow, convert::TryInto};

use crate::{
    ast::{
        ast::{
            ASTNode, BlockNode, CoreAssignmentNode, CoreFunctionDeclarationNode,
            CoreStatemenIndentWrapperNode, CoreStatementNode, CoreTokenNode,
            CoreTypeDeclarationNode, ExpressionStatementNode, FunctionDeclarationNode,
            LambdaTypeDeclarationNode, OkAssignmentNode, ReturnStatementNode, StatementNode,
            StructDeclarationNode, StructPropertyDeclarationNode, TokenNode,
            VariableDeclarationNode, OkIdentifierNode,
        },
        walk::Visitor,
    },
    code::Code,
    context,
    lexer::token::Token, scope::core::IdentifierKind,
};

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

pub struct PythonCodeGenerator {
    generate_code: String,
    code: Code,
}

impl PythonCodeGenerator {
    pub fn new(code: &Code) -> PythonCodeGenerator {
        PythonCodeGenerator {
            generate_code: "".to_string(),
            code: code.clone(),
        }
    }

    pub fn generate_python_code(mut self, ast: &BlockNode) -> String {
        let code_block = ast.0.as_ref().borrow();
        for stmt in &code_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        self.generate_code
    }

    pub fn add_str_to_python_code(&mut self, str: &str) {
        self.generate_code.push_str(str);
    }

    pub fn print_token(&mut self, token: &Token) {
        let trivia = &token.trivia;
        if let Some(trivia) = trivia {
            for trivia_entry in trivia.as_ref() {
                self.print_token(trivia_entry);
            }
        }
        self.add_str_to_python_code(&token.token_value(&self.code));
    }

    pub fn print_token_node(&mut self, token: &TokenNode) {
        match token.core_ref() {
            CoreTokenNode::OK(ok_token_node) => {
                self.walk_ok_token(&ok_token_node);
            }
            CoreTokenNode::MISSING_TOKENS(_) => unreachable!(),
        }
    }

    pub fn print_identifier(&mut self, identifier: &OkIdentifierNode) {
        let suffix_str = match &identifier.0.as_ref().borrow().decl {
            Some((ident_kind, _)) => {
                match ident_kind {
                    IdentifierKind::VARIABLE(_) => {
                        "_var"
                    }
                    IdentifierKind::FUNCTION(_) => {
                        "_func"
                    }
                    IdentifierKind::USER_DEFINED_TYPE(_) => {
                        "_ty"
                    }
                }
            }
            None => ""
        };
        let mut token_value = identifier.token_value(&self.code);
        token_value.push_str(suffix_str);
        let token = identifier.0.as_ref().borrow().token.core_ref().token.clone();
        let trivia = &token.trivia;
        if let Some(trivia) = trivia {
            for trivia_entry in trivia.as_ref() {
                self.print_token(trivia_entry);
            }
        }
        self.add_str_to_python_code(&token_value);
    }
}

impl Visitor for PythonCodeGenerator {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        match node {
            ASTNode::STATEMENT_INDENT_WRAPPER(stmt_wrapper) => {
                let core_stmt_wrapper = stmt_wrapper.core_ref();
                match core_stmt_wrapper {
                    CoreStatemenIndentWrapperNode::CORRECTLY_INDENTED(ok_stmt) => {
                        self.walk_stmt(ok_stmt);
                    }
                    CoreStatemenIndentWrapperNode::EXTRA_NEWLINES(extra_newlines) => {
                        let core_extra_newlines = extra_newlines.core_ref();
                        for extra_newline in &core_extra_newlines.skipped_tokens {
                            let core_token = &extra_newline.core_ref().skipped_token;
                            self.print_token(core_token);
                        }
                    }
                    CoreStatemenIndentWrapperNode::INCORRECTLY_INDENTED(_) => unreachable!(),
                    CoreStatemenIndentWrapperNode::LEADING_SKIPPED_TOKENS(_) => unreachable!(),
                    CoreStatemenIndentWrapperNode::TRAILING_SKIPPED_TOKENS(_) => unreachable!(),
                }
                return None;
            }
            ASTNode::OK_IDENTIFIER(ok_identifier) => {
                self.print_identifier(ok_identifier);
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
