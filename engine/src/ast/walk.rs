use crate::ast::ast::ASTNode;

use super::ast::{StatemenIndentWrapper, StatementKind};

pub trait Visitor {
    fn visit(&mut self, node: &ASTNode) -> Option<()>;  // if `None` then return else traverse the children
    fn walk(&mut self, node: ASTNode) {
        match self.visit(&node) {
            None => return,
            _ => {}
        }
        
        match &node {
            ASTNode::BLOCK(block_node) => {
                let core_block = block_node.core_ref();
                for stmt in &*core_block.stmts.as_ref().borrow() {
                    match stmt {
                        StatemenIndentWrapper::CORRECTLY_INDENTED(stmt_node) => self.walk(ASTNode::new_with_stmt(stmt_node)),
                        StatemenIndentWrapper::INCORRECTLY_INDENTED((stmt_node, _)) => {
                            self.walk(ASTNode::new_with_stmt(stmt_node));
                        },
                        StatemenIndentWrapper::LEADING_SKIPPED_TOKENS(skipped_tokens) => {
                            self.walk(ASTNode::new_with_skipped_tokens(skipped_tokens));
                        },
                        StatemenIndentWrapper::TRAILING_SKIPPED_TOKENS(skipped_tokens) => {
                            self.walk(ASTNode::new_with_skipped_tokens(skipped_tokens));
                        },
                        StatemenIndentWrapper::EXTRA_NEWLINES(skipped_tokens) => {
                            self.walk(ASTNode::new_with_skipped_tokens(skipped_tokens));
                        },
                    }
                }
            },
            ASTNode::SKIPPED_TOKENS(skipped_tokens) => {
                let core_skipped_tokens = skipped_tokens.core_ref();
                for skipped_token in core_skipped_tokens.skipped_tokens.as_ref() {
                    self.walk(ASTNode::new_with_skipped_token(skipped_token));
                }
            },
            ASTNode::STATEMENT(statement_node) => {
                let core_stmt = statement_node.core_ref();
                match &core_stmt.kind {
                    StatementKind::EXPRESSION((expr_node, _)) => {
                        self.walk(ASTNode::new_with_expr(expr_node));
                    },
                    StatementKind::ASSIGNMENT(assignment_node) => {
                        self.walk(ASTNode::new_with_assignment(assignment_node));
                    },
                    StatementKind::VARIABLE_DECLARATION(variable_decl_node) => {
                        self.walk(ASTNode::new_with_variable_declaration(variable_decl_node));
                    },
                    StatementKind::FUNCTION_DECLARATION(func_decl_node) => {
                        self.walk(ASTNode::new_with_function_declaration(func_decl_node));
                    },
                    StatementKind::TYPE_DECLARATION(type_decl_node) => {
                        self.walk(ASTNode::new_with_type_declaration(type_decl_node));
                    },
                    StatementKind::STRUCT_STATEMENT(struct_stmt_node) => {
                        self.walk(ASTNode::new_with_struct_stmt(struct_stmt_node));
                    },
                    StatementKind::MISSING_TOKENS(missing_token_node) => {
                        self.walk(ASTNode::new_with_missing_token(missing_token_node));
                    },
                }
            },
            ASTNode::ASSIGNMENT(assignment_node) => {
                todo!()
            },
            ASTNode::STRUCT_STATEMENT(struct_statement_node) => {
                todo!()
            },
            ASTNode::TYPE_DECLARATION(type_declaration_node) => {
                todo!()
            },
            ASTNode::STRUCT_DECLARATION(struct_declaration_node) => {
                todo!()
            },
            ASTNode::LAMBDA_DECLARATION(lambda_declaration_node) => {
                todo!()
            },
            ASTNode::OK_LAMBDA_DECLARATION(ok_lambda_declaration_node) => {
                todo!()
            },
            ASTNode::FUNCTION_DECLARATION(function_declaration_node) => {
                todo!()
            },
            ASTNode::OK_FUNCTION_DECLARATION(ok_function_declaration_node) => {
                todo!()
            },
            ASTNode::VARIABLE_DECLARATION(variable_declaration_node) => {
                todo!()
            },
            ASTNode::R_ASSIGNMENT(r_assignment_node) => {
                todo!()
            },
            ASTNode::NAME_TYPE_SPECS(name_type_specs_node) => {
                todo!()
            },
            ASTNode::OK_NAME_TYPE_SPECS(ok_name_type_specs_node) => {
                todo!()
            },
            ASTNode::NAME_TYPE_SPEC(name_type_spec_node) => {
                todo!()
            },
            ASTNode::TYPE_EXPRESSION(type_expression_node) => {
                todo!()
            },
            ASTNode::ATOMIC_TYPE(atomic_type_node) => {
                todo!()
            },
            ASTNode::ARRAY_TYPE(array_type_node) => {
                todo!()
            },
            ASTNode::USER_DEFINED_TYPE(user_defined_type) => {
                todo!()
            },
            ASTNode::EXPRESSION(expression_node) => {
                todo!()
            },
            ASTNode::ATOMIC_EXPRESSION(atomic_expression_node) => {
                todo!()
            },
            ASTNode::PARENTHESISED_EXPRESSION(parenthesised_expression_node) => {
                todo!()
            },
            ASTNode::UNARY_EXPRESSION(unary_expression_node) => {
                todo!()
            },
            ASTNode::ONLY_UNARY_EXPRESSION(only_unary_expression_node) => {
                todo!()
            },
            ASTNode::BINARY_EXPRESSION(binary_expression_node) => {
                todo!()
            },
            ASTNode::LOGICAL_EXPRESSION(logical_expression_node) => {
                todo!()
            },
            ASTNode::PARAMS(params_node) => {
                todo!()
            },
            ASTNode::OK_PARAMS(ok_params_node) => {
                todo!()
            },
            ASTNode::CALL_EXPRESSION(call_expression_node) => {
                todo!()
            },
            ASTNode::CLASS_METHOD_CALL(class_method_call_node) => {
                todo!()
            },
            ASTNode::ATOM(atom_node) => {
                todo!()
            },
            ASTNode::ATOM_START(atom_start_node) => {
                todo!()
            },
            ASTNode::CALL(call_node) => {
                todo!()
            },
            ASTNode::PROPERTY_ACCESS(property_access_node) => {
                todo!()
            },
            ASTNode::METHOD_ACCESS(method_access_node) => {
                todo!()
            },
            ASTNode::INDEX_ACCESS(index_access_node) => {
                todo!()
            },
            ASTNode::TOKEN(token_node) => {
                todo!()
            },
            ASTNode::OK_TOKEN(ok_token_node) => {
                todo!()
            },
            ASTNode::MISSING_TOKEN(missing_token_node) => {
                todo!()
            },
            ASTNode::SKIPPED_TOKEN(skipped_token_node) => {
                todo!()
            }
        }
    }
}