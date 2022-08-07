use crate::{ast::ast::ASTNode, scope::function};
use super::ast::{StatementKind, StatementIndentWrapperKind, TypeDeclarationKind, LambdaDeclarationKind, FunctionDeclarationKind, RAssignmentKind, ExpressionKind, NameTypeSpecsKind, TypeExpressionKind};

// This kind of visitor pattern implementation is taken from Golang Programming Language
// See /src/go/ast/walk.go
pub trait Visitor {
    fn visit(&mut self, node: &ASTNode) -> Option<()>;

    // This method is AST walk which means it does not visit symbols. Visiting symbols can be useful while formatting
    fn walk(&mut self, node: ASTNode) {
        match self.visit(&node) {
            None => return,
            _ => {}
        }
        
        match &node {
            ASTNode::BLOCK(block_node) => {
                let core_block = block_node.core_ref();
                for stmt in &*core_block.stmts.as_ref().borrow() {
                    self.walk(ASTNode::new_with_StatemenIndentWrapperNode(stmt));
                }
            },
            ASTNode::STATEMENT_INDENT_WRAPPER(stmt_indent_wrapper_node) => {
                match &stmt_indent_wrapper_node.core_ref().kind {
                    StatementIndentWrapperKind::CORRECTLY_INDENTED(stmt_node) => {
                        self.walk(ASTNode::new_with_StatementNode(stmt_node));
                    }
                    StatementIndentWrapperKind::INCORRECTLY_INDENTED((stmt_node, _)) => {
                        self.walk(ASTNode::new_with_StatementNode(stmt_node));
                    },
                    StatementIndentWrapperKind::LEADING_SKIPPED_TOKENS(skipped_tokens) => {
                        self.walk(ASTNode::new_with_SkippedTokens(skipped_tokens));
                    },
                    StatementIndentWrapperKind::TRAILING_SKIPPED_TOKENS(skipped_tokens) => {
                        self.walk(ASTNode::new_with_SkippedTokens(skipped_tokens));
                    },
                    StatementIndentWrapperKind::EXTRA_NEWLINES(skipped_tokens) => {
                        self.walk(ASTNode::new_with_SkippedTokens(skipped_tokens));
                    },
                }
            },
            ASTNode::SKIPPED_TOKENS(skipped_tokens) => {
                let core_skipped_tokens = skipped_tokens.core_ref();
                for skipped_token in core_skipped_tokens.skipped_tokens.as_ref() {
                    self.walk(ASTNode::new_with_SkippedTokenNode(skipped_token));
                }
            },
            ASTNode::STATEMENT(statement_node) => {
                let core_stmt = statement_node.core_ref();
                match &core_stmt.kind {
                    StatementKind::EXPRESSION((expr_node, _)) => {
                        self.walk(ASTNode::new_with_ExpressionNode(expr_node));
                    },
                    StatementKind::ASSIGNMENT(assignment_node) => {
                        self.walk(ASTNode::new_with_AssignmentNode(assignment_node));
                    },
                    StatementKind::VARIABLE_DECLARATION(variable_decl_node) => {
                        self.walk(ASTNode::new_with_VariableDeclarationNode(variable_decl_node));
                    },
                    StatementKind::FUNCTION_DECLARATION(func_decl_node) => {
                        self.walk(ASTNode::new_with_FunctionDeclarationNode(func_decl_node));
                    },
                    StatementKind::TYPE_DECLARATION(type_decl_node) => {
                        self.walk(ASTNode::new_with_TypeDeclarationNode(type_decl_node));
                    },
                    StatementKind::STRUCT_STATEMENT(struct_stmt_node) => {
                        self.walk(ASTNode::new_with_StructStatementNode(struct_stmt_node));
                    },
                    StatementKind::MISSING_TOKENS(missing_token_node) => {
                        self.walk(ASTNode::new_with_MissingTokenNode(missing_token_node));
                    },
                }
            },
            ASTNode::ASSIGNMENT(assignment_node) => {
                let core_assignment = assignment_node.core_ref();
                self.walk(ASTNode::new_with_AtomNode(&core_assignment.l_atom));
                self.walk(ASTNode::new_with_RAssignmentNode(&core_assignment.r_assign));
            },
            ASTNode::STRUCT_STATEMENT(struct_statement_node) => {
                let core_struct_stmt = struct_statement_node.core_ref();
                self.walk(ASTNode::new_with_NameTypeSpecNode(&core_struct_stmt.name_type_spec));
            },
            ASTNode::TYPE_DECLARATION(type_declaration_node) => {
                let core_type_decl = type_declaration_node.core_ref();
                match &core_type_decl.kind {
                    TypeDeclarationKind::STRUCT(struct_decl) => {
                        self.walk(ASTNode::new_with_StructDeclarationNode(&struct_decl));
                    },
                    TypeDeclarationKind::LAMBDA(lambda_decl) => {
                        self.walk(ASTNode::new_with_LambdaDeclarationNode(&lambda_decl));
                    },
                    TypeDeclarationKind::MISSING_TOKENS(missing_tokens) => {
                        self.walk(ASTNode::new_with_MissingTokenNode(&missing_tokens));
                    }
                }
            },
            ASTNode::STRUCT_DECLARATION(struct_declaration_node) => {
                let core_struct_decl = struct_declaration_node.core_ref();
                self.walk(ASTNode::new_with_TokenNode(&core_struct_decl.name));
                self.walk(ASTNode::new_with_BlockNode(&core_struct_decl.block));
            },
            ASTNode::LAMBDA_DECLARATION(lambda_declaration_node) => {
                let core_lambda_decl = lambda_declaration_node.core_ref();
                match &core_lambda_decl.kind {
                    LambdaDeclarationKind::OK(ok_lambda_decl) => {
                        self.walk(ASTNode::new_with_OkLambdaDeclarationNode(&ok_lambda_decl));
                    },
                    LambdaDeclarationKind::MISSING_TOKENS(missing_tokens) => {
                        self.walk(ASTNode::new_with_MissingTokenNode(&missing_tokens));
                    }
                }
            },
            ASTNode::OK_LAMBDA_DECLARATION(ok_lambda_declaration_node) => {
                let core_ok_lambda_decl = ok_lambda_declaration_node.core_ref();
                self.walk(ASTNode::new_with_TokenNode(&core_ok_lambda_decl.name));
                match &core_ok_lambda_decl.args {
                    Some(args) => {
                        self.walk(ASTNode::new_with_NameTypeSpecsNode(&args));
                    },
                    None => {}
                }
                match &core_ok_lambda_decl.return_type {
                    Some(return_type) => {
                        self.walk(ASTNode::new_with_TypeExpressionNode(&return_type));
                    },
                    None => {}
                }
            },
            ASTNode::FUNCTION_DECLARATION(function_declaration_node) => {
                let core_func_decl = function_declaration_node.core_ref();
                match &core_func_decl.kind {
                    FunctionDeclarationKind::OK(ok_func_decl) => {
                        self.walk(ASTNode::new_with_OkFunctionDeclarationNode(&ok_func_decl));
                    },
                    FunctionDeclarationKind::MISSING_TOKENS(missing_tokens) => {
                        self.walk(ASTNode::new_with_MissingTokenNode(&missing_tokens));
                    }
                }
            },
            ASTNode::OK_FUNCTION_DECLARATION(ok_function_declaration_node) => {
                let core_ok_func_decl = ok_function_declaration_node.core_ref();
                match &core_ok_func_decl.name {
                    Some(func_name) => {
                        self.walk(ASTNode::new_with_TokenNode(func_name));
                    },
                    None => {}
                }
                match &core_ok_func_decl.args {
                    Some(name_type_specs) => {
                        self.walk(ASTNode::new_with_NameTypeSpecsNode(name_type_specs));
                    },
                    None => {}
                }
                match &core_ok_func_decl.return_type {
                    Some(return_type) => {
                        self.walk(ASTNode::new_with_TypeExpressionNode(return_type));
                    },
                    None => {}
                }
                self.walk(ASTNode::new_with_BlockNode(&core_ok_func_decl.block));
            },
            ASTNode::VARIABLE_DECLARATION(variable_declaration_node) => {
                let core_variable_decl = variable_declaration_node.core_ref();
                self.walk(ASTNode::new_with_TokenNode(&core_variable_decl.name));
                self.walk(ASTNode::new_with_RAssignmentNode(&core_variable_decl.r_assign));
            },
            ASTNode::R_ASSIGNMENT(r_assignment_node) => {
                let core_r_assignment = r_assignment_node.core_ref();
                match &core_r_assignment.kind {
                    RAssignmentKind::EXPRESSION((expr, _)) => {
                        self.walk(ASTNode::new_with_ExpressionNode(expr));
                    },
                    RAssignmentKind::LAMBDA(lambda) => {
                        self.walk(ASTNode::new_with_FunctionDeclarationNode(lambda));
                    },
                    RAssignmentKind::MISSING_TOKENS(missing_tokens) => {
                        self.walk(ASTNode::new_with_MissingTokenNode(missing_tokens));
                    }
                }
            },
            ASTNode::NAME_TYPE_SPECS(name_type_specs_node) => {
                let core_name_type_specs = name_type_specs_node.core_ref();
                match &core_name_type_specs.kind {
                    NameTypeSpecsKind::OK(ok_name_type_specs) => {
                        self.walk(ASTNode::new_with_OkNameTypeSpecsNode(ok_name_type_specs));
                    },
                    NameTypeSpecsKind::MISSING_TOKENS(missing_tokens) => {
                        self.walk(ASTNode::new_with_MissingTokenNode(missing_tokens));
                    }
                }
            },
            ASTNode::OK_NAME_TYPE_SPECS(ok_name_type_specs_node) => {
                let core_ok_name_type_specs = ok_name_type_specs_node.core_ref();
                self.walk(ASTNode::new_with_NameTypeSpecNode(&core_ok_name_type_specs.arg));
                match &core_ok_name_type_specs.remaining_args {
                    Some(remaining_args) => {
                        self.walk(ASTNode::new_with_NameTypeSpecsNode(remaining_args));
                    },
                    None => {}
                }
            },
            ASTNode::NAME_TYPE_SPEC(name_type_spec_node) => {
                let core_name_type_spec = name_type_spec_node.core_ref();
                self.walk(ASTNode::new_with_TokenNode(&core_name_type_spec.name));
                self.walk(ASTNode::new_with_TypeExpressionNode(&core_name_type_spec.data_type));
            },
            ASTNode::TYPE_EXPRESSION(type_expression_node) => {
                let core_type_expr = type_expression_node.core_ref();
                match &core_type_expr.kind {
                    TypeExpressionKind::ATOMIC(atomic_type) => {
                        self.walk(ASTNode::new_with_AtomicTypeNode(atomic_type));
                    },
                    TypeExpressionKind::USER_DEFINED(user_defined_type) => {
                        self.walk(ASTNode::new_with_UserDefinedTypeNode(user_defined_type));
                    },
                    TypeExpressionKind::ARRAY(array_type) => {
                        self.walk(ASTNode::new_with_ArrayTypeNode(array_type));
                    },
                    TypeExpressionKind::MISSING_TOKENS(missing_tokens) => {
                        self.walk(ASTNode::new_with_MissingTokenNode(missing_tokens));
                    }
                }
            },
            ASTNode::ATOMIC_TYPE(atomic_type_node) => {
                let core_atomic_type = atomic_type_node.core_ref();
                self.walk(ASTNode::new_with_TokenNode(&core_atomic_type.kind));
            },
            ASTNode::ARRAY_TYPE(array_type_node) => {
                let core_array_type = array_type_node.core_ref();
                self.walk(ASTNode::new_with_TypeExpressionNode(&core_array_type.sub_type));
                self.walk(ASTNode::new_with_TokenNode(&core_array_type.size));
            },
            ASTNode::USER_DEFINED_TYPE(user_defined_type) => {
                let core_user_defined_type = user_defined_type.core_ref();
                self.walk(ASTNode::new_with_TokenNode(&core_user_defined_type.name));
            },
            ASTNode::EXPRESSION(expression_node) => {
                let core_expr = expression_node.core_ref();
                match &core_expr.kind {
                    ExpressionKind::UNARY(unary_expr) => {
                        self.walk(ASTNode::new_with_UnaryExpressionNode(unary_expr));
                    },
                    ExpressionKind::BINARY(binary_expr) => {
                        self.walk(ASTNode::new_with_BinaryExpressionNode(binary_expr));
                    },
                    ExpressionKind::LOGICAL(logical_expr) => {
                        self.walk(ASTNode::new_with_LogicalExpressionNode(logical_expr));
                    },
                    ExpressionKind::MISSING_TOKENS(missing_tokens) => {
                        self.walk(ASTNode::new_with_MissingTokenNode(missing_tokens));
                    }
                }
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
                todo!()  // TODO - do nothing as they don't have children
            },
            ASTNode::OK_TOKEN(ok_token_node) => {
                todo!()  // TODO - do nothing as they don't have children
            },
            ASTNode::MISSING_TOKEN(missing_token_node) => {
                todo!()  // TODO - do nothing as they don't have children
            },
            ASTNode::SKIPPED_TOKEN(skipped_token_node) => {
                todo!()  // TODO - do nothing as they don't have children
            }
        }
    }
}