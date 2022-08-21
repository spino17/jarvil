use crate::{
    ast::ast::ASTNode
};
use crate::ast::ast::{
    StatemenIndentWrapperNode, CoreStatemenIndentWrapperNode, StatementNode, IncorrectlyIndentedStatementNode, 
    SkippedTokenNode, SkippedTokensNode, CoreStatementNode, ExpressionStatementNode, AssignmentNode, VariableDeclarationNode, 
    FunctionDeclarationNode, TypeDeclarationNode, StructStatementNode, MissingTokenNode, CoreAssignmentNode, OkAssignmentNode, 
    InvalidLValueNode, CoreTypeDeclarationNode, StructDeclarationNode, LambdaDeclarationNode, BlockNode, CoreLambdaDeclarationNode, 
    TokenNode, NameTypeSpecNode, OkLambdaDeclarationNode, NameTypeSpecsNode, TypeExpressionNode, CoreFunctionDeclarationNode, 
    OkFunctionDeclarationNode, RAssignmentNode, CoreRAssignmentNode, ExpressionNode, CoreNameTypeSpecsNode, OkNameTypeSpecsNode, 
    CoreTypeExpressionNode, AtomicTypeNode, UserDefinedTypeNode, ArrayTypeNode, CoreExpressionNode, UnaryExpressionNode, BinaryExpressionNode, 
    ComparisonNode, CoreAtomicExpressionNode, ParenthesisedExpressionNode, AtomNode, CoreUnaryExpressionNode, AtomicExpressionNode, 
    OnlyUnaryExpressionNode, CoreParamsNode, OkParamsNode, ParamsNode, CoreAtomNode, AtomStartNode, CallNode, PropertyAccessNode, 
    MethodAccessNode, IndexAccessNode, CoreAtomStartNode, CallExpressionNode, ClassMethodCallNode, ReturnStatementNode
};

use super::ast::{CoreTokenNode, OkTokenNode, CoreIdentifierNode, OkIdentifierNode, IdentifierNode};

// This kind of visitor pattern implementation is taken from Golang Programming Language
// See /src/go/ast/walk.go

// TODO - make this file generated by AST enum. In future we can have complete ast module being generated by a syntax tree DSL.

pub trait Visitor {
    fn visit(&mut self, node: &ASTNode) -> Option<()>;

    impl_node_walk!(walk_block, BlockNode, new_with_BlockNode);
    impl_node_walk!(walk_stmt_indent_wrapper, StatemenIndentWrapperNode, new_with_StatemenIndentWrapperNode);
    impl_node_walk!(walk_stmt, StatementNode, new_with_StatementNode);
    impl_node_walk!(walk_incorrectly_indented_stmt, IncorrectlyIndentedStatementNode, new_with_IncorrectlyIndentedStatementNode);
    impl_node_walk!(walk_skipped_tokens, SkippedTokensNode, new_with_SkippedTokensNode);
    impl_node_walk!(walk_skipped_token, SkippedTokenNode, new_with_SkippedTokenNode);
    impl_node_walk!(walk_expr_stmt, ExpressionStatementNode, new_with_ExpressionStatementNode);
    impl_node_walk!(walk_assignment, AssignmentNode, new_with_AssignmentNode);
    impl_node_walk!(walk_variable_decl, VariableDeclarationNode, new_with_VariableDeclarationNode);
    impl_node_walk!(walk_func_decl, FunctionDeclarationNode, new_with_FunctionDeclarationNode);
    impl_node_walk!(walk_type_declaration, TypeDeclarationNode, new_with_TypeDeclarationNode);
    impl_node_walk!(walk_struct_stmt, StructStatementNode, new_with_StructStatementNode);
    impl_node_walk!(walk_missing_tokens, MissingTokenNode, new_with_MissingTokenNode);
    impl_node_walk!(walk_ok_assignment, OkAssignmentNode, new_with_OkAssignmentNode);
    impl_node_walk!(walk_invalid_l_value_assignment, InvalidLValueNode, new_with_InvalidLValueNode);
    impl_node_walk!(walk_struct_decl, StructDeclarationNode, new_with_StructDeclarationNode);
    impl_node_walk!(walk_lambda_decl, LambdaDeclarationNode, new_with_LambdaDeclarationNode);
    impl_node_walk!(walk_token, TokenNode, new_with_TokenNode);
    impl_node_walk!(walk_name_type_spec, NameTypeSpecNode, new_with_NameTypeSpecNode);
    impl_node_walk!(walk_name_type_specs, NameTypeSpecsNode, new_with_NameTypeSpecsNode);
    impl_node_walk!(walk_ok_lambda_declaration, OkLambdaDeclarationNode, new_with_OkLambdaDeclarationNode);
    impl_node_walk!(walk_type_expression, TypeExpressionNode, new_with_TypeExpressionNode);
    impl_node_walk!(walk_ok_func_decl, OkFunctionDeclarationNode, new_with_OkFunctionDeclarationNode);
    impl_node_walk!(walk_r_assignment, RAssignmentNode, new_with_RAssignmentNode);
    impl_node_walk!(walk_expression, ExpressionNode, new_with_ExpressionNode);
    impl_node_walk!(walk_ok_name_type_specs, OkNameTypeSpecsNode, new_with_OkNameTypeSpecsNode);
    impl_node_walk!(walk_atomic_type, AtomicTypeNode, new_with_AtomicTypeNode);
    impl_node_walk!(walk_user_defined_type, UserDefinedTypeNode, new_with_UserDefinedTypeNode);
    impl_node_walk!(walk_array_type, ArrayTypeNode, new_with_ArrayTypeNode);
    impl_node_walk!(walk_unary_expression, UnaryExpressionNode, new_with_UnaryExpressionNode);
    impl_node_walk!(walk_binary_expression, BinaryExpressionNode, new_with_BinaryExpressionNode);
    impl_node_walk!(walk_comparison, ComparisonNode, new_with_ComparisonNode);
    impl_node_walk!(walk_parenthesised_expression, ParenthesisedExpressionNode, new_with_ParenthesisedExpressionNode);
    impl_node_walk!(walk_atom, AtomNode, new_with_AtomNode);
    impl_node_walk!(walk_atomic_expression, AtomicExpressionNode, new_with_AtomicExpressionNode);
    impl_node_walk!(walk_only_unary_expression, OnlyUnaryExpressionNode, new_with_OnlyUnaryExpressionNode);
    impl_node_walk!(walk_ok_params, OkParamsNode, new_with_OkParamsNode);
    impl_node_walk!(walk_params, ParamsNode, new_with_ParamsNode);
    impl_node_walk!(walk_atom_start, AtomStartNode, new_with_AtomStartNode);
    impl_node_walk!(walk_call, CallNode, new_with_CallNode);
    impl_node_walk!(walk_property_access, PropertyAccessNode, new_with_PropertyAccessNode);
    impl_node_walk!(walk_method_access, MethodAccessNode, new_with_MethodAccessNode);
    impl_node_walk!(walk_index_access, IndexAccessNode, new_with_IndexAccessNode);
    impl_node_walk!(walk_call_expression, CallExpressionNode, new_with_CallExpressionNode);
    impl_node_walk!(walk_class_method_call, ClassMethodCallNode, new_with_ClassMethodCallNode);
    impl_node_walk!(walk_return_stmt, ReturnStatementNode, new_with_ReturnStatementNode);
    impl_node_walk!(walk_ok_token, OkTokenNode, new_with_OkTokenNode);
    impl_node_walk!(walk_ok_identifier, OkIdentifierNode, new_with_OkIdentifierNode);
    impl_node_walk!(walk_identifier, IdentifierNode, new_with_IdentifierNode);

    // This method is AST walk which means it does not visit symbols. Visiting symbols can be useful while formatting
    fn walk(&mut self, node: &ASTNode) {
        match self.visit(node) {
            None => return,
            _ => {}
        }

        match node {
            ASTNode::BLOCK(block_node) => {
                for stmt in &block_node.0.as_ref().borrow().stmts {
                    self.walk_stmt_indent_wrapper(stmt);
                }
            },
            ASTNode::STATEMENT_INDENT_WRAPPER(stmt_indent_wrapper_node) => {
                match stmt_indent_wrapper_node.core_ref() {
                    CoreStatemenIndentWrapperNode::CORRECTLY_INDENTED(stmt) => {
                        self.walk_stmt(stmt);
                    }
                    CoreStatemenIndentWrapperNode::INCORRECTLY_INDENTED(stmt) => {
                        self.walk_incorrectly_indented_stmt(stmt);
                    },
                    CoreStatemenIndentWrapperNode::LEADING_SKIPPED_TOKENS(skipped_tokens) => {
                        self.walk_skipped_tokens(skipped_tokens);
                    },
                    CoreStatemenIndentWrapperNode::TRAILING_SKIPPED_TOKENS(skipped_tokens) => {
                        self.walk_skipped_tokens(skipped_tokens);
                    },
                    CoreStatemenIndentWrapperNode::EXTRA_NEWLINES(skipped_tokens) => {
                        self.walk_skipped_tokens(skipped_tokens);
                    },
                }
            },
            ASTNode::SKIPPED_TOKENS(skipped_tokens) => {
                for skipped_token in &skipped_tokens.core_ref().skipped_tokens {
                    self.walk_skipped_token(skipped_token);
                }
            },
            ASTNode::INCORRECTLY_INDENTED_STATEMENT(stmt) => {
                self.walk_incorrectly_indented_stmt(stmt);
            },
            ASTNode::STATEMENT(statement_node) => {
                match statement_node.core_ref() {
                    CoreStatementNode::EXPRESSION(expr_stmt) => {
                        self.walk_expr_stmt(expr_stmt);
                    },
                    CoreStatementNode::ASSIGNMENT(assignment) => {
                        self.walk_assignment(assignment);
                    },
                    CoreStatementNode::VARIABLE_DECLARATION(variable_decl) => {
                        self.walk_variable_decl(variable_decl);
                    },
                    CoreStatementNode::FUNCTION_DECLARATION(func_decl) => {
                        self.walk_func_decl(func_decl);
                    },
                    CoreStatementNode::TYPE_DECLARATION(type_decl) => {
                        self.walk_type_declaration(type_decl);
                    },
                    CoreStatementNode::STRUCT_STATEMENT(struct_stmt) => {
                        self.walk_struct_stmt(struct_stmt);
                    },
                    CoreStatementNode::RETURN(return_stmt) => {
                        self.walk_return_stmt(return_stmt);
                    }
                    CoreStatementNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    },
                }
            },
            ASTNode::EXPRESSION_STATEMENT(expr_stmt) => {
                let core_expr_stmt = expr_stmt.core_ref();
                self.walk_expression(&core_expr_stmt.expr);
                self.walk_token(&core_expr_stmt.newline);
            },
            ASTNode::ASSIGNMENT(assignment_node) => {
                match assignment_node.core_ref() {
                    CoreAssignmentNode::OK(ok_assignment) => {
                        self.walk_ok_assignment(ok_assignment);
                    },
                    CoreAssignmentNode::INVALID_L_VALUE(invalid_l_value_assignment) => {
                        self.walk_invalid_l_value_assignment(invalid_l_value_assignment);
                    }
                }
            },
            ASTNode::OK_ASSIGNMENT(ok_assignment) => {
                let core_ok_assignment = ok_assignment.core_ref();
                self.walk_atom(&core_ok_assignment.l_atom);
                self.walk_r_assignment(&core_ok_assignment.r_assign);
            },
            ASTNode::INVALID_L_VALUE(invalid_l_value) => {
                let core_invalid_l_value = invalid_l_value.core_ref();
                self.walk_expression(&core_invalid_l_value.l_expr);
                self.walk_r_assignment(&core_invalid_l_value.r_assign);
            },
            ASTNode::STRUCT_STATEMENT(struct_statement) => {
                self.walk_name_type_spec(&struct_statement.core_ref().name_type_spec);
            },
            ASTNode::TYPE_DECLARATION(type_declaration_node) => {
                match &type_declaration_node.core_ref() {
                    CoreTypeDeclarationNode::STRUCT(struct_decl) => {
                        self.walk_struct_decl(struct_decl);
                    },
                    CoreTypeDeclarationNode::LAMBDA(lambda_decl) => {
                        self.walk_lambda_decl(lambda_decl);
                    },
                    CoreTypeDeclarationNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            },
            ASTNode::STRUCT_DECLARATION(struct_declaration_node) => {
                let core_struct_decl = struct_declaration_node.core_ref();
                self.walk_identifier(&core_struct_decl.name);
                self.walk_block(&core_struct_decl.block);
            },
            ASTNode::LAMBDA_DECLARATION(lambda_declaration_node) => {
                match &lambda_declaration_node.core_ref() {
                    CoreLambdaDeclarationNode::OK(ok_lambda_decl) => {
                        self.walk_ok_lambda_declaration(ok_lambda_decl);
                    },
                    CoreLambdaDeclarationNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            },
            ASTNode::OK_LAMBDA_DECLARATION(ok_lambda_declaration_node) => {
                let core_ok_lambda_decl = ok_lambda_declaration_node.core_ref();
                self.walk_identifier(&core_ok_lambda_decl.name);
                match &core_ok_lambda_decl.args {
                    Some(args) => {
                        self.walk_name_type_specs(args);
                    },
                    None => {}
                }
                match &core_ok_lambda_decl.return_type {
                    Some(return_type) => {
                        self.walk_type_expression(return_type);
                    },
                    None => {}
                }
            },
            ASTNode::FUNCTION_DECLARATION(function_declaration_node) => {
                let core_func_decl = function_declaration_node.core_ref();
                match &core_func_decl {
                    CoreFunctionDeclarationNode::OK(ok_func_decl) => {
                        self.walk_ok_func_decl(ok_func_decl);
                    },
                    CoreFunctionDeclarationNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            },
            ASTNode::OK_FUNCTION_DECLARATION(ok_function_declaration_node) => {
                let core_ok_func_decl = ok_function_declaration_node.core_ref();
                match &core_ok_func_decl.name {
                    Some(func_name) => {
                        self.walk_identifier(func_name);
                    },
                    None => {}
                }
                match &core_ok_func_decl.args {
                    Some(name_type_specs) => {
                        self.walk_name_type_specs(name_type_specs);
                    },
                    None => {}
                }
                match &core_ok_func_decl.return_type {
                    Some(return_type) => {
                        self.walk_type_expression(return_type);
                    },
                    None => {}
                }
                self.walk_block(&core_ok_func_decl.block);
            },
            ASTNode::VARIABLE_DECLARATION(variable_declaration_node) => {
                let core_variable_decl = variable_declaration_node.core_ref();
                self.walk_identifier(&core_variable_decl.name);
                self.walk_r_assignment(&core_variable_decl.r_assign);
            },
            ASTNode::RETURN(return_stmt) => {
                let core_return_stmt = return_stmt.core_ref();
                self.walk_expression(&core_return_stmt.expr);
            }
            ASTNode::R_ASSIGNMENT(r_assignment_node) => {
                let core_r_assignment = r_assignment_node.core_ref();
                match core_r_assignment {
                    CoreRAssignmentNode::EXPRESSION(expr_stmt) => {
                        self.walk_expr_stmt(expr_stmt);
                    },
                    CoreRAssignmentNode::LAMBDA(lambda) => {
                        self.walk_func_decl(lambda);
                    },
                    CoreRAssignmentNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            },
            ASTNode::NAME_TYPE_SPECS(name_type_specs_node) => {
                let core_name_type_specs = name_type_specs_node.core_ref();
                match core_name_type_specs {
                    CoreNameTypeSpecsNode::OK(ok_name_type_specs) => {
                        self.walk_ok_name_type_specs(ok_name_type_specs);
                    },
                    CoreNameTypeSpecsNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            },
            ASTNode::OK_NAME_TYPE_SPECS(ok_name_type_specs_node) => {
                let core_ok_name_type_specs = ok_name_type_specs_node.core_ref();
                self.walk_name_type_spec(&core_ok_name_type_specs.arg);
                match &core_ok_name_type_specs.remaining_args {
                    Some(remaining_args) => {
                        self.walk_name_type_specs(remaining_args);
                    },
                    None => {}
                }
            },
            ASTNode::NAME_TYPE_SPEC(name_type_spec_node) => {
                let core_name_type_spec = name_type_spec_node.core_ref();
                self.walk_identifier(&core_name_type_spec.name);
                self.walk_type_expression(&core_name_type_spec.data_type);
            },
            ASTNode::TYPE_EXPRESSION(type_expression_node) => {
                let core_type_expr = type_expression_node.core_ref();
                match core_type_expr {
                    CoreTypeExpressionNode::ATOMIC(atomic_type) => {
                        self.walk_atomic_type(atomic_type);
                    },
                    CoreTypeExpressionNode::USER_DEFINED(user_defined_type) => {
                        self.walk_user_defined_type(user_defined_type);
                    },
                    CoreTypeExpressionNode::ARRAY(array_type) => {
                        self.walk_array_type(array_type);
                    },
                    CoreTypeExpressionNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            },
            ASTNode::ATOMIC_TYPE(atomic_type_node) => {
                let core_atomic_type = atomic_type_node.core_ref();
                self.walk_token(&core_atomic_type.kind)
            },
            ASTNode::ARRAY_TYPE(array_type_node) => {
                let core_array_type = array_type_node.core_ref();
                self.walk_type_expression(&core_array_type.sub_type);
                self.walk_token(&core_array_type.size);
            },
            ASTNode::USER_DEFINED_TYPE(user_defined_type) => {
                let core_user_defined_type = user_defined_type.core_ref();
                self.walk_identifier(&core_user_defined_type.name)
            },
            ASTNode::EXPRESSION(expression_node) => {
                let core_expr = expression_node.core_ref();
                match core_expr {
                    CoreExpressionNode::UNARY(unary_expr) => {
                        self.walk_unary_expression(unary_expr);
                    },
                    CoreExpressionNode::BINARY(binary_expr) => {
                        self.walk_binary_expression(binary_expr);
                    },
                    CoreExpressionNode::COMPARISON(comparison_expr) => {
                        self.walk_comparison(comparison_expr);
                    }
                    CoreExpressionNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            },
            ASTNode::ATOMIC_EXPRESSION(atomic_expression_node) => {
                let core_atomic_expr = atomic_expression_node.core_ref();
                match core_atomic_expr {
                    CoreAtomicExpressionNode::BOOL_VALUE(token) => {
                        self.walk_token(token);
                    },
                    CoreAtomicExpressionNode::INTEGER(token) => {
                        self.walk_token(token);
                    },
                    CoreAtomicExpressionNode::FLOATING_POINT_NUMBER(token) => {
                        self.walk_token(token);
                    },
                    CoreAtomicExpressionNode::LITERAL(token) => {
                        self.walk_token(token);
                    },
                    CoreAtomicExpressionNode::PARENTHESISED_EXPRESSION(parenthesised_expr) => {
                        self.walk_parenthesised_expression(parenthesised_expr);
                    },
                    CoreAtomicExpressionNode::ATOM(atom) => {
                        self.walk_atom(atom);
                    },
                    CoreAtomicExpressionNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens)
                    },
                }
            },
            ASTNode::PARENTHESISED_EXPRESSION(parenthesised_expression_node) => {
                let parenthesised_expr = parenthesised_expression_node.core_ref();
                self.walk_expression(&parenthesised_expr.expr);
            },
            ASTNode::UNARY_EXPRESSION(unary_expression_node) => {
                let core_unary_expr = unary_expression_node.core_ref();
                match core_unary_expr {
                    CoreUnaryExpressionNode::ATOMIC(atomic) => {
                        self.walk_atomic_expression(atomic);
                    },
                    CoreUnaryExpressionNode::UNARY(unary) => {
                        self.walk_only_unary_expression(unary);
                    },
                    CoreUnaryExpressionNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            },
            ASTNode::ONLY_UNARY_EXPRESSION(only_unary_expression_node) => {
                let core_only_unary_expr = only_unary_expression_node.core_ref();
                self.walk_token(&core_only_unary_expr.operator);
                self.walk_unary_expression(&core_only_unary_expr.unary_expr);
            },
            ASTNode::BINARY_EXPRESSION(binary_expression_node) => {
                let core_binary_expr = binary_expression_node.core_ref();
                self.walk_expression(&core_binary_expr.left_expr);
                self.walk_token(&core_binary_expr.operator);
                self.walk_expression(&core_binary_expr.right_expr);
            },
            ASTNode::COMPARISON(comparison_expression_node) => {
                let core_comp_expr = comparison_expression_node.core_ref();
                let operator_len = core_comp_expr.operators.len();
                self.walk_expression(&core_comp_expr.operands[0]);
                for i in 0..operator_len {
                    self.walk_token(&core_comp_expr.operators[i]);
                    self.walk_expression(&core_comp_expr.operands[i]);
                }
            },
            ASTNode::PARAMS(params_node) => {
                let core_params = params_node.core_ref();
                match core_params {
                    CoreParamsNode::OK(ok_params) => {
                        self.walk_ok_params(ok_params);
                    },
                    CoreParamsNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            },
            ASTNode::OK_PARAMS(ok_params_node) => {
                let core_ok_params = ok_params_node.core_ref();
                self.walk_expression(&core_ok_params.param);
                match &core_ok_params.remaining_params {
                    Some(remaining_params) => {
                        self.walk_params(remaining_params);
                    },
                    None => {}
                }
            },
            ASTNode::CALL_EXPRESSION(call_expression_node) => {
                let core_call_expr = call_expression_node.core_ref();
                self.walk_identifier(&core_call_expr.function_name);
                match &core_call_expr.params {
                    Some(params) => {
                        self.walk_params(params)
                    },
                    None => {}
                }
            },
            ASTNode::CLASS_METHOD_CALL(class_method_call_node) => {
                let core_class_method_call = class_method_call_node.core_ref();
                self.walk_identifier(&core_class_method_call.class_name);
                self.walk_identifier(&core_class_method_call.class_method_name);
                match &core_class_method_call.params {
                    Some(params) => {
                        self.walk_params(params);
                    },
                    None => {}
                }
            },
            ASTNode::ATOM(atom_node) => {
                let core_atom = atom_node.core_ref();
                match core_atom {
                    CoreAtomNode::ATOM_START(atom_start) => {
                        self.walk_atom_start(atom_start);
                    },
                    CoreAtomNode::CALL(call_node) => {
                        self.walk_call(call_node);
                    },
                    CoreAtomNode::PROPERTRY_ACCESS(property_access) => {
                        self.walk_property_access(property_access);
                    },
                    CoreAtomNode::METHOD_ACCESS(method_access) => {
                        self.walk_method_access(method_access);
                    },
                    CoreAtomNode::INDEX_ACCESS(index_access) => {
                        self.walk_index_access(index_access);
                    },
                }
            },
            ASTNode::ATOM_START(atom_start_node) => {
                let core_atom_start = atom_start_node.core_ref();
                match core_atom_start {
                    CoreAtomStartNode::IDENTIFIER(token) => {
                        self.walk_identifier(token);
                    },
                    CoreAtomStartNode::FUNCTION_CALL(call_expr) => {
                        self.walk_call_expression(call_expr);
                    },
                    CoreAtomStartNode::CLASS_METHOD_CALL(class_method) => {
                        self.walk_class_method_call(class_method);
                    },
                }
            },
            ASTNode::CALL(call_node) => {
                let core_call = call_node.core_ref();
                self.walk_atom(&core_call.atom);
                match &core_call.params {
                    Some(params) => {
                        self.walk_params(params);
                    },
                    None => {}
                }
            },
            ASTNode::PROPERTY_ACCESS(property_access_node) => {
                let core_property_access = property_access_node.core_ref();
                self.walk_atom(&core_property_access.atom);
                self.walk_identifier(&core_property_access.propertry);
            },
            ASTNode::METHOD_ACCESS(method_access_node) => {
                let core_method_access = method_access_node.core_ref();
                self.walk_atom(&core_method_access.atom);
                self.walk_identifier(&core_method_access.method_name);
                match &core_method_access.params {
                    Some(params) => {
                        self.walk_params(params);
                    },
                    None => {}
                }
            },
            ASTNode::INDEX_ACCESS(index_access_node) => {
                let core_index_access = index_access_node.core_ref();
                self.walk_atom(&core_index_access.atom);
                self.walk_expression(&core_index_access.index);
            },
            ASTNode::TOKEN(token) => {
                let token = token.core_ref();
                match token {
                    CoreTokenNode::OK(ok_token) => self.walk_ok_token(ok_token),
                    CoreTokenNode::MISSING_TOKENS(missing_tokens) => self.walk_missing_tokens(missing_tokens),
                    CoreTokenNode::SKIPPED(skipped_token) => self.walk_skipped_token(skipped_token),
                }
            },
            ASTNode::IDENTIFIER(identifier) => {
                let core_identifier = identifier.core_ref();
                match core_identifier {
                    CoreIdentifierNode::OK(ok_identifier) => self.walk_ok_identifier(ok_identifier),
                    CoreIdentifierNode::MISSING_TOKENS(missing_tokens) => self.walk_missing_tokens(missing_tokens),
                    CoreIdentifierNode::SKIPPED(skipped_token) => self.walk_skipped_token(skipped_token),
                }
            },
            ASTNode::OK_IDENTIFIER(_) => {
                // do nothing
            },
            ASTNode::OK_TOKEN(_) => {
                // do nothing
            },
            ASTNode::MISSING_TOKEN(_) => {
                // do nothing
            },
            ASTNode::SKIPPED_TOKEN(_) => {
                // do nothing
            }
        }
    }
}