use crate::ast::ast::{
    ASTNode, ArrayTypeNode, AssignmentNode, AtomNode, AtomStartNode, AtomicExpressionNode,
    AtomicTypeNode, BinaryExpressionNode, BlockNode, BoundedMethodWrapperNode, CallExpressionNode,
    CallNode, CallableBodyNode, CallablePrototypeNode, ClassMethodCallNode, ComparisonNode,
    CoreAssignmentNode, CoreAtomNode, CoreAtomStartNode, CoreAtomicExpressionNode,
    CoreCallableBodyNode, CoreExpressionNode, CoreIdentifierNode, CoreLambdaTypeDeclarationNode,
    CoreNameTypeSpecsNode, CoreParamsNode, CoreRAssignmentNode, CoreRVariableDeclarationNode,
    CoreSelfKeywordNode, CoreStatemenIndentWrapperNode, CoreStatementNode, CoreTokenNode,
    CoreTypeDeclarationNode, CoreTypeExpressionNode, CoreTypeTupleNode, CoreUnaryExpressionNode,
    ExpressionNode, ExpressionStatementNode, FunctionDeclarationNode, FunctionWrapperNode,
    HashMapTypeNode, IdentifierNode, IncorrectlyIndentedStatementNode, IndexAccessNode,
    InvalidLValueNode, LambdaDeclarationNode, LambdaTypeDeclarationNode, MethodAccessNode,
    MissingTokenNode, NameTypeSpecNode, NameTypeSpecsNode, OkAssignmentNode, OkCallableBodyNode,
    OkIdentifierNode, OkLambdaTypeDeclarationNode, OkNameTypeSpecsNode, OkParamsNode,
    OkSelfKeywordNode, OkTokenNode, OkTypeTupleNode, OnlyUnaryExpressionNode, ParamsNode,
    ParenthesisedExpressionNode, PropertyAccessNode, RAssignmentNode, RVariableDeclarationNode,
    ReturnStatementNode, SelfKeywordNode, SkippedTokenNode, SkippedTokensNode,
    StatemenIndentWrapperNode, StatementNode, StructDeclarationNode, StructPropertyDeclarationNode,
    TokenNode, TypeDeclarationNode, TypeExpressionNode, TypeTupleNode, UnaryExpressionNode,
    UserDefinedTypeNode, VariableDeclarationNode,
};

use super::ast::TupleTypeNode;

// This kind of visitor pattern implementation is taken from `Golang` Programming Language
// See /src/go/ast/walk.go

// TODO - make this file generated by AST enum. In future we can have complete ast module being generated by a syntax tree DSL.

pub trait Visitor {
    fn visit(&mut self, node: &ASTNode) -> Option<()>;

    // Below are helpful macros for wrapping around ASTNode constructor to get specific Node constructors
    impl_node_walk!(walk_block, BlockNode, new_with_BlockNode);
    impl_node_walk!(
        walk_stmt_indent_wrapper,
        StatemenIndentWrapperNode,
        new_with_StatemenIndentWrapperNode
    );
    impl_node_walk!(walk_stmt, StatementNode, new_with_StatementNode);
    impl_node_walk!(
        walk_incorrectly_indented_stmt,
        IncorrectlyIndentedStatementNode,
        new_with_IncorrectlyIndentedStatementNode
    );
    impl_node_walk!(
        walk_skipped_tokens,
        SkippedTokensNode,
        new_with_SkippedTokensNode
    );
    impl_node_walk!(
        walk_skipped_token,
        SkippedTokenNode,
        new_with_SkippedTokenNode
    );
    impl_node_walk!(
        walk_expr_stmt,
        ExpressionStatementNode,
        new_with_ExpressionStatementNode
    );
    impl_node_walk!(walk_assignment, AssignmentNode, new_with_AssignmentNode);
    impl_node_walk!(
        walk_variable_decl,
        VariableDeclarationNode,
        new_with_VariableDeclarationNode
    );
    impl_node_walk!(
        walk_func_decl,
        FunctionDeclarationNode,
        new_with_FunctionDeclarationNode
    );
    impl_node_walk!(
        walk_func_wrapper,
        FunctionWrapperNode,
        new_with_FunctionWrapperNode
    );
    impl_node_walk!(
        walk_bounded_method_wrapper,
        BoundedMethodWrapperNode,
        new_with_BoundedMethodWrapperNode
    );
    impl_node_walk!(
        walk_lambda_decl,
        LambdaDeclarationNode,
        new_with_LambdaDeclarationNode
    );
    impl_node_walk!(
        walk_callable_prototype,
        CallablePrototypeNode,
        new_with_CallablePrototypeNode
    );
    impl_node_walk!(
        walk_callable_body,
        CallableBodyNode,
        new_with_CallableBodyNode
    );
    impl_node_walk!(
        walk_ok_callable_body,
        OkCallableBodyNode,
        new_with_OkCallableBodyNode
    );
    impl_node_walk!(
        walk_type_decl,
        TypeDeclarationNode,
        new_with_TypeDeclarationNode
    );
    impl_node_walk!(
        walk_struct_property_declaration,
        StructPropertyDeclarationNode,
        new_with_StructPropertyDeclarationNode
    );
    impl_node_walk!(
        walk_missing_tokens,
        MissingTokenNode,
        new_with_MissingTokenNode
    );
    impl_node_walk!(
        walk_ok_assignment,
        OkAssignmentNode,
        new_with_OkAssignmentNode
    );
    impl_node_walk!(
        walk_invalid_l_value_assignment,
        InvalidLValueNode,
        new_with_InvalidLValueNode
    );
    impl_node_walk!(
        walk_struct_decl,
        StructDeclarationNode,
        new_with_StructDeclarationNode
    );
    impl_node_walk!(
        walk_lambda_type_decl,
        LambdaTypeDeclarationNode,
        new_with_LambdaTypeDeclarationNode
    );
    impl_node_walk!(walk_token, TokenNode, new_with_TokenNode);
    impl_node_walk!(
        walk_name_type_spec,
        NameTypeSpecNode,
        new_with_NameTypeSpecNode
    );
    impl_node_walk!(
        walk_name_type_specs,
        NameTypeSpecsNode,
        new_with_NameTypeSpecsNode
    );
    impl_node_walk!(
        walk_ok_lambda_type_declaration,
        OkLambdaTypeDeclarationNode,
        new_with_OkLambdaTypeDeclarationNode
    );
    impl_node_walk!(
        walk_type_expression,
        TypeExpressionNode,
        new_with_TypeExpressionNode
    );
    impl_node_walk!(walk_r_assignment, RAssignmentNode, new_with_RAssignmentNode);
    impl_node_walk!(
        walk_r_variable_declaration,
        RVariableDeclarationNode,
        new_with_RVariableDeclarationNode
    );
    impl_node_walk!(walk_expression, ExpressionNode, new_with_ExpressionNode);
    impl_node_walk!(
        walk_ok_name_type_specs,
        OkNameTypeSpecsNode,
        new_with_OkNameTypeSpecsNode
    );
    impl_node_walk!(walk_type_tuple, TypeTupleNode, new_with_TypeTupleNode);
    impl_node_walk!(
        walk_ok_type_tuple,
        OkTypeTupleNode,
        new_with_OkTypeTupleNode
    );
    impl_node_walk!(walk_atomic_type, AtomicTypeNode, new_with_AtomicTypeNode);
    impl_node_walk!(
        walk_user_defined_type,
        UserDefinedTypeNode,
        new_with_UserDefinedTypeNode
    );
    impl_node_walk!(walk_tuple_type, TupleTypeNode, new_with_TupleTypeNode);
    impl_node_walk!(walk_array_type, ArrayTypeNode, new_with_ArrayTypeNode);
    impl_node_walk!(walk_hashmap_type, HashMapTypeNode, new_with_HashMapTypeNode);
    impl_node_walk!(
        walk_unary_expression,
        UnaryExpressionNode,
        new_with_UnaryExpressionNode
    );
    impl_node_walk!(
        walk_binary_expression,
        BinaryExpressionNode,
        new_with_BinaryExpressionNode
    );
    impl_node_walk!(walk_comparison, ComparisonNode, new_with_ComparisonNode);
    impl_node_walk!(
        walk_parenthesised_expression,
        ParenthesisedExpressionNode,
        new_with_ParenthesisedExpressionNode
    );
    impl_node_walk!(walk_atom, AtomNode, new_with_AtomNode);
    impl_node_walk!(
        walk_atomic_expression,
        AtomicExpressionNode,
        new_with_AtomicExpressionNode
    );
    impl_node_walk!(
        walk_only_unary_expression,
        OnlyUnaryExpressionNode,
        new_with_OnlyUnaryExpressionNode
    );
    impl_node_walk!(walk_ok_params, OkParamsNode, new_with_OkParamsNode);
    impl_node_walk!(walk_params, ParamsNode, new_with_ParamsNode);
    impl_node_walk!(walk_atom_start, AtomStartNode, new_with_AtomStartNode);
    impl_node_walk!(walk_call, CallNode, new_with_CallNode);
    impl_node_walk!(
        walk_property_access,
        PropertyAccessNode,
        new_with_PropertyAccessNode
    );
    impl_node_walk!(
        walk_method_access,
        MethodAccessNode,
        new_with_MethodAccessNode
    );
    impl_node_walk!(walk_index_access, IndexAccessNode, new_with_IndexAccessNode);
    impl_node_walk!(
        walk_call_expression,
        CallExpressionNode,
        new_with_CallExpressionNode
    );
    impl_node_walk!(
        walk_class_method_call,
        ClassMethodCallNode,
        new_with_ClassMethodCallNode
    );
    impl_node_walk!(
        walk_return_stmt,
        ReturnStatementNode,
        new_with_ReturnStatementNode
    );
    impl_node_walk!(walk_ok_token, OkTokenNode, new_with_OkTokenNode);
    impl_node_walk!(
        walk_ok_identifier,
        OkIdentifierNode,
        new_with_OkIdentifierNode
    );
    impl_node_walk!(walk_identifier, IdentifierNode, new_with_IdentifierNode);
    impl_node_walk!(walk_self_keyword, SelfKeywordNode, new_with_SelfKeywordNode);
    impl_node_walk!(
        walk_ok_self_keyword,
        OkSelfKeywordNode,
        new_with_OkSelfKeywordNode
    );

    fn walk(&mut self, node: &ASTNode) {
        match self.visit(node) {
            None => return,
            _ => {}
        }

        match node {
            ASTNode::BLOCK(block_node) => {
                let core_block_node = &block_node.0.as_ref().borrow();
                self.walk_token(&core_block_node.newline);
                for stmt in &core_block_node.stmts {
                    self.walk_stmt_indent_wrapper(stmt);
                }
            }
            ASTNode::STATEMENT_INDENT_WRAPPER(stmt_indent_wrapper_node) => {
                match stmt_indent_wrapper_node.core_ref() {
                    CoreStatemenIndentWrapperNode::CORRECTLY_INDENTED(stmt) => {
                        self.walk_stmt(stmt);
                    }
                    CoreStatemenIndentWrapperNode::INCORRECTLY_INDENTED(stmt) => {
                        self.walk_incorrectly_indented_stmt(stmt);
                    }
                    CoreStatemenIndentWrapperNode::LEADING_SKIPPED_TOKENS(skipped_tokens) => {
                        self.walk_skipped_tokens(skipped_tokens);
                    }
                    CoreStatemenIndentWrapperNode::TRAILING_SKIPPED_TOKENS(skipped_tokens) => {
                        self.walk_skipped_tokens(skipped_tokens);
                    }
                    CoreStatemenIndentWrapperNode::EXTRA_NEWLINES(skipped_tokens) => {
                        self.walk_skipped_tokens(skipped_tokens);
                    }
                }
            }
            ASTNode::SKIPPED_TOKENS(skipped_tokens) => {
                for skipped_token in &skipped_tokens.core_ref().skipped_tokens {
                    self.walk_skipped_token(skipped_token);
                }
            }
            ASTNode::INCORRECTLY_INDENTED_STATEMENT(stmt) => {
                let core_stmt = stmt.core_ref();
                self.walk_stmt(&core_stmt.stmt);
            }
            ASTNode::STATEMENT(statement_node) => match statement_node.core_ref() {
                CoreStatementNode::EXPRESSION(expr_stmt) => {
                    self.walk_expr_stmt(expr_stmt);
                }
                CoreStatementNode::ASSIGNMENT(assignment) => {
                    self.walk_assignment(assignment);
                }
                CoreStatementNode::VARIABLE_DECLARATION(variable_decl) => {
                    self.walk_variable_decl(variable_decl);
                }
                CoreStatementNode::FUNCTION_WRAPPER(func_wrapper) => {
                    self.walk_func_wrapper(func_wrapper);
                }
                CoreStatementNode::BOUNDED_METHOD_WRAPPER(bounded_method_wrapper) => {
                    self.walk_bounded_method_wrapper(bounded_method_wrapper);
                }
                CoreStatementNode::TYPE_DECLARATION(type_decl) => {
                    self.walk_type_decl(type_decl);
                }
                CoreStatementNode::STRUCT_PROPERTY_DECLARATION(struct_stmt) => {
                    self.walk_struct_property_declaration(struct_stmt);
                }
                CoreStatementNode::RETURN(return_stmt) => {
                    self.walk_return_stmt(return_stmt);
                }
                CoreStatementNode::MISSING_TOKENS(missing_tokens) => {
                    self.walk_missing_tokens(missing_tokens);
                }
            },
            ASTNode::EXPRESSION_STATEMENT(expr_stmt) => {
                let core_expr_stmt = expr_stmt.core_ref();
                self.walk_expression(&core_expr_stmt.expr);
                self.walk_token(&core_expr_stmt.newline);
            }
            ASTNode::ASSIGNMENT(assignment_node) => match assignment_node.core_ref() {
                CoreAssignmentNode::OK(ok_assignment) => {
                    self.walk_ok_assignment(ok_assignment);
                }
                CoreAssignmentNode::INVALID_L_VALUE(invalid_l_value_assignment) => {
                    self.walk_invalid_l_value_assignment(invalid_l_value_assignment);
                }
            },
            ASTNode::OK_ASSIGNMENT(ok_assignment) => {
                let core_ok_assignment = ok_assignment.core_ref();
                self.walk_atom(&core_ok_assignment.l_atom);
                self.walk_token(&core_ok_assignment.equal);
                self.walk_r_assignment(&core_ok_assignment.r_assign);
            }
            ASTNode::INVALID_L_VALUE(invalid_l_value) => {
                let core_invalid_l_value = invalid_l_value.core_ref();
                self.walk_expression(&core_invalid_l_value.l_expr);
                self.walk_token(&core_invalid_l_value.equal);
                self.walk_r_assignment(&core_invalid_l_value.r_assign);
            }
            ASTNode::STRUCT_PROPERTY_DECLARATION(struct_statement) => {
                let core_struct_stmt = struct_statement.core_ref();
                self.walk_name_type_spec(&core_struct_stmt.name_type_spec);
                self.walk_token(&core_struct_stmt.newline);
            }
            ASTNode::TYPE_DECLARATION(type_decl_node) => match &type_decl_node.core_ref() {
                CoreTypeDeclarationNode::STRUCT(struct_decl) => {
                    self.walk_struct_decl(struct_decl);
                }
                CoreTypeDeclarationNode::LAMBDA(lambda_decl) => {
                    self.walk_lambda_type_decl(lambda_decl);
                }
                CoreTypeDeclarationNode::MISSING_TOKENS(missing_tokens) => {
                    self.walk_missing_tokens(missing_tokens);
                }
            },
            ASTNode::STRUCT_DECLARATION(struct_decl_node) => {
                let core_struct_decl = struct_decl_node.core_ref();
                self.walk_token(&core_struct_decl.type_keyword);
                self.walk_identifier(&core_struct_decl.name);
                self.walk_token(&core_struct_decl.struct_keyword);
                self.walk_token(&core_struct_decl.colon);
                self.walk_block(&core_struct_decl.block);
            }
            ASTNode::LAMBDA_TYPE_DECLARATION(lambda_decl_node) => {
                match &lambda_decl_node.core_ref() {
                    CoreLambdaTypeDeclarationNode::OK(ok_lambda_decl) => {
                        self.walk_ok_lambda_type_declaration(ok_lambda_decl);
                    }
                    CoreLambdaTypeDeclarationNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            }
            ASTNode::OK_LAMBDA_TYPE_DECLARATION(ok_lambda_decl_node) => {
                let core_ok_lambda_decl = ok_lambda_decl_node.core_ref();
                self.walk_token(&core_ok_lambda_decl.type_keyword);
                self.walk_identifier(&core_ok_lambda_decl.name);
                self.walk_token(&core_ok_lambda_decl.lambda_keyword);
                self.walk_token(&core_ok_lambda_decl.equal);
                self.walk_token(&core_ok_lambda_decl.lparen);
                if let Some(type_tuple) = &core_ok_lambda_decl.type_tuple {
                    self.walk_type_tuple(type_tuple);
                }
                self.walk_token(&core_ok_lambda_decl.rparen);
                if let Some(right_arrow) = &core_ok_lambda_decl.right_arrow {
                    self.walk_token(right_arrow);
                }
                if let Some(return_type) = &core_ok_lambda_decl.return_type {
                    self.walk_type_expression(return_type);
                }
                self.walk_token(&core_ok_lambda_decl.newline);
            }
            ASTNode::CALLABLE_PROTOTYPE(callable_prototype) => {
                let core_callable_prototype = callable_prototype.core_ref();
                self.walk_token(&core_callable_prototype.lparen);
                if let Some(name_type_specs) = &core_callable_prototype.params {
                    self.walk_name_type_specs(name_type_specs);
                }
                self.walk_token(&core_callable_prototype.rparen);
                if let Some(right_arrow) = &core_callable_prototype.right_arrow {
                    self.walk_token(right_arrow);
                }
                if let Some(return_type) = &core_callable_prototype.return_type {
                    self.walk_type_expression(return_type);
                }
            }
            ASTNode::CALLABLE_BODY(callable_body) => {
                let core_callable_body = callable_body.0.as_ref();
                match &core_callable_body {
                    CoreCallableBodyNode::OK(ok_callable_body) => {
                        self.walk_ok_callable_body(ok_callable_body);
                    }
                    CoreCallableBodyNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            }
            ASTNode::OK_CALLABLE_BODY(ok_callable_body) => {
                let core_ok_callable_body = ok_callable_body.core_ref();
                self.walk_callable_prototype(&core_ok_callable_body.prototype);
                self.walk_token(&core_ok_callable_body.colon);
                self.walk_block(&core_ok_callable_body.block);
            }
            ASTNode::LAMBDA_DECLARATION(lambda_decl_node) => {
                let core_lambda_decl_node = lambda_decl_node.core_ref();
                self.walk_token(&core_lambda_decl_node.lambda_keyword);
                self.walk_callable_body(&core_lambda_decl_node.body);
            }
            ASTNode::FUNCTION_DECLARATION(function_decl_node) => {
                let core_func_decl = function_decl_node.core_ref();
                self.walk_token(&core_func_decl.def_keyword);
                self.walk_identifier(&core_func_decl.name);
                self.walk_callable_body(&core_func_decl.body);
            }
            ASTNode::FUNCTION_WRAPPER(func_wrapper) => {
                self.walk_func_decl(&func_wrapper.core_ref().func_decl);
            }
            ASTNode::BOUNDED_METHOD_WRAPPER(bounded_method_wrapper) => {
                self.walk_func_decl(&bounded_method_wrapper.0.as_ref().borrow().func_decl);
            }
            ASTNode::VARIABLE_DECLARATION(variable_decl_node) => {
                let core_variable_decl = variable_decl_node.core_ref();
                self.walk_token(&core_variable_decl.let_keyword);
                self.walk_identifier(&core_variable_decl.name);
                self.walk_token(&core_variable_decl.equal);
                self.walk_r_variable_declaration(&core_variable_decl.r_node);
            }
            ASTNode::RETURN(return_stmt) => {
                let core_return_stmt = return_stmt.core_ref();
                self.walk_token(&core_return_stmt.return_keyword);
                if let Some(expr) = &core_return_stmt.expr {
                    self.walk_expression(expr);
                }
                self.walk_token(&core_return_stmt.newline);
            }
            ASTNode::R_ASSIGNMENT(r_assignment_node) => {
                let core_r_assignment = r_assignment_node.core_ref();
                match core_r_assignment {
                    CoreRAssignmentNode::EXPRESSION(expr_stmt) => {
                        self.walk_expr_stmt(expr_stmt);
                    }
                    CoreRAssignmentNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            }
            ASTNode::R_VARIABLE_DECLARATION(r_variable_decl) => {
                let core_r_variable_decl = r_variable_decl.core_ref();
                match core_r_variable_decl {
                    CoreRVariableDeclarationNode::EXPRESSION(expr_stmt) => {
                        self.walk_expr_stmt(expr_stmt);
                    }
                    CoreRVariableDeclarationNode::LAMBDA(lambda) => {
                        self.walk_lambda_decl(lambda);
                    }
                    CoreRVariableDeclarationNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            }
            ASTNode::NAME_TYPE_SPECS(name_type_specs_node) => {
                let core_name_type_specs = name_type_specs_node.core_ref();
                match core_name_type_specs {
                    CoreNameTypeSpecsNode::OK(ok_name_type_specs) => {
                        self.walk_ok_name_type_specs(ok_name_type_specs);
                    }
                    CoreNameTypeSpecsNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            }
            ASTNode::OK_NAME_TYPE_SPECS(ok_name_type_specs_node) => {
                let core_ok_name_type_specs = ok_name_type_specs_node.core_ref();
                self.walk_name_type_spec(&core_ok_name_type_specs.arg);
                if let Some(comma) = &core_ok_name_type_specs.comma {
                    self.walk_token(comma);
                }
                if let Some(remaining_args) = &core_ok_name_type_specs.remaining_args {
                    self.walk_name_type_specs(remaining_args);
                }
            }
            ASTNode::NAME_TYPE_SPEC(name_type_spec_node) => {
                let core_name_type_spec = name_type_spec_node.core_ref();
                self.walk_identifier(&core_name_type_spec.name);
                self.walk_token(&core_name_type_spec.colon);
                self.walk_type_expression(&core_name_type_spec.data_type);
            }
            ASTNode::TYPE_TUPLE(type_tuple_node) => {
                let core_type_tuple = type_tuple_node.core_ref();
                match core_type_tuple {
                    CoreTypeTupleNode::OK(ok_type_tuple) => {
                        self.walk_ok_type_tuple(ok_type_tuple);
                    }
                    CoreTypeTupleNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            }
            ASTNode::OK_TYPE_TUPLE(ok_type_tuple_node) => {
                let core_ok_type_tuple = ok_type_tuple_node.core_ref();
                self.walk_type_expression(&core_ok_type_tuple.data_type);
                if let Some(comma) = &core_ok_type_tuple.comma {
                    self.walk_token(comma);
                }
                if let Some(remaining_types) = &core_ok_type_tuple.remaining_types {
                    self.walk_type_tuple(remaining_types);
                }
            }
            ASTNode::TYPE_EXPRESSION(type_expression_node) => {
                let core_type_expr = type_expression_node.core_ref();
                match core_type_expr {
                    CoreTypeExpressionNode::ATOMIC(atomic_type) => {
                        self.walk_atomic_type(atomic_type);
                    }
                    CoreTypeExpressionNode::USER_DEFINED(user_defined_type) => {
                        self.walk_user_defined_type(user_defined_type);
                    }
                    CoreTypeExpressionNode::ARRAY(array_type) => {
                        self.walk_array_type(array_type);
                    }
                    CoreTypeExpressionNode::TUPLE(tuple_type) => {
                        self.walk_tuple_type(tuple_type);
                    }
                    CoreTypeExpressionNode::HASHMAP(hashmap_type) => {
                        self.walk_hashmap_type(hashmap_type);
                    }
                    CoreTypeExpressionNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            }
            ASTNode::ATOMIC_TYPE(atomic_type_node) => {
                let core_atomic_type = atomic_type_node.core_ref();
                self.walk_token(&core_atomic_type.kind)
            }
            ASTNode::ARRAY_TYPE(array_type_node) => {
                let core_array_type = array_type_node.core_ref();
                self.walk_token(&core_array_type.lsquare);
                self.walk_type_expression(&core_array_type.sub_type);
                self.walk_token(&core_array_type.rsquare);
            }
            ASTNode::TUPLE_TYPE(tuple_type_node) => {
                let core_tuple_type = tuple_type_node.core_ref();
                self.walk_token(&core_tuple_type.lparen);
                self.walk_type_tuple(&core_tuple_type.types);
                self.walk_token(&core_tuple_type.rparen);
            }
            ASTNode::HASHMAP_TYPE(hashmap_type_node) => {
                let core_hashmap_type_node = hashmap_type_node.core_ref();
                self.walk_token(&core_hashmap_type_node.lcurly);
                self.walk_type_expression(&core_hashmap_type_node.key_type);
                self.walk_token(&core_hashmap_type_node.colon);
                self.walk_type_expression(&core_hashmap_type_node.value_type);
                self.walk_token(&core_hashmap_type_node.rcurly);
            }
            ASTNode::USER_DEFINED_TYPE(user_defined_type) => {
                let core_user_defined_type = user_defined_type.core_ref();
                self.walk_identifier(&core_user_defined_type.name)
            }
            ASTNode::EXPRESSION(expression_node) => {
                let core_expr = expression_node.core_ref();
                match core_expr {
                    CoreExpressionNode::UNARY(unary_expr) => {
                        self.walk_unary_expression(unary_expr);
                    }
                    CoreExpressionNode::BINARY(binary_expr) => {
                        self.walk_binary_expression(binary_expr);
                    }
                    CoreExpressionNode::COMPARISON(comparison_expr) => {
                        self.walk_comparison(comparison_expr);
                    }
                    CoreExpressionNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            }
            ASTNode::ATOMIC_EXPRESSION(atomic_expression_node) => {
                let core_atomic_expr = atomic_expression_node.core_ref();
                match core_atomic_expr {
                    CoreAtomicExpressionNode::BOOL_VALUE(token) => {
                        self.walk_token(token);
                    }
                    CoreAtomicExpressionNode::INTEGER(token) => {
                        self.walk_token(token);
                    }
                    CoreAtomicExpressionNode::FLOATING_POINT_NUMBER(token) => {
                        self.walk_token(token);
                    }
                    CoreAtomicExpressionNode::LITERAL(token) => {
                        self.walk_token(token);
                    }
                    CoreAtomicExpressionNode::PARENTHESISED_EXPRESSION(parenthesised_expr) => {
                        self.walk_parenthesised_expression(parenthesised_expr);
                    }
                    CoreAtomicExpressionNode::ATOM(atom) => {
                        self.walk_atom(atom);
                    }
                    CoreAtomicExpressionNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens)
                    }
                }
            }
            ASTNode::PARENTHESISED_EXPRESSION(parenthesised_expression_node) => {
                let parenthesised_expr = parenthesised_expression_node.core_ref();
                self.walk_token(&parenthesised_expr.lparen);
                self.walk_expression(&parenthesised_expr.expr);
                self.walk_token(&parenthesised_expr.rparen);
            }
            ASTNode::UNARY_EXPRESSION(unary_expression_node) => {
                let core_unary_expr = unary_expression_node.core_ref();
                match core_unary_expr {
                    CoreUnaryExpressionNode::ATOMIC(atomic) => {
                        self.walk_atomic_expression(atomic);
                    }
                    CoreUnaryExpressionNode::UNARY(unary) => {
                        self.walk_only_unary_expression(unary);
                    }
                    CoreUnaryExpressionNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            }
            ASTNode::ONLY_UNARY_EXPRESSION(only_unary_expression_node) => {
                let core_only_unary_expr = only_unary_expression_node.core_ref();
                self.walk_token(&core_only_unary_expr.operator);
                self.walk_unary_expression(&core_only_unary_expr.unary_expr);
            }
            ASTNode::BINARY_EXPRESSION(binary_expression_node) => {
                let core_binary_expr = binary_expression_node.core_ref();
                self.walk_expression(&core_binary_expr.left_expr);
                self.walk_token(&core_binary_expr.operator);
                self.walk_expression(&core_binary_expr.right_expr);
            }
            ASTNode::COMPARISON(comparison_expression_node) => {
                let core_comp_expr = comparison_expression_node.core_ref();
                let operator_len = core_comp_expr.operators.len();
                self.walk_expression(&core_comp_expr.operands[0]);
                for i in 0..operator_len {
                    self.walk_token(&core_comp_expr.operators[i]);
                    self.walk_expression(&core_comp_expr.operands[i + 1]);
                }
            }
            ASTNode::PARAMS(params_node) => {
                let core_params = params_node.core_ref();
                match core_params {
                    CoreParamsNode::OK(ok_params) => {
                        self.walk_ok_params(ok_params);
                    }
                    CoreParamsNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            }
            ASTNode::OK_PARAMS(ok_params_node) => {
                let core_ok_params = ok_params_node.core_ref();
                self.walk_expression(&core_ok_params.param);
                if let Some(comma) = &core_ok_params.comma {
                    self.walk_token(comma);
                }
                if let Some(remaining_params) = &core_ok_params.remaining_params {
                    self.walk_params(remaining_params);
                }
            }
            ASTNode::CALL_EXPRESSION(call_expression_node) => {
                let core_call_expr = call_expression_node.core_ref();
                self.walk_identifier(&core_call_expr.function_name);
                self.walk_token(&core_call_expr.lparen);
                if let Some(params) = &core_call_expr.params {
                    self.walk_params(params)
                }
                self.walk_token(&core_call_expr.rparen);
            }
            ASTNode::CLASS_METHOD_CALL(class_method_call_node) => {
                let core_class_method_call = class_method_call_node.core_ref();
                self.walk_identifier(&core_class_method_call.class_name);
                self.walk_token(&core_class_method_call.double_colon);
                self.walk_identifier(&core_class_method_call.class_method_name);
                self.walk_token(&core_class_method_call.lparen);
                if let Some(params) = &core_class_method_call.params {
                    self.walk_params(params);
                }
                self.walk_token(&core_class_method_call.rparen);
            }
            ASTNode::ATOM(atom_node) => {
                let core_atom = atom_node.core_ref();
                match core_atom {
                    CoreAtomNode::ATOM_START(atom_start) => {
                        self.walk_atom_start(atom_start);
                    }
                    CoreAtomNode::CALL(call_node) => {
                        self.walk_call(call_node);
                    }
                    CoreAtomNode::PROPERTRY_ACCESS(property_access) => {
                        self.walk_property_access(property_access);
                    }
                    CoreAtomNode::METHOD_ACCESS(method_access) => {
                        self.walk_method_access(method_access);
                    }
                    CoreAtomNode::INDEX_ACCESS(index_access) => {
                        self.walk_index_access(index_access);
                    }
                }
            }
            ASTNode::ATOM_START(atom_start_node) => {
                let core_atom_start = atom_start_node.core_ref();
                match core_atom_start {
                    CoreAtomStartNode::IDENTIFIER(token) => {
                        self.walk_identifier(token);
                    }
                    CoreAtomStartNode::SELF_KEYWORD(self_keyword) => {
                        self.walk_self_keyword(self_keyword);
                    }
                    CoreAtomStartNode::CALL(call_expr) => {
                        self.walk_call_expression(call_expr);
                    }
                    CoreAtomStartNode::CLASS_METHOD_CALL(class_method) => {
                        self.walk_class_method_call(class_method);
                    }
                }
            }
            ASTNode::CALL(call_node) => {
                let core_call = call_node.core_ref();
                self.walk_atom(&core_call.atom);
                self.walk_token(&core_call.lparen);
                if let Some(params) = &core_call.params {
                    self.walk_params(params);
                }
                self.walk_token(&core_call.rparen);
            }
            ASTNode::PROPERTY_ACCESS(property_access_node) => {
                let core_property_access = property_access_node.core_ref();
                self.walk_atom(&core_property_access.atom);
                self.walk_token(&core_property_access.dot);
                self.walk_identifier(&core_property_access.propertry);
            }
            ASTNode::METHOD_ACCESS(method_access_node) => {
                let core_method_access = method_access_node.core_ref();
                self.walk_atom(&core_method_access.atom);
                self.walk_token(&core_method_access.dot);
                self.walk_identifier(&core_method_access.method_name);
                self.walk_token(&core_method_access.lparen);
                if let Some(params) = &core_method_access.params {
                    self.walk_params(params);
                }
                self.walk_token(&core_method_access.rparen);
            }
            ASTNode::INDEX_ACCESS(index_access_node) => {
                let core_index_access = index_access_node.core_ref();
                self.walk_atom(&core_index_access.atom);
                self.walk_token(&core_index_access.lsquare);
                self.walk_expression(&core_index_access.index);
                self.walk_token(&core_index_access.rsquare);
            }
            ASTNode::TOKEN(token) => {
                let token = token.core_ref();
                match token {
                    CoreTokenNode::OK(ok_token) => self.walk_ok_token(ok_token),
                    CoreTokenNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens)
                    }
                }
            }
            ASTNode::IDENTIFIER(identifier) => {
                let core_identifier = identifier.core_ref();
                match core_identifier {
                    CoreIdentifierNode::OK(ok_identifier) => self.walk_ok_identifier(ok_identifier),
                    CoreIdentifierNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens)
                    }
                }
            }
            ASTNode::OK_IDENTIFIER(ok_identifier) => {
                self.walk_ok_token(&ok_identifier.0.as_ref().borrow().token);
            }
            ASTNode::SELF_KEYWORD(self_keyword) => {
                let core_self_keyword = self_keyword.core_ref();
                match core_self_keyword {
                    CoreSelfKeywordNode::OK(ok_self_keyword) => {
                        self.walk_ok_self_keyword(ok_self_keyword)
                    }
                    CoreSelfKeywordNode::MISSING_TOKENS(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens)
                    }
                }
            }
            ASTNode::OK_SELF_KEYWORD(ok_self_keyword) => {
                self.walk_ok_token(&ok_self_keyword.0.as_ref().borrow().token);
            }
            ASTNode::OK_TOKEN(_) => {
                // do nothing
            }
            ASTNode::MISSING_TOKEN(_) => {
                // do nothing
            }
            ASTNode::SKIPPED_TOKEN(_) => {
                // do nothing
            }
        }
    }
}