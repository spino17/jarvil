use super::ast::{
    CoreIdentifierInDeclNode, CoreIdentifierInUseNode, GenericTypeDeclNode, IdentifierInDeclNode,
    IdentifierInUseNode, InterfaceDeclarationNode, InterfaceMethodPrototypeWrapperNode,
    InterfaceMethodTerminalNode, OkIdentifierInDeclNode, OkIdentifierInUseNode,
    SymbolSeparatedSequenceNode, TupleTypeNode,
};
use crate::ast::ast::{
    ASTNode, ArrayTypeNode, AssignmentNode, AtomNode, AtomStartNode, AtomicExpressionNode,
    AtomicTypeNode, BinaryExpressionNode, BlockNode, BoundedMethodWrapperNode, CallExpressionNode,
    CallNode, CallableBodyNode, CallablePrototypeNode, ClassMethodCallNode, ComparisonNode,
    CoreAssignmentNode, CoreAtomNode, CoreAtomStartNode, CoreAtomicExpressionNode,
    CoreExpressionNode, CoreRVariableDeclarationNode, CoreSelfKeywordNode,
    CoreStatemenIndentWrapperNode, CoreStatementNode, CoreTokenNode, CoreTypeDeclarationNode,
    CoreTypeExpressionNode, CoreUnaryExpressionNode, ExpressionNode, ExpressionStatementNode,
    FunctionDeclarationNode, FunctionWrapperNode, HashMapTypeNode,
    IncorrectlyIndentedStatementNode, IndexAccessNode, InvalidLValueNode, LambdaDeclarationNode,
    LambdaTypeDeclarationNode, MethodAccessNode, MissingTokenNode, NameTypeSpecNode,
    OkAssignmentNode, OkSelfKeywordNode, OkTokenNode, OnlyUnaryExpressionNode,
    ParenthesisedExpressionNode, PropertyAccessNode, RAssignmentNode, RVariableDeclarationNode,
    ReturnStatementNode, SelfKeywordNode, SkippedTokenNode, SkippedTokensNode,
    StatemenIndentWrapperNode, StatementNode, StructDeclarationNode, StructPropertyDeclarationNode,
    TokenNode, TypeDeclarationNode, TypeExpressionNode, UnaryExpressionNode, UserDefinedTypeNode,
    VariableDeclarationNode,
};

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
        walk_interface_decl,
        InterfaceDeclarationNode,
        new_with_InterfaceDeclarationNode
    );
    impl_node_walk!(
        walk_interface_method_prototype_wrapper,
        InterfaceMethodPrototypeWrapperNode,
        new_with_InterfaceMethodPrototypeWrapperNode
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
        walk_identifier_in_use,
        IdentifierInUseNode,
        new_with_IdentifierInUseNode
    );
    impl_node_walk!(
        walk_identifier_in_decl,
        IdentifierInDeclNode,
        new_with_IdentifierInDeclNode
    );
    impl_node_walk!(
        walk_ok_identifier_in_use,
        OkIdentifierInUseNode,
        new_with_OkIdentifierInUseNode
    );
    impl_node_walk!(
        walk_ok_identifier_in_decl,
        OkIdentifierInDeclNode,
        new_with_OkIdentifierInDeclNode
    );
    impl_node_walk!(
        walk_generic_type_decl,
        GenericTypeDeclNode,
        new_with_GenericTypeDeclNode
    );
    impl_node_walk!(walk_self_keyword, SelfKeywordNode, new_with_SelfKeywordNode);
    impl_node_walk!(
        walk_ok_self_keyword,
        OkSelfKeywordNode,
        new_with_OkSelfKeywordNode
    );
    fn walk_type_tuple(&mut self, x: &SymbolSeparatedSequenceNode<TypeExpressionNode>) {
        self.walk(&ASTNode::new_with_TypeTuple(x));
    }
    fn walk_name_type_specs(&mut self, x: &SymbolSeparatedSequenceNode<NameTypeSpecNode>) {
        self.walk(&ASTNode::new_with_NameTypeSpecs(x));
    }
    fn walk_params(&mut self, x: &SymbolSeparatedSequenceNode<ExpressionNode>) {
        self.walk(&ASTNode::new_with_Params(x));
    }
    fn walk_generic_type_decls(&mut self, x: &SymbolSeparatedSequenceNode<GenericTypeDeclNode>) {
        let core_generic_type_decls = x.core_ref();
        self.walk_generic_type_decl(&core_generic_type_decls.entity);
        if let Some((comma, remaining_entities)) = &core_generic_type_decls.remaining_entities {
            self.walk_token(comma);
            self.walk_generic_type_decls(remaining_entities);
        }
    }
    fn walk_interface_bounds(&mut self, x: &SymbolSeparatedSequenceNode<IdentifierInUseNode>) {
        let core_interface_bounds = x.core_ref();
        self.walk_identifier_in_use(&core_interface_bounds.entity);
        if let Some((comma, remaining_entities)) = &core_interface_bounds.remaining_entities {
            self.walk_token(comma);
            self.walk_interface_bounds(remaining_entities);
        }
    }

    fn walk(&mut self, node: &ASTNode) {
        match self.visit(node) {
            None => return,
            _ => {}
        }

        match node {
            ASTNode::Block(block_node) => {
                let core_block_node = &block_node.0.as_ref();
                self.walk_token(&core_block_node.newline);
                for stmt in &*core_block_node.stmts.as_ref() {
                    self.walk_stmt_indent_wrapper(stmt);
                }
            }
            ASTNode::StatementIndentWrapper(stmt_indent_wrapper_node) => {
                match stmt_indent_wrapper_node.core_ref() {
                    CoreStatemenIndentWrapperNode::CorrectlyIndented(stmt) => {
                        self.walk_stmt(stmt);
                    }
                    CoreStatemenIndentWrapperNode::IncorrectlyIndented(stmt) => {
                        self.walk_incorrectly_indented_stmt(stmt);
                    }
                    CoreStatemenIndentWrapperNode::LeadingSkippedTokens(skipped_tokens) => {
                        self.walk_skipped_tokens(skipped_tokens);
                    }
                    CoreStatemenIndentWrapperNode::TrailingSkippedTokens(skipped_tokens) => {
                        self.walk_skipped_tokens(skipped_tokens);
                    }
                    CoreStatemenIndentWrapperNode::ExtraNewlines(skipped_tokens) => {
                        self.walk_skipped_tokens(skipped_tokens);
                    }
                }
            }
            ASTNode::SkippedTokens(skipped_tokens) => {
                for skipped_token in &skipped_tokens.core_ref().skipped_tokens {
                    self.walk_skipped_token(skipped_token);
                }
            }
            ASTNode::IncorrectlyIndentedStatement(stmt) => {
                let core_stmt = stmt.core_ref();
                self.walk_stmt(&core_stmt.stmt);
            }
            ASTNode::Statement(statement_node) => match statement_node.core_ref() {
                CoreStatementNode::Expression(expr_stmt) => {
                    self.walk_expr_stmt(expr_stmt);
                }
                CoreStatementNode::Assignment(assignment) => {
                    self.walk_assignment(assignment);
                }
                CoreStatementNode::VariableDeclaration(variable_decl) => {
                    self.walk_variable_decl(variable_decl);
                }
                CoreStatementNode::FunctionWrapper(func_wrapper) => {
                    self.walk_func_wrapper(func_wrapper);
                }
                CoreStatementNode::BoundedMethodWrapper(bounded_method_wrapper) => {
                    self.walk_bounded_method_wrapper(bounded_method_wrapper);
                }
                CoreStatementNode::TypeDeclaration(type_decl) => {
                    self.walk_type_decl(type_decl);
                }
                CoreStatementNode::StructPropertyDeclaration(struct_stmt) => {
                    self.walk_struct_property_declaration(struct_stmt);
                }
                CoreStatementNode::InterfaceDeclaration(interface_decl) => {
                    self.walk_interface_decl(interface_decl);
                }
                CoreStatementNode::InterfaceMethodPrototypeWrapper(
                    interface_method_prototype_wrapper,
                ) => {
                    self.walk_interface_method_prototype_wrapper(
                        interface_method_prototype_wrapper,
                    );
                }
                CoreStatementNode::Return(return_stmt) => {
                    self.walk_return_stmt(return_stmt);
                }
            },
            ASTNode::ExpressionStatement(expr_stmt) => {
                let core_expr_stmt = expr_stmt.core_ref();
                self.walk_expression(&core_expr_stmt.expr);
                self.walk_token(&core_expr_stmt.newline);
            }
            ASTNode::Assignment(assignment_node) => match assignment_node.core_ref() {
                CoreAssignmentNode::Ok(ok_assignment) => {
                    self.walk_ok_assignment(ok_assignment);
                }
                CoreAssignmentNode::InvalidLValue(invalid_l_value_assignment) => {
                    self.walk_invalid_l_value_assignment(invalid_l_value_assignment);
                }
            },
            ASTNode::OkAssignment(ok_assignment) => {
                let core_ok_assignment = ok_assignment.core_ref();
                self.walk_atom(&core_ok_assignment.l_atom);
                self.walk_token(&core_ok_assignment.equal);
                self.walk_r_assignment(&core_ok_assignment.r_assign);
            }
            ASTNode::InvalidLValue(invalid_l_value) => {
                let core_invalid_l_value = invalid_l_value.core_ref();
                self.walk_expression(&core_invalid_l_value.l_expr);
                self.walk_token(&core_invalid_l_value.equal);
                self.walk_r_assignment(&core_invalid_l_value.r_assign);
            }
            ASTNode::StructPropertyDeclaration(struct_statement) => {
                let core_struct_stmt = struct_statement.core_ref();
                self.walk_name_type_spec(&core_struct_stmt.name_type_spec);
                self.walk_token(&core_struct_stmt.newline);
            }
            ASTNode::TypeDeclaration(type_decl_node) => match &type_decl_node.core_ref() {
                CoreTypeDeclarationNode::Struct(struct_decl) => {
                    self.walk_struct_decl(struct_decl);
                }
                CoreTypeDeclarationNode::Lambda(lambda_decl) => {
                    self.walk_lambda_type_decl(lambda_decl);
                }
                CoreTypeDeclarationNode::MissingTokens(missing_tokens) => {
                    self.walk_missing_tokens(missing_tokens);
                }
            },
            ASTNode::InterfaceDeclaration(interface_decl_node) => {
                let core_interface_decl = interface_decl_node.core_ref();
                self.walk_token(&core_interface_decl.interface_keyword);
                self.walk_identifier_in_decl(&core_interface_decl.name);
                self.walk_token(&core_interface_decl.colon);
                self.walk_block(&core_interface_decl.block);
            }
            ASTNode::InterfaceMethodPrototypeWrapper(interface_method_prototype_wrapper_node) => {
                let core_interface_method_prototype_wrapper =
                    interface_method_prototype_wrapper_node.core_ref();
                self.walk_token(&core_interface_method_prototype_wrapper.def_keyword);
                self.walk_identifier_in_decl(&core_interface_method_prototype_wrapper.name);
                self.walk_callable_prototype(&core_interface_method_prototype_wrapper.prototype);
                match &core_interface_method_prototype_wrapper.terminal {
                    InterfaceMethodTerminalNode::HasDefaultBody(colon, block) => {
                        self.walk_token(colon);
                        self.walk_block(block);
                    }
                    InterfaceMethodTerminalNode::NoDefaultBody(newline) => self.walk_token(newline),
                }
            }
            ASTNode::StructDeclaration(struct_decl_node) => {
                let core_struct_decl = struct_decl_node.core_ref();
                self.walk_token(&core_struct_decl.type_keyword);
                self.walk_identifier_in_decl(&core_struct_decl.name);
                self.walk_token(&core_struct_decl.struct_keyword);
                self.walk_token(&core_struct_decl.colon);
                self.walk_block(&core_struct_decl.block);
            }
            ASTNode::LambdaTypeDeclaration(lambda_decl_node) => {
                let core_lambda_decl = lambda_decl_node.core_ref();
                self.walk_token(&core_lambda_decl.type_keyword);
                self.walk_identifier_in_decl(&core_lambda_decl.name);
                self.walk_token(&core_lambda_decl.lambda_keyword);
                self.walk_token(&core_lambda_decl.equal);
                self.walk_token(&core_lambda_decl.lparen);
                if let Some(type_tuple) = &core_lambda_decl.type_tuple {
                    self.walk_type_tuple(type_tuple);
                }
                self.walk_token(&core_lambda_decl.rparen);
                if let Some(right_arrow) = &core_lambda_decl.right_arrow {
                    self.walk_token(right_arrow);
                }
                if let Some(return_type) = &core_lambda_decl.return_type {
                    self.walk_type_expression(return_type);
                }
                self.walk_token(&core_lambda_decl.newline);
            }
            ASTNode::CallablePrototype(callable_prototype) => {
                let core_callable_prototype = callable_prototype.core_ref();
                self.walk_token(&core_callable_prototype.lparen);
                if let Some(name_type_specs) = &core_callable_prototype.params {
                    self.walk_name_type_specs(name_type_specs);
                }
                self.walk_token(&core_callable_prototype.rparen);
                if let Some((right_arrow, return_type)) = &core_callable_prototype.return_type {
                    self.walk_token(right_arrow);
                    self.walk_type_expression(return_type);
                }
            }
            ASTNode::CallableBody(callable_body) => {
                let core_callable_body = callable_body.core_ref();
                self.walk_callable_prototype(&core_callable_body.prototype);
                self.walk_token(&core_callable_body.colon);
                self.walk_block(&core_callable_body.block);
            }
            ASTNode::LambdaDeclaration(lambda_decl_node) => {
                let core_lambda_decl_node = lambda_decl_node.core_ref();
                self.walk_token(&core_lambda_decl_node.lambda_keyword);
                self.walk_callable_body(&core_lambda_decl_node.body);
            }
            ASTNode::FunctionDeclaration(function_decl_node) => {
                let core_func_decl = function_decl_node.core_ref();
                self.walk_token(&core_func_decl.def_keyword);
                self.walk_identifier_in_decl(&core_func_decl.name);
                self.walk_callable_body(&core_func_decl.body);
            }
            ASTNode::FunctionWrapper(func_wrapper) => {
                self.walk_func_decl(&func_wrapper.core_ref().func_decl);
            }
            ASTNode::BoundedMethodWrapper(bounded_method_wrapper) => {
                self.walk_func_decl(&bounded_method_wrapper.0.as_ref().func_decl);
            }
            ASTNode::VariableDeclaration(variable_decl_node) => {
                let core_variable_decl = variable_decl_node.core_ref();
                self.walk_token(&core_variable_decl.let_keyword);
                self.walk_identifier_in_decl(&core_variable_decl.name);
                self.walk_token(&core_variable_decl.equal);
                self.walk_r_variable_declaration(&core_variable_decl.r_node);
            }
            ASTNode::Return(return_stmt) => {
                let core_return_stmt = return_stmt.core_ref();
                self.walk_token(&core_return_stmt.return_keyword);
                if let Some(expr) = &core_return_stmt.expr {
                    self.walk_expression(expr);
                }
                self.walk_token(&core_return_stmt.newline);
            }
            ASTNode::RAssignment(r_assignment_node) => {
                let core_r_assignment = r_assignment_node.core_ref();
                self.walk_expr_stmt(&core_r_assignment.expr);
            }
            ASTNode::RVariableDeclaration(r_variable_decl) => {
                let core_r_variable_decl = r_variable_decl.core_ref();
                match core_r_variable_decl {
                    CoreRVariableDeclarationNode::Expression(expr_stmt) => {
                        self.walk_expr_stmt(expr_stmt);
                    }
                    CoreRVariableDeclarationNode::Lambda(lambda) => {
                        self.walk_lambda_decl(lambda);
                    }
                }
            }
            ASTNode::NameTypeSpecs(name_type_specs_node) => {
                let core_name_type_specs = name_type_specs_node.core_ref();
                self.walk_name_type_spec(&core_name_type_specs.entity);
                if let Some((comma, remaining_args)) = &core_name_type_specs.remaining_entities {
                    self.walk_token(comma);
                    self.walk_name_type_specs(remaining_args);
                }
            }
            ASTNode::NameTypeSpec(name_type_spec_node) => {
                let core_name_type_spec = name_type_spec_node.core_ref();
                self.walk_identifier_in_decl(&core_name_type_spec.name);
                self.walk_token(&core_name_type_spec.colon);
                self.walk_type_expression(&core_name_type_spec.data_type);
            }
            ASTNode::TypeTuple(type_tuple_node) => {
                let core_type_tuple = type_tuple_node.core_ref();
                self.walk_type_expression(&core_type_tuple.entity);
                if let Some((comma, remaining_types)) = &core_type_tuple.remaining_entities {
                    self.walk_token(comma);
                    self.walk_type_tuple(remaining_types);
                }
            }
            ASTNode::TypeExpression(type_expression_node) => {
                let core_type_expr = type_expression_node.core_ref();
                match core_type_expr {
                    CoreTypeExpressionNode::Atomic(atomic_type) => {
                        self.walk_atomic_type(atomic_type);
                    }
                    CoreTypeExpressionNode::UserDefined(user_defined_type) => {
                        self.walk_user_defined_type(user_defined_type);
                    }
                    CoreTypeExpressionNode::Array(array_type) => {
                        self.walk_array_type(array_type);
                    }
                    CoreTypeExpressionNode::Tuple(tuple_type) => {
                        self.walk_tuple_type(tuple_type);
                    }
                    CoreTypeExpressionNode::HashMap(hashmap_type) => {
                        self.walk_hashmap_type(hashmap_type);
                    }
                    CoreTypeExpressionNode::MissingTokens(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens);
                    }
                }
            }
            ASTNode::AtomicType(atomic_type_node) => {
                let core_atomic_type = atomic_type_node.core_ref();
                self.walk_token(&core_atomic_type.kind)
            }
            ASTNode::ArrayType(array_type_node) => {
                let core_array_type = array_type_node.core_ref();
                self.walk_token(&core_array_type.lsquare);
                self.walk_type_expression(&core_array_type.sub_type);
                self.walk_token(&core_array_type.rsquare);
            }
            ASTNode::TupleType(tuple_type_node) => {
                let core_tuple_type = tuple_type_node.core_ref();
                self.walk_token(&core_tuple_type.lparen);
                self.walk_type_tuple(&core_tuple_type.types);
                self.walk_token(&core_tuple_type.rparen);
            }
            ASTNode::HashmapType(hashmap_type_node) => {
                let core_hashmap_type_node = hashmap_type_node.core_ref();
                self.walk_token(&core_hashmap_type_node.lcurly);
                self.walk_type_expression(&core_hashmap_type_node.key_type);
                self.walk_token(&core_hashmap_type_node.colon);
                self.walk_type_expression(&core_hashmap_type_node.value_type);
                self.walk_token(&core_hashmap_type_node.rcurly);
            }
            ASTNode::UserDefinedType(user_defined_type) => {
                let core_user_defined_type = user_defined_type.core_ref();
                self.walk_identifier_in_use(&core_user_defined_type.name)
            }
            ASTNode::Expression(expression_node) => {
                let core_expr = expression_node.core_ref();
                match core_expr {
                    CoreExpressionNode::Unary(unary_expr) => {
                        self.walk_unary_expression(unary_expr);
                    }
                    CoreExpressionNode::Binary(binary_expr) => {
                        self.walk_binary_expression(binary_expr);
                    }
                    CoreExpressionNode::Comparison(comparison_expr) => {
                        self.walk_comparison(comparison_expr);
                    }
                }
            }
            ASTNode::AtomicExpression(atomic_expression_node) => {
                let core_atomic_expr = atomic_expression_node.core_ref();
                match core_atomic_expr {
                    CoreAtomicExpressionNode::Bool(token) => {
                        self.walk_token(token);
                    }
                    CoreAtomicExpressionNode::Integer(token) => {
                        self.walk_token(token);
                    }
                    CoreAtomicExpressionNode::FloatingPointNumber(token) => {
                        self.walk_token(token);
                    }
                    CoreAtomicExpressionNode::Literal(token) => {
                        self.walk_token(token);
                    }
                    CoreAtomicExpressionNode::ParenthesisedExpression(parenthesised_expr) => {
                        self.walk_parenthesised_expression(parenthesised_expr);
                    }
                    CoreAtomicExpressionNode::Atom(atom) => {
                        self.walk_atom(atom);
                    }
                    CoreAtomicExpressionNode::MissingTokens(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens)
                    }
                }
            }
            ASTNode::ParenthesisedExpression(parenthesised_expression_node) => {
                let parenthesised_expr = parenthesised_expression_node.core_ref();
                self.walk_token(&parenthesised_expr.lparen);
                self.walk_expression(&parenthesised_expr.expr);
                self.walk_token(&parenthesised_expr.rparen);
            }
            ASTNode::UnaryExpression(unary_expression_node) => {
                let core_unary_expr = unary_expression_node.core_ref();
                match core_unary_expr {
                    CoreUnaryExpressionNode::Atomic(atomic) => {
                        self.walk_atomic_expression(atomic);
                    }
                    CoreUnaryExpressionNode::Unary(unary) => {
                        self.walk_only_unary_expression(unary);
                    }
                }
            }
            ASTNode::OnlyUnaryExpression(only_unary_expression_node) => {
                let core_only_unary_expr = only_unary_expression_node.core_ref();
                self.walk_token(&core_only_unary_expr.operator);
                self.walk_unary_expression(&core_only_unary_expr.unary_expr);
            }
            ASTNode::BinaryExpression(binary_expression_node) => {
                let core_binary_expr = binary_expression_node.core_ref();
                self.walk_expression(&core_binary_expr.left_expr);
                self.walk_token(&core_binary_expr.operator);
                self.walk_expression(&core_binary_expr.right_expr);
            }
            ASTNode::Comparison(comparison_expression_node) => {
                let core_comp_expr = comparison_expression_node.core_ref();
                let operator_len = core_comp_expr.operators.len();
                self.walk_expression(&core_comp_expr.operands[0]);
                for i in 0..operator_len {
                    self.walk_token(&core_comp_expr.operators[i]);
                    self.walk_expression(&core_comp_expr.operands[i + 1]);
                }
            }
            ASTNode::Params(params_node) => {
                let core_params = params_node.core_ref();
                self.walk_expression(&core_params.entity);
                if let Some((comma, remaining_params)) = &core_params.remaining_entities {
                    self.walk_token(comma);
                    self.walk_params(remaining_params);
                }
            }
            ASTNode::CallExpression(call_expression_node) => {
                let core_call_expr = call_expression_node.core_ref();
                self.walk_identifier_in_use(&core_call_expr.function_name);
                self.walk_token(&core_call_expr.lparen);
                if let Some(params) = &core_call_expr.params {
                    self.walk_params(params)
                }
                self.walk_token(&core_call_expr.rparen);
            }
            ASTNode::ClassMethodCall(class_method_call_node) => {
                let core_class_method_call = class_method_call_node.core_ref();
                self.walk_identifier_in_use(&core_class_method_call.class_name);
                self.walk_token(&core_class_method_call.double_colon);
                self.walk_identifier_in_use(&core_class_method_call.class_method_name);
                self.walk_token(&core_class_method_call.lparen);
                if let Some(params) = &core_class_method_call.params {
                    self.walk_params(params);
                }
                self.walk_token(&core_class_method_call.rparen);
            }
            ASTNode::Atom(atom_node) => {
                let core_atom = atom_node.core_ref();
                match core_atom {
                    CoreAtomNode::AtomStart(atom_start) => {
                        self.walk_atom_start(atom_start);
                    }
                    CoreAtomNode::Call(call_node) => {
                        self.walk_call(call_node);
                    }
                    CoreAtomNode::PropertyAccess(property_access) => {
                        self.walk_property_access(property_access);
                    }
                    CoreAtomNode::MethodAccess(method_access) => {
                        self.walk_method_access(method_access);
                    }
                    CoreAtomNode::IndexAccess(index_access) => {
                        self.walk_index_access(index_access);
                    }
                }
            }
            ASTNode::AtomStart(atom_start_node) => {
                let core_atom_start = atom_start_node.core_ref();
                match core_atom_start {
                    CoreAtomStartNode::Identifier(token) => {
                        self.walk_identifier_in_use(token);
                    }
                    CoreAtomStartNode::SelfKeyword(self_keyword) => {
                        self.walk_self_keyword(self_keyword);
                    }
                    CoreAtomStartNode::Call(call_expr) => {
                        self.walk_call_expression(call_expr);
                    }
                    CoreAtomStartNode::ClassMethodCall(class_method) => {
                        self.walk_class_method_call(class_method);
                    }
                }
            }
            ASTNode::Call(call_node) => {
                let core_call = call_node.core_ref();
                self.walk_atom(&core_call.atom);
                self.walk_token(&core_call.lparen);
                if let Some(params) = &core_call.params {
                    self.walk_params(params);
                }
                self.walk_token(&core_call.rparen);
            }
            ASTNode::PropertyAccess(property_access_node) => {
                let core_property_access = property_access_node.core_ref();
                self.walk_atom(&core_property_access.atom);
                self.walk_token(&core_property_access.dot);
                self.walk_identifier_in_use(&core_property_access.propertry);
            }
            ASTNode::MethodAccess(method_access_node) => {
                let core_method_access = method_access_node.core_ref();
                self.walk_atom(&core_method_access.atom);
                self.walk_token(&core_method_access.dot);
                self.walk_identifier_in_use(&core_method_access.method_name);
                self.walk_token(&core_method_access.lparen);
                if let Some(params) = &core_method_access.params {
                    self.walk_params(params);
                }
                self.walk_token(&core_method_access.rparen);
            }
            ASTNode::IndexAccess(index_access_node) => {
                let core_index_access = index_access_node.core_ref();
                self.walk_atom(&core_index_access.atom);
                self.walk_token(&core_index_access.lsquare);
                self.walk_expression(&core_index_access.index);
                self.walk_token(&core_index_access.rsquare);
            }
            ASTNode::Token(token) => {
                let token = token.core_ref();
                match token {
                    CoreTokenNode::Ok(ok_token) => self.walk_ok_token(ok_token),
                    CoreTokenNode::MissingTokens(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens)
                    }
                }
            }
            ASTNode::SelfKeyword(self_keyword) => {
                let core_self_keyword = self_keyword.core_ref();
                match core_self_keyword {
                    CoreSelfKeywordNode::Ok(ok_self_keyword) => {
                        self.walk_ok_self_keyword(ok_self_keyword)
                    }
                    CoreSelfKeywordNode::MissingTokens(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens)
                    }
                }
            }
            ASTNode::IdentifierInUse(identifier_in_use) => {
                let core_identifier_in_use = identifier_in_use.core_ref();
                match core_identifier_in_use {
                    CoreIdentifierInUseNode::Ok(ok_identifier) => {
                        self.walk_ok_identifier_in_use(ok_identifier)
                    }
                    CoreIdentifierInUseNode::MissingTokens(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens)
                    }
                }
            }
            ASTNode::IdentifierInDecl(identifier_in_decl) => {
                let core_identifier_in_decl = identifier_in_decl.core_ref();
                match core_identifier_in_decl {
                    CoreIdentifierInDeclNode::Ok(ok_identifier) => {
                        self.walk_ok_identifier_in_decl(ok_identifier)
                    }
                    CoreIdentifierInDeclNode::MissingTokens(missing_tokens) => {
                        self.walk_missing_tokens(missing_tokens)
                    }
                }
            }
            ASTNode::OkIdentifierInUse(ok_identifier_in_use) => {
                let core_ok_identifier_in_use = ok_identifier_in_use.core_ref();
                self.walk_ok_token(&core_ok_identifier_in_use.name);
                if let Some((langle, generic_type_args, rangle)) =
                    &core_ok_identifier_in_use.generic_type_args
                {
                    self.walk_token(langle);
                    self.walk_type_tuple(generic_type_args);
                    self.walk_token(rangle);
                }
            }
            ASTNode::OkIdentifierInDecl(ok_identifier_in_decl) => {
                let core_ok_identifier_in_decl = ok_identifier_in_decl.core_ref();
                self.walk_ok_token(&core_ok_identifier_in_decl.name);
                if let Some((langle, generic_type_decls, rangle)) =
                    &core_ok_identifier_in_decl.generic_type_decls
                {
                    self.walk_token(langle);
                    self.walk_generic_type_decls(generic_type_decls);
                    self.walk_token(rangle);
                }
            }
            ASTNode::GenericTypeDecl(generic_type_decl) => {
                let core_generic_type_decl = generic_type_decl.core_ref();
                self.walk_identifier_in_decl(&core_generic_type_decl.generic_type_name);
                if let Some((colon, interface_bounds)) = &core_generic_type_decl.interface_bounds {
                    self.walk_token(colon);
                    self.walk_interface_bounds(interface_bounds);
                }
            }
            ASTNode::OkSelfKeyword(ok_self_keyword) => {
                self.walk_ok_token(&ok_self_keyword.0.as_ref().token);
            }
            ASTNode::OkToken(_) => {
                // do nothing
            }
            ASTNode::MissingToken(_) => {
                // do nothing
            }
            ASTNode::SkippedToken(_) => {
                // do nothing
            }
        }
    }
}
