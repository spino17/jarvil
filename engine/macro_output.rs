pub mod ast {
    #[macro_use]
    use jarvil_macros::Nodify;
    #[macro_use]
    use jarvil_macros::Node;
    use crate::scope::core::SymbolData;
    use crate::types::atomic::Atomic;
    use crate::{
        code::Code,
        lexer::token::{CoreToken, Token},
        scope::core::Namespace,
        types::{array::Array, core::Type},
    };
    use std::sync::Weak;
    use std::{cell::RefCell, rc::Rc};
    use text_size::{TextRange, TextSize};
    pub trait Node {
        fn range(&self) -> TextRange;
        fn start_line_number(&self) -> usize;
    }
    pub trait ErrornousNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
        ) -> Self;
    }
    pub enum ASTNode {
        BLOCK(BlockNode),
        STATEMENT_INDENT_WRAPPER(StatemenIndentWrapperNode),
        SKIPPED_TOKENS(SkippedTokensNode),
        INCORRECTLY_INDENTED_STATEMENT(IncorrectlyIndentedStatementNode),
        STATEMENT(StatementNode),
        EXPRESSION_STATEMENT(ExpressionStatementNode),
        ASSIGNMENT(AssignmentNode),
        OK_ASSIGNMENT(OkAssignmentNode),
        INVALID_L_VALUE(InvalidLValueNode),
        STRUCT_STATEMENT(StructStatementNode),
        TYPE_DECLARATION(TypeDeclarationNode),
        STRUCT_DECLARATION(StructDeclarationNode),
        LAMBDA_DECLARATION(LambdaDeclarationNode),
        OK_LAMBDA_DECLARATION(OkLambdaDeclarationNode),
        FUNCTION_DECLARATION(FunctionDeclarationNode),
        OK_FUNCTION_DECLARATION(OkFunctionDeclarationNode),
        VARIABLE_DECLARATION(VariableDeclarationNode),
        R_ASSIGNMENT(RAssignmentNode),
        NAME_TYPE_SPECS(NameTypeSpecsNode),
        OK_NAME_TYPE_SPECS(OkNameTypeSpecsNode),
        NAME_TYPE_SPEC(NameTypeSpecNode),
        TYPE_EXPRESSION(TypeExpressionNode),
        ATOMIC_TYPE(AtomicTypeNode),
        ARRAY_TYPE(ArrayTypeNode),
        USER_DEFINED_TYPE(UserDefinedTypeNode),
        EXPRESSION(ExpressionNode),
        ATOMIC_EXPRESSION(AtomicExpressionNode),
        PARENTHESISED_EXPRESSION(ParenthesisedExpressionNode),
        UNARY_EXPRESSION(UnaryExpressionNode),
        ONLY_UNARY_EXPRESSION(OnlyUnaryExpressionNode),
        BINARY_EXPRESSION(BinaryExpressionNode),
        COMPARISON(ComparisonNode),
        PARAMS(ParamsNode),
        OK_PARAMS(OkParamsNode),
        CALL_EXPRESSION(CallExpressionNode),
        CLASS_METHOD_CALL(ClassMethodCallNode),
        ATOM(AtomNode),
        ATOM_START(AtomStartNode),
        CALL(CallNode),
        PROPERTY_ACCESS(PropertyAccessNode),
        METHOD_ACCESS(MethodAccessNode),
        INDEX_ACCESS(IndexAccessNode),
        TOKEN(TokenNode),
        OK_TOKEN(OkTokenNode),
        MISSING_TOKEN(MissingTokenNode),
        SKIPPED_TOKEN(SkippedTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ASTNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                ASTNode::BLOCK(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "BLOCK", &__self_0)
                }
                ASTNode::STATEMENT_INDENT_WRAPPER(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "STATEMENT_INDENT_WRAPPER",
                        &__self_0,
                    )
                }
                ASTNode::SKIPPED_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "SKIPPED_TOKENS",
                        &__self_0,
                    )
                }
                ASTNode::INCORRECTLY_INDENTED_STATEMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "INCORRECTLY_INDENTED_STATEMENT",
                        &__self_0,
                    )
                }
                ASTNode::STATEMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "STATEMENT", &__self_0)
                }
                ASTNode::EXPRESSION_STATEMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "EXPRESSION_STATEMENT",
                        &__self_0,
                    )
                }
                ASTNode::ASSIGNMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ASSIGNMENT", &__self_0)
                }
                ASTNode::OK_ASSIGNMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK_ASSIGNMENT", &__self_0)
                }
                ASTNode::INVALID_L_VALUE(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "INVALID_L_VALUE",
                        &__self_0,
                    )
                }
                ASTNode::STRUCT_STATEMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "STRUCT_STATEMENT",
                        &__self_0,
                    )
                }
                ASTNode::TYPE_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "TYPE_DECLARATION",
                        &__self_0,
                    )
                }
                ASTNode::STRUCT_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "STRUCT_DECLARATION",
                        &__self_0,
                    )
                }
                ASTNode::LAMBDA_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "LAMBDA_DECLARATION",
                        &__self_0,
                    )
                }
                ASTNode::OK_LAMBDA_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "OK_LAMBDA_DECLARATION",
                        &__self_0,
                    )
                }
                ASTNode::FUNCTION_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "FUNCTION_DECLARATION",
                        &__self_0,
                    )
                }
                ASTNode::OK_FUNCTION_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "OK_FUNCTION_DECLARATION",
                        &__self_0,
                    )
                }
                ASTNode::VARIABLE_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "VARIABLE_DECLARATION",
                        &__self_0,
                    )
                }
                ASTNode::R_ASSIGNMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "R_ASSIGNMENT", &__self_0)
                }
                ASTNode::NAME_TYPE_SPECS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "NAME_TYPE_SPECS",
                        &__self_0,
                    )
                }
                ASTNode::OK_NAME_TYPE_SPECS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "OK_NAME_TYPE_SPECS",
                        &__self_0,
                    )
                }
                ASTNode::NAME_TYPE_SPEC(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "NAME_TYPE_SPEC",
                        &__self_0,
                    )
                }
                ASTNode::TYPE_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "TYPE_EXPRESSION",
                        &__self_0,
                    )
                }
                ASTNode::ATOMIC_TYPE(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ATOMIC_TYPE", &__self_0)
                }
                ASTNode::ARRAY_TYPE(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ARRAY_TYPE", &__self_0)
                }
                ASTNode::USER_DEFINED_TYPE(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "USER_DEFINED_TYPE",
                        &__self_0,
                    )
                }
                ASTNode::EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "EXPRESSION", &__self_0)
                }
                ASTNode::ATOMIC_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "ATOMIC_EXPRESSION",
                        &__self_0,
                    )
                }
                ASTNode::PARENTHESISED_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "PARENTHESISED_EXPRESSION",
                        &__self_0,
                    )
                }
                ASTNode::UNARY_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "UNARY_EXPRESSION",
                        &__self_0,
                    )
                }
                ASTNode::ONLY_UNARY_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "ONLY_UNARY_EXPRESSION",
                        &__self_0,
                    )
                }
                ASTNode::BINARY_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "BINARY_EXPRESSION",
                        &__self_0,
                    )
                }
                ASTNode::COMPARISON(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "COMPARISON", &__self_0)
                }
                ASTNode::PARAMS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "PARAMS", &__self_0)
                }
                ASTNode::OK_PARAMS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK_PARAMS", &__self_0)
                }
                ASTNode::CALL_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "CALL_EXPRESSION",
                        &__self_0,
                    )
                }
                ASTNode::CLASS_METHOD_CALL(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "CLASS_METHOD_CALL",
                        &__self_0,
                    )
                }
                ASTNode::ATOM(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ATOM", &__self_0)
                }
                ASTNode::ATOM_START(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ATOM_START", &__self_0)
                }
                ASTNode::CALL(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "CALL", &__self_0)
                }
                ASTNode::PROPERTY_ACCESS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "PROPERTY_ACCESS",
                        &__self_0,
                    )
                }
                ASTNode::METHOD_ACCESS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "METHOD_ACCESS", &__self_0)
                }
                ASTNode::INDEX_ACCESS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "INDEX_ACCESS", &__self_0)
                }
                ASTNode::TOKEN(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "TOKEN", &__self_0)
                }
                ASTNode::OK_TOKEN(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK_TOKEN", &__self_0)
                }
                ASTNode::MISSING_TOKEN(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "MISSING_TOKEN", &__self_0)
                }
                ASTNode::SKIPPED_TOKEN(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "SKIPPED_TOKEN", &__self_0)
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ASTNode {
        #[inline]
        fn clone(&self) -> ASTNode {
            match self {
                ASTNode::BLOCK(__self_0) => ASTNode::BLOCK(::core::clone::Clone::clone(__self_0)),
                ASTNode::STATEMENT_INDENT_WRAPPER(__self_0) => {
                    ASTNode::STATEMENT_INDENT_WRAPPER(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::SKIPPED_TOKENS(__self_0) => {
                    ASTNode::SKIPPED_TOKENS(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::INCORRECTLY_INDENTED_STATEMENT(__self_0) => {
                    ASTNode::INCORRECTLY_INDENTED_STATEMENT(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::STATEMENT(__self_0) => {
                    ASTNode::STATEMENT(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::EXPRESSION_STATEMENT(__self_0) => {
                    ASTNode::EXPRESSION_STATEMENT(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::ASSIGNMENT(__self_0) => {
                    ASTNode::ASSIGNMENT(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::OK_ASSIGNMENT(__self_0) => {
                    ASTNode::OK_ASSIGNMENT(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::INVALID_L_VALUE(__self_0) => {
                    ASTNode::INVALID_L_VALUE(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::STRUCT_STATEMENT(__self_0) => {
                    ASTNode::STRUCT_STATEMENT(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::TYPE_DECLARATION(__self_0) => {
                    ASTNode::TYPE_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::STRUCT_DECLARATION(__self_0) => {
                    ASTNode::STRUCT_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::LAMBDA_DECLARATION(__self_0) => {
                    ASTNode::LAMBDA_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::OK_LAMBDA_DECLARATION(__self_0) => {
                    ASTNode::OK_LAMBDA_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::FUNCTION_DECLARATION(__self_0) => {
                    ASTNode::FUNCTION_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::OK_FUNCTION_DECLARATION(__self_0) => {
                    ASTNode::OK_FUNCTION_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::VARIABLE_DECLARATION(__self_0) => {
                    ASTNode::VARIABLE_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::R_ASSIGNMENT(__self_0) => {
                    ASTNode::R_ASSIGNMENT(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::NAME_TYPE_SPECS(__self_0) => {
                    ASTNode::NAME_TYPE_SPECS(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::OK_NAME_TYPE_SPECS(__self_0) => {
                    ASTNode::OK_NAME_TYPE_SPECS(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::NAME_TYPE_SPEC(__self_0) => {
                    ASTNode::NAME_TYPE_SPEC(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::TYPE_EXPRESSION(__self_0) => {
                    ASTNode::TYPE_EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::ATOMIC_TYPE(__self_0) => {
                    ASTNode::ATOMIC_TYPE(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::ARRAY_TYPE(__self_0) => {
                    ASTNode::ARRAY_TYPE(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::USER_DEFINED_TYPE(__self_0) => {
                    ASTNode::USER_DEFINED_TYPE(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::EXPRESSION(__self_0) => {
                    ASTNode::EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::ATOMIC_EXPRESSION(__self_0) => {
                    ASTNode::ATOMIC_EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::PARENTHESISED_EXPRESSION(__self_0) => {
                    ASTNode::PARENTHESISED_EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::UNARY_EXPRESSION(__self_0) => {
                    ASTNode::UNARY_EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::ONLY_UNARY_EXPRESSION(__self_0) => {
                    ASTNode::ONLY_UNARY_EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::BINARY_EXPRESSION(__self_0) => {
                    ASTNode::BINARY_EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::COMPARISON(__self_0) => {
                    ASTNode::COMPARISON(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::PARAMS(__self_0) => ASTNode::PARAMS(::core::clone::Clone::clone(__self_0)),
                ASTNode::OK_PARAMS(__self_0) => {
                    ASTNode::OK_PARAMS(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::CALL_EXPRESSION(__self_0) => {
                    ASTNode::CALL_EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::CLASS_METHOD_CALL(__self_0) => {
                    ASTNode::CLASS_METHOD_CALL(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::ATOM(__self_0) => ASTNode::ATOM(::core::clone::Clone::clone(__self_0)),
                ASTNode::ATOM_START(__self_0) => {
                    ASTNode::ATOM_START(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::CALL(__self_0) => ASTNode::CALL(::core::clone::Clone::clone(__self_0)),
                ASTNode::PROPERTY_ACCESS(__self_0) => {
                    ASTNode::PROPERTY_ACCESS(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::METHOD_ACCESS(__self_0) => {
                    ASTNode::METHOD_ACCESS(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::INDEX_ACCESS(__self_0) => {
                    ASTNode::INDEX_ACCESS(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::TOKEN(__self_0) => ASTNode::TOKEN(::core::clone::Clone::clone(__self_0)),
                ASTNode::OK_TOKEN(__self_0) => {
                    ASTNode::OK_TOKEN(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::MISSING_TOKEN(__self_0) => {
                    ASTNode::MISSING_TOKEN(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::SKIPPED_TOKEN(__self_0) => {
                    ASTNode::SKIPPED_TOKEN(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct WeakBlockNode(Weak<CoreBlockNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakBlockNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakBlockNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakBlockNode {
        #[inline]
        fn clone(&self) -> WeakBlockNode {
            WeakBlockNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakStatemenIndentWrapperNode(Weak<CoreStatemenIndentWrapperNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakStatemenIndentWrapperNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakStatemenIndentWrapperNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakStatemenIndentWrapperNode {
        #[inline]
        fn clone(&self) -> WeakStatemenIndentWrapperNode {
            WeakStatemenIndentWrapperNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakSkippedTokensNode(Weak<CoreSkippedTokensNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakSkippedTokensNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakSkippedTokensNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakSkippedTokensNode {
        #[inline]
        fn clone(&self) -> WeakSkippedTokensNode {
            WeakSkippedTokensNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakIncorrectlyIndentedStatementNode(Weak<CoreIncorrectlyIndentedStatementNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakIncorrectlyIndentedStatementNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakIncorrectlyIndentedStatementNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakIncorrectlyIndentedStatementNode {
        #[inline]
        fn clone(&self) -> WeakIncorrectlyIndentedStatementNode {
            WeakIncorrectlyIndentedStatementNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakStatementNode(Weak<CoreStatementNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakStatementNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakStatementNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakStatementNode {
        #[inline]
        fn clone(&self) -> WeakStatementNode {
            WeakStatementNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakExpressionStatementNode(Weak<CoreExpressionStatementNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakExpressionStatementNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakExpressionStatementNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakExpressionStatementNode {
        #[inline]
        fn clone(&self) -> WeakExpressionStatementNode {
            WeakExpressionStatementNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakAssignmentNode(Weak<CoreAssignmentNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakAssignmentNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakAssignmentNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakAssignmentNode {
        #[inline]
        fn clone(&self) -> WeakAssignmentNode {
            WeakAssignmentNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakOkAssignmentNode(Weak<CoreOkAssignmentNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakOkAssignmentNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakOkAssignmentNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakOkAssignmentNode {
        #[inline]
        fn clone(&self) -> WeakOkAssignmentNode {
            WeakOkAssignmentNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakInvalidLValueNode(Weak<CoreInvalidLValueNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakInvalidLValueNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakInvalidLValueNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakInvalidLValueNode {
        #[inline]
        fn clone(&self) -> WeakInvalidLValueNode {
            WeakInvalidLValueNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakStructStatementNode(Weak<CoreStructStatementNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakStructStatementNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakStructStatementNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakStructStatementNode {
        #[inline]
        fn clone(&self) -> WeakStructStatementNode {
            WeakStructStatementNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakTypeDeclarationNode(Weak<CoreTypeDeclarationNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakTypeDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakTypeDeclarationNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakTypeDeclarationNode {
        #[inline]
        fn clone(&self) -> WeakTypeDeclarationNode {
            WeakTypeDeclarationNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakStructDeclarationNode(Weak<CoreStructDeclarationNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakStructDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakStructDeclarationNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakStructDeclarationNode {
        #[inline]
        fn clone(&self) -> WeakStructDeclarationNode {
            WeakStructDeclarationNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakLambdaDeclarationNode(Weak<CoreLambdaDeclarationNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakLambdaDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakLambdaDeclarationNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakLambdaDeclarationNode {
        #[inline]
        fn clone(&self) -> WeakLambdaDeclarationNode {
            WeakLambdaDeclarationNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakOkLambdaDeclarationNode(Weak<CoreOkLambdaDeclarationNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakOkLambdaDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakOkLambdaDeclarationNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakOkLambdaDeclarationNode {
        #[inline]
        fn clone(&self) -> WeakOkLambdaDeclarationNode {
            WeakOkLambdaDeclarationNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakFunctionDeclarationNode(Weak<CoreFunctionDeclarationNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakFunctionDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakFunctionDeclarationNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakFunctionDeclarationNode {
        #[inline]
        fn clone(&self) -> WeakFunctionDeclarationNode {
            WeakFunctionDeclarationNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakOkFunctionDeclarationNode(Weak<CoreOkFunctionDeclarationNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakOkFunctionDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakOkFunctionDeclarationNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakOkFunctionDeclarationNode {
        #[inline]
        fn clone(&self) -> WeakOkFunctionDeclarationNode {
            WeakOkFunctionDeclarationNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakVariableDeclarationNode(Weak<CoreVariableDeclarationNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakVariableDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakVariableDeclarationNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakVariableDeclarationNode {
        #[inline]
        fn clone(&self) -> WeakVariableDeclarationNode {
            WeakVariableDeclarationNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakRAssignmentNode(Weak<CoreRAssignmentNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakRAssignmentNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakRAssignmentNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakRAssignmentNode {
        #[inline]
        fn clone(&self) -> WeakRAssignmentNode {
            WeakRAssignmentNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakNameTypeSpecsNode(Weak<CoreNameTypeSpecsNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakNameTypeSpecsNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakNameTypeSpecsNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakNameTypeSpecsNode {
        #[inline]
        fn clone(&self) -> WeakNameTypeSpecsNode {
            WeakNameTypeSpecsNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakOkNameTypeSpecsNode(Weak<CoreOkNameTypeSpecsNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakOkNameTypeSpecsNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakOkNameTypeSpecsNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakOkNameTypeSpecsNode {
        #[inline]
        fn clone(&self) -> WeakOkNameTypeSpecsNode {
            WeakOkNameTypeSpecsNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakNameTypeSpecNode(Weak<CoreNameTypeSpecNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakNameTypeSpecNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakNameTypeSpecNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakNameTypeSpecNode {
        #[inline]
        fn clone(&self) -> WeakNameTypeSpecNode {
            WeakNameTypeSpecNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakTypeExpressionNode(Weak<CoreTypeExpressionNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakTypeExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakTypeExpressionNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakTypeExpressionNode {
        #[inline]
        fn clone(&self) -> WeakTypeExpressionNode {
            WeakTypeExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakAtomicTypeNode(Weak<CoreAtomicTypeNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakAtomicTypeNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakAtomicTypeNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakAtomicTypeNode {
        #[inline]
        fn clone(&self) -> WeakAtomicTypeNode {
            WeakAtomicTypeNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakArrayTypeNode(Weak<CoreArrayTypeNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakArrayTypeNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakArrayTypeNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakArrayTypeNode {
        #[inline]
        fn clone(&self) -> WeakArrayTypeNode {
            WeakArrayTypeNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakUserDefinedTypeNode(Weak<CoreUserDefinedTypeNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakUserDefinedTypeNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakUserDefinedTypeNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakUserDefinedTypeNode {
        #[inline]
        fn clone(&self) -> WeakUserDefinedTypeNode {
            WeakUserDefinedTypeNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakExpressionNode(Weak<CoreExpressionNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakExpressionNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakExpressionNode {
        #[inline]
        fn clone(&self) -> WeakExpressionNode {
            WeakExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakAtomicExpressionNode(Weak<CoreAtomicExpressionNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakAtomicExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakAtomicExpressionNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakAtomicExpressionNode {
        #[inline]
        fn clone(&self) -> WeakAtomicExpressionNode {
            WeakAtomicExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakParenthesisedExpressionNode(Weak<CoreParenthesisedExpressionNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakParenthesisedExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakParenthesisedExpressionNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakParenthesisedExpressionNode {
        #[inline]
        fn clone(&self) -> WeakParenthesisedExpressionNode {
            WeakParenthesisedExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakUnaryExpressionNode(Weak<CoreUnaryExpressionNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakUnaryExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakUnaryExpressionNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakUnaryExpressionNode {
        #[inline]
        fn clone(&self) -> WeakUnaryExpressionNode {
            WeakUnaryExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakOnlyUnaryExpressionNode(Weak<CoreOnlyUnaryExpressionNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakOnlyUnaryExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakOnlyUnaryExpressionNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakOnlyUnaryExpressionNode {
        #[inline]
        fn clone(&self) -> WeakOnlyUnaryExpressionNode {
            WeakOnlyUnaryExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakBinaryExpressionNode(Weak<CoreBinaryExpressionNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakBinaryExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakBinaryExpressionNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakBinaryExpressionNode {
        #[inline]
        fn clone(&self) -> WeakBinaryExpressionNode {
            WeakBinaryExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakComparisonNode(Weak<CoreComparisonNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakComparisonNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakComparisonNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakComparisonNode {
        #[inline]
        fn clone(&self) -> WeakComparisonNode {
            WeakComparisonNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakParamsNode(Weak<CoreParamsNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakParamsNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakParamsNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakParamsNode {
        #[inline]
        fn clone(&self) -> WeakParamsNode {
            WeakParamsNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakOkParamsNode(Weak<CoreOkParamsNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakOkParamsNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakOkParamsNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakOkParamsNode {
        #[inline]
        fn clone(&self) -> WeakOkParamsNode {
            WeakOkParamsNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakCallExpressionNode(Weak<CoreCallExpressionNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakCallExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakCallExpressionNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakCallExpressionNode {
        #[inline]
        fn clone(&self) -> WeakCallExpressionNode {
            WeakCallExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakClassMethodCallNode(Weak<CoreClassMethodCallNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakClassMethodCallNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakClassMethodCallNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakClassMethodCallNode {
        #[inline]
        fn clone(&self) -> WeakClassMethodCallNode {
            WeakClassMethodCallNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakAtomNode(Weak<CoreAtomNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakAtomNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakAtomNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakAtomNode {
        #[inline]
        fn clone(&self) -> WeakAtomNode {
            WeakAtomNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakAtomStartNode(Weak<CoreAtomStartNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakAtomStartNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakAtomStartNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakAtomStartNode {
        #[inline]
        fn clone(&self) -> WeakAtomStartNode {
            WeakAtomStartNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakCallNode(Weak<CoreCallNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakCallNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakCallNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakCallNode {
        #[inline]
        fn clone(&self) -> WeakCallNode {
            WeakCallNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakPropertyAccessNode(Weak<CorePropertyAccessNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakPropertyAccessNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakPropertyAccessNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakPropertyAccessNode {
        #[inline]
        fn clone(&self) -> WeakPropertyAccessNode {
            WeakPropertyAccessNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakMethodAccessNode(Weak<CoreMethodAccessNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakMethodAccessNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakMethodAccessNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakMethodAccessNode {
        #[inline]
        fn clone(&self) -> WeakMethodAccessNode {
            WeakMethodAccessNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakIndexAccessNode(Weak<CoreIndexAccessNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakIndexAccessNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakIndexAccessNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakIndexAccessNode {
        #[inline]
        fn clone(&self) -> WeakIndexAccessNode {
            WeakIndexAccessNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakTokenNode(Weak<CoreTokenNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakTokenNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakTokenNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakTokenNode {
        #[inline]
        fn clone(&self) -> WeakTokenNode {
            WeakTokenNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakOkTokenNode(Weak<CoreOkTokenNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakOkTokenNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakOkTokenNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakOkTokenNode {
        #[inline]
        fn clone(&self) -> WeakOkTokenNode {
            WeakOkTokenNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakMissingTokenNode(Weak<CoreMissingTokenNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakMissingTokenNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakMissingTokenNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakMissingTokenNode {
        #[inline]
        fn clone(&self) -> WeakMissingTokenNode {
            WeakMissingTokenNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakSkippedTokenNode(Weak<CoreSkippedTokenNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakSkippedTokenNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakSkippedTokenNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakSkippedTokenNode {
        #[inline]
        fn clone(&self) -> WeakSkippedTokenNode {
            WeakSkippedTokenNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub enum WeakASTNode {
        BLOCK(WeakBlockNode),
        STATEMENT_INDENT_WRAPPER(WeakStatemenIndentWrapperNode),
        SKIPPED_TOKENS(WeakSkippedTokensNode),
        INCORRECTLY_INDENTED_STATEMENT(WeakIncorrectlyIndentedStatementNode),
        STATEMENT(WeakStatementNode),
        EXPRESSION_STATEMENT(WeakExpressionStatementNode),
        ASSIGNMENT(WeakAssignmentNode),
        OK_ASSIGNMENT(WeakOkAssignmentNode),
        INVALID_L_VALUE(WeakInvalidLValueNode),
        STRUCT_STATEMENT(WeakStructStatementNode),
        TYPE_DECLARATION(WeakTypeDeclarationNode),
        STRUCT_DECLARATION(WeakStructDeclarationNode),
        LAMBDA_DECLARATION(WeakLambdaDeclarationNode),
        OK_LAMBDA_DECLARATION(WeakOkLambdaDeclarationNode),
        FUNCTION_DECLARATION(WeakFunctionDeclarationNode),
        OK_FUNCTION_DECLARATION(WeakOkFunctionDeclarationNode),
        VARIABLE_DECLARATION(WeakVariableDeclarationNode),
        R_ASSIGNMENT(WeakRAssignmentNode),
        NAME_TYPE_SPECS(WeakNameTypeSpecsNode),
        OK_NAME_TYPE_SPECS(WeakOkNameTypeSpecsNode),
        NAME_TYPE_SPEC(WeakNameTypeSpecNode),
        TYPE_EXPRESSION(WeakTypeExpressionNode),
        ATOMIC_TYPE(WeakAtomicTypeNode),
        ARRAY_TYPE(WeakArrayTypeNode),
        USER_DEFINED_TYPE(WeakUserDefinedTypeNode),
        EXPRESSION(WeakExpressionNode),
        ATOMIC_EXPRESSION(WeakAtomicExpressionNode),
        PARENTHESISED_EXPRESSION(WeakParenthesisedExpressionNode),
        UNARY_EXPRESSION(WeakUnaryExpressionNode),
        ONLY_UNARY_EXPRESSION(WeakOnlyUnaryExpressionNode),
        BINARY_EXPRESSION(WeakBinaryExpressionNode),
        COMPARISON(WeakComparisonNode),
        PARAMS(WeakParamsNode),
        OK_PARAMS(WeakOkParamsNode),
        CALL_EXPRESSION(WeakCallExpressionNode),
        CLASS_METHOD_CALL(WeakClassMethodCallNode),
        ATOM(WeakAtomNode),
        ATOM_START(WeakAtomStartNode),
        CALL(WeakCallNode),
        PROPERTY_ACCESS(WeakPropertyAccessNode),
        METHOD_ACCESS(WeakMethodAccessNode),
        INDEX_ACCESS(WeakIndexAccessNode),
        TOKEN(WeakTokenNode),
        OK_TOKEN(WeakOkTokenNode),
        MISSING_TOKEN(WeakMissingTokenNode),
        SKIPPED_TOKEN(WeakSkippedTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakASTNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                WeakASTNode::BLOCK(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "BLOCK", &__self_0)
                }
                WeakASTNode::STATEMENT_INDENT_WRAPPER(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "STATEMENT_INDENT_WRAPPER",
                        &__self_0,
                    )
                }
                WeakASTNode::SKIPPED_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "SKIPPED_TOKENS",
                        &__self_0,
                    )
                }
                WeakASTNode::INCORRECTLY_INDENTED_STATEMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "INCORRECTLY_INDENTED_STATEMENT",
                        &__self_0,
                    )
                }
                WeakASTNode::STATEMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "STATEMENT", &__self_0)
                }
                WeakASTNode::EXPRESSION_STATEMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "EXPRESSION_STATEMENT",
                        &__self_0,
                    )
                }
                WeakASTNode::ASSIGNMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ASSIGNMENT", &__self_0)
                }
                WeakASTNode::OK_ASSIGNMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK_ASSIGNMENT", &__self_0)
                }
                WeakASTNode::INVALID_L_VALUE(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "INVALID_L_VALUE",
                        &__self_0,
                    )
                }
                WeakASTNode::STRUCT_STATEMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "STRUCT_STATEMENT",
                        &__self_0,
                    )
                }
                WeakASTNode::TYPE_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "TYPE_DECLARATION",
                        &__self_0,
                    )
                }
                WeakASTNode::STRUCT_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "STRUCT_DECLARATION",
                        &__self_0,
                    )
                }
                WeakASTNode::LAMBDA_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "LAMBDA_DECLARATION",
                        &__self_0,
                    )
                }
                WeakASTNode::OK_LAMBDA_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "OK_LAMBDA_DECLARATION",
                        &__self_0,
                    )
                }
                WeakASTNode::FUNCTION_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "FUNCTION_DECLARATION",
                        &__self_0,
                    )
                }
                WeakASTNode::OK_FUNCTION_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "OK_FUNCTION_DECLARATION",
                        &__self_0,
                    )
                }
                WeakASTNode::VARIABLE_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "VARIABLE_DECLARATION",
                        &__self_0,
                    )
                }
                WeakASTNode::R_ASSIGNMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "R_ASSIGNMENT", &__self_0)
                }
                WeakASTNode::NAME_TYPE_SPECS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "NAME_TYPE_SPECS",
                        &__self_0,
                    )
                }
                WeakASTNode::OK_NAME_TYPE_SPECS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "OK_NAME_TYPE_SPECS",
                        &__self_0,
                    )
                }
                WeakASTNode::NAME_TYPE_SPEC(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "NAME_TYPE_SPEC",
                        &__self_0,
                    )
                }
                WeakASTNode::TYPE_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "TYPE_EXPRESSION",
                        &__self_0,
                    )
                }
                WeakASTNode::ATOMIC_TYPE(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ATOMIC_TYPE", &__self_0)
                }
                WeakASTNode::ARRAY_TYPE(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ARRAY_TYPE", &__self_0)
                }
                WeakASTNode::USER_DEFINED_TYPE(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "USER_DEFINED_TYPE",
                        &__self_0,
                    )
                }
                WeakASTNode::EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "EXPRESSION", &__self_0)
                }
                WeakASTNode::ATOMIC_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "ATOMIC_EXPRESSION",
                        &__self_0,
                    )
                }
                WeakASTNode::PARENTHESISED_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "PARENTHESISED_EXPRESSION",
                        &__self_0,
                    )
                }
                WeakASTNode::UNARY_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "UNARY_EXPRESSION",
                        &__self_0,
                    )
                }
                WeakASTNode::ONLY_UNARY_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "ONLY_UNARY_EXPRESSION",
                        &__self_0,
                    )
                }
                WeakASTNode::BINARY_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "BINARY_EXPRESSION",
                        &__self_0,
                    )
                }
                WeakASTNode::COMPARISON(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "COMPARISON", &__self_0)
                }
                WeakASTNode::PARAMS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "PARAMS", &__self_0)
                }
                WeakASTNode::OK_PARAMS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK_PARAMS", &__self_0)
                }
                WeakASTNode::CALL_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "CALL_EXPRESSION",
                        &__self_0,
                    )
                }
                WeakASTNode::CLASS_METHOD_CALL(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "CLASS_METHOD_CALL",
                        &__self_0,
                    )
                }
                WeakASTNode::ATOM(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ATOM", &__self_0)
                }
                WeakASTNode::ATOM_START(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ATOM_START", &__self_0)
                }
                WeakASTNode::CALL(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "CALL", &__self_0)
                }
                WeakASTNode::PROPERTY_ACCESS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "PROPERTY_ACCESS",
                        &__self_0,
                    )
                }
                WeakASTNode::METHOD_ACCESS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "METHOD_ACCESS", &__self_0)
                }
                WeakASTNode::INDEX_ACCESS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "INDEX_ACCESS", &__self_0)
                }
                WeakASTNode::TOKEN(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "TOKEN", &__self_0)
                }
                WeakASTNode::OK_TOKEN(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK_TOKEN", &__self_0)
                }
                WeakASTNode::MISSING_TOKEN(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "MISSING_TOKEN", &__self_0)
                }
                WeakASTNode::SKIPPED_TOKEN(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "SKIPPED_TOKEN", &__self_0)
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakASTNode {
        #[inline]
        fn clone(&self) -> WeakASTNode {
            match self {
                WeakASTNode::BLOCK(__self_0) => {
                    WeakASTNode::BLOCK(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::STATEMENT_INDENT_WRAPPER(__self_0) => {
                    WeakASTNode::STATEMENT_INDENT_WRAPPER(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::SKIPPED_TOKENS(__self_0) => {
                    WeakASTNode::SKIPPED_TOKENS(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::INCORRECTLY_INDENTED_STATEMENT(__self_0) => {
                    WeakASTNode::INCORRECTLY_INDENTED_STATEMENT(::core::clone::Clone::clone(
                        __self_0,
                    ))
                }
                WeakASTNode::STATEMENT(__self_0) => {
                    WeakASTNode::STATEMENT(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::EXPRESSION_STATEMENT(__self_0) => {
                    WeakASTNode::EXPRESSION_STATEMENT(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::ASSIGNMENT(__self_0) => {
                    WeakASTNode::ASSIGNMENT(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::OK_ASSIGNMENT(__self_0) => {
                    WeakASTNode::OK_ASSIGNMENT(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::INVALID_L_VALUE(__self_0) => {
                    WeakASTNode::INVALID_L_VALUE(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::STRUCT_STATEMENT(__self_0) => {
                    WeakASTNode::STRUCT_STATEMENT(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::TYPE_DECLARATION(__self_0) => {
                    WeakASTNode::TYPE_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::STRUCT_DECLARATION(__self_0) => {
                    WeakASTNode::STRUCT_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::LAMBDA_DECLARATION(__self_0) => {
                    WeakASTNode::LAMBDA_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::OK_LAMBDA_DECLARATION(__self_0) => {
                    WeakASTNode::OK_LAMBDA_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::FUNCTION_DECLARATION(__self_0) => {
                    WeakASTNode::FUNCTION_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::OK_FUNCTION_DECLARATION(__self_0) => {
                    WeakASTNode::OK_FUNCTION_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::VARIABLE_DECLARATION(__self_0) => {
                    WeakASTNode::VARIABLE_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::R_ASSIGNMENT(__self_0) => {
                    WeakASTNode::R_ASSIGNMENT(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::NAME_TYPE_SPECS(__self_0) => {
                    WeakASTNode::NAME_TYPE_SPECS(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::OK_NAME_TYPE_SPECS(__self_0) => {
                    WeakASTNode::OK_NAME_TYPE_SPECS(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::NAME_TYPE_SPEC(__self_0) => {
                    WeakASTNode::NAME_TYPE_SPEC(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::TYPE_EXPRESSION(__self_0) => {
                    WeakASTNode::TYPE_EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::ATOMIC_TYPE(__self_0) => {
                    WeakASTNode::ATOMIC_TYPE(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::ARRAY_TYPE(__self_0) => {
                    WeakASTNode::ARRAY_TYPE(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::USER_DEFINED_TYPE(__self_0) => {
                    WeakASTNode::USER_DEFINED_TYPE(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::EXPRESSION(__self_0) => {
                    WeakASTNode::EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::ATOMIC_EXPRESSION(__self_0) => {
                    WeakASTNode::ATOMIC_EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::PARENTHESISED_EXPRESSION(__self_0) => {
                    WeakASTNode::PARENTHESISED_EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::UNARY_EXPRESSION(__self_0) => {
                    WeakASTNode::UNARY_EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::ONLY_UNARY_EXPRESSION(__self_0) => {
                    WeakASTNode::ONLY_UNARY_EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::BINARY_EXPRESSION(__self_0) => {
                    WeakASTNode::BINARY_EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::COMPARISON(__self_0) => {
                    WeakASTNode::COMPARISON(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::PARAMS(__self_0) => {
                    WeakASTNode::PARAMS(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::OK_PARAMS(__self_0) => {
                    WeakASTNode::OK_PARAMS(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::CALL_EXPRESSION(__self_0) => {
                    WeakASTNode::CALL_EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::CLASS_METHOD_CALL(__self_0) => {
                    WeakASTNode::CLASS_METHOD_CALL(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::ATOM(__self_0) => {
                    WeakASTNode::ATOM(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::ATOM_START(__self_0) => {
                    WeakASTNode::ATOM_START(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::CALL(__self_0) => {
                    WeakASTNode::CALL(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::PROPERTY_ACCESS(__self_0) => {
                    WeakASTNode::PROPERTY_ACCESS(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::METHOD_ACCESS(__self_0) => {
                    WeakASTNode::METHOD_ACCESS(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::INDEX_ACCESS(__self_0) => {
                    WeakASTNode::INDEX_ACCESS(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::TOKEN(__self_0) => {
                    WeakASTNode::TOKEN(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::OK_TOKEN(__self_0) => {
                    WeakASTNode::OK_TOKEN(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::MISSING_TOKEN(__self_0) => {
                    WeakASTNode::MISSING_TOKEN(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::SKIPPED_TOKEN(__self_0) => {
                    WeakASTNode::SKIPPED_TOKEN(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    impl ASTNode {
        pub fn new_with_BlockNode(x: &BlockNode) -> Self {
            ASTNode::BLOCK(x.clone())
        }
        pub fn new_with_StatemenIndentWrapperNode(x: &StatemenIndentWrapperNode) -> Self {
            ASTNode::STATEMENT_INDENT_WRAPPER(x.clone())
        }
        pub fn new_with_SkippedTokensNode(x: &SkippedTokensNode) -> Self {
            ASTNode::SKIPPED_TOKENS(x.clone())
        }
        pub fn new_with_IncorrectlyIndentedStatementNode(
            x: &IncorrectlyIndentedStatementNode,
        ) -> Self {
            ASTNode::INCORRECTLY_INDENTED_STATEMENT(x.clone())
        }
        pub fn new_with_StatementNode(x: &StatementNode) -> Self {
            ASTNode::STATEMENT(x.clone())
        }
        pub fn new_with_ExpressionStatementNode(x: &ExpressionStatementNode) -> Self {
            ASTNode::EXPRESSION_STATEMENT(x.clone())
        }
        pub fn new_with_AssignmentNode(x: &AssignmentNode) -> Self {
            ASTNode::ASSIGNMENT(x.clone())
        }
        pub fn new_with_OkAssignmentNode(x: &OkAssignmentNode) -> Self {
            ASTNode::OK_ASSIGNMENT(x.clone())
        }
        pub fn new_with_InvalidLValueNode(x: &InvalidLValueNode) -> Self {
            ASTNode::INVALID_L_VALUE(x.clone())
        }
        pub fn new_with_StructStatementNode(x: &StructStatementNode) -> Self {
            ASTNode::STRUCT_STATEMENT(x.clone())
        }
        pub fn new_with_TypeDeclarationNode(x: &TypeDeclarationNode) -> Self {
            ASTNode::TYPE_DECLARATION(x.clone())
        }
        pub fn new_with_StructDeclarationNode(x: &StructDeclarationNode) -> Self {
            ASTNode::STRUCT_DECLARATION(x.clone())
        }
        pub fn new_with_LambdaDeclarationNode(x: &LambdaDeclarationNode) -> Self {
            ASTNode::LAMBDA_DECLARATION(x.clone())
        }
        pub fn new_with_OkLambdaDeclarationNode(x: &OkLambdaDeclarationNode) -> Self {
            ASTNode::OK_LAMBDA_DECLARATION(x.clone())
        }
        pub fn new_with_FunctionDeclarationNode(x: &FunctionDeclarationNode) -> Self {
            ASTNode::FUNCTION_DECLARATION(x.clone())
        }
        pub fn new_with_OkFunctionDeclarationNode(x: &OkFunctionDeclarationNode) -> Self {
            ASTNode::OK_FUNCTION_DECLARATION(x.clone())
        }
        pub fn new_with_VariableDeclarationNode(x: &VariableDeclarationNode) -> Self {
            ASTNode::VARIABLE_DECLARATION(x.clone())
        }
        pub fn new_with_RAssignmentNode(x: &RAssignmentNode) -> Self {
            ASTNode::R_ASSIGNMENT(x.clone())
        }
        pub fn new_with_NameTypeSpecsNode(x: &NameTypeSpecsNode) -> Self {
            ASTNode::NAME_TYPE_SPECS(x.clone())
        }
        pub fn new_with_OkNameTypeSpecsNode(x: &OkNameTypeSpecsNode) -> Self {
            ASTNode::OK_NAME_TYPE_SPECS(x.clone())
        }
        pub fn new_with_NameTypeSpecNode(x: &NameTypeSpecNode) -> Self {
            ASTNode::NAME_TYPE_SPEC(x.clone())
        }
        pub fn new_with_TypeExpressionNode(x: &TypeExpressionNode) -> Self {
            ASTNode::TYPE_EXPRESSION(x.clone())
        }
        pub fn new_with_AtomicTypeNode(x: &AtomicTypeNode) -> Self {
            ASTNode::ATOMIC_TYPE(x.clone())
        }
        pub fn new_with_ArrayTypeNode(x: &ArrayTypeNode) -> Self {
            ASTNode::ARRAY_TYPE(x.clone())
        }
        pub fn new_with_UserDefinedTypeNode(x: &UserDefinedTypeNode) -> Self {
            ASTNode::USER_DEFINED_TYPE(x.clone())
        }
        pub fn new_with_ExpressionNode(x: &ExpressionNode) -> Self {
            ASTNode::EXPRESSION(x.clone())
        }
        pub fn new_with_AtomicExpressionNode(x: &AtomicExpressionNode) -> Self {
            ASTNode::ATOMIC_EXPRESSION(x.clone())
        }
        pub fn new_with_ParenthesisedExpressionNode(x: &ParenthesisedExpressionNode) -> Self {
            ASTNode::PARENTHESISED_EXPRESSION(x.clone())
        }
        pub fn new_with_UnaryExpressionNode(x: &UnaryExpressionNode) -> Self {
            ASTNode::UNARY_EXPRESSION(x.clone())
        }
        pub fn new_with_OnlyUnaryExpressionNode(x: &OnlyUnaryExpressionNode) -> Self {
            ASTNode::ONLY_UNARY_EXPRESSION(x.clone())
        }
        pub fn new_with_BinaryExpressionNode(x: &BinaryExpressionNode) -> Self {
            ASTNode::BINARY_EXPRESSION(x.clone())
        }
        pub fn new_with_ComparisonNode(x: &ComparisonNode) -> Self {
            ASTNode::COMPARISON(x.clone())
        }
        pub fn new_with_ParamsNode(x: &ParamsNode) -> Self {
            ASTNode::PARAMS(x.clone())
        }
        pub fn new_with_OkParamsNode(x: &OkParamsNode) -> Self {
            ASTNode::OK_PARAMS(x.clone())
        }
        pub fn new_with_CallExpressionNode(x: &CallExpressionNode) -> Self {
            ASTNode::CALL_EXPRESSION(x.clone())
        }
        pub fn new_with_ClassMethodCallNode(x: &ClassMethodCallNode) -> Self {
            ASTNode::CLASS_METHOD_CALL(x.clone())
        }
        pub fn new_with_AtomNode(x: &AtomNode) -> Self {
            ASTNode::ATOM(x.clone())
        }
        pub fn new_with_AtomStartNode(x: &AtomStartNode) -> Self {
            ASTNode::ATOM_START(x.clone())
        }
        pub fn new_with_CallNode(x: &CallNode) -> Self {
            ASTNode::CALL(x.clone())
        }
        pub fn new_with_PropertyAccessNode(x: &PropertyAccessNode) -> Self {
            ASTNode::PROPERTY_ACCESS(x.clone())
        }
        pub fn new_with_MethodAccessNode(x: &MethodAccessNode) -> Self {
            ASTNode::METHOD_ACCESS(x.clone())
        }
        pub fn new_with_IndexAccessNode(x: &IndexAccessNode) -> Self {
            ASTNode::INDEX_ACCESS(x.clone())
        }
        pub fn new_with_TokenNode(x: &TokenNode) -> Self {
            ASTNode::TOKEN(x.clone())
        }
        pub fn new_with_OkTokenNode(x: &OkTokenNode) -> Self {
            ASTNode::OK_TOKEN(x.clone())
        }
        pub fn new_with_MissingTokenNode(x: &MissingTokenNode) -> Self {
            ASTNode::MISSING_TOKEN(x.clone())
        }
        pub fn new_with_SkippedTokenNode(x: &SkippedTokenNode) -> Self {
            ASTNode::SKIPPED_TOKEN(x.clone())
        }
    }
    pub struct CoreBlockNode {
        pub newline: TokenNode,
        pub stmts: Vec<StatemenIndentWrapperNode>,
        pub scope: Option<Namespace>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreBlockNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "CoreBlockNode",
                "newline",
                &&self.newline,
                "stmts",
                &&self.stmts,
                "scope",
                &&self.scope,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreBlockNode {
        #[inline]
        fn clone(&self) -> CoreBlockNode {
            CoreBlockNode {
                newline: ::core::clone::Clone::clone(&self.newline),
                stmts: ::core::clone::Clone::clone(&self.stmts),
                scope: ::core::clone::Clone::clone(&self.scope),
            }
        }
    }
    pub struct BlockNode(Rc<RefCell<CoreBlockNode>>);
    #[automatically_derived]
    impl ::core::fmt::Debug for BlockNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "BlockNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for BlockNode {
        #[inline]
        fn clone(&self) -> BlockNode {
            BlockNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl BlockNode {
        pub fn new(stmts: Vec<StatemenIndentWrapperNode>, newline: &TokenNode) -> Self {
            let node = Rc::new(RefCell::new(CoreBlockNode {
                newline: newline.clone(),
                stmts,
                scope: None,
            }));
            BlockNode(node)
        }
        pub fn set_scope(&self, scope: &Namespace) {
            self.0.as_ref().borrow_mut().scope = Some(scope.clone());
        }
    }
    impl Node for BlockNode {
        fn range(&self) -> TextRange {
            let stmts_len = self.0.as_ref().borrow().stmts.len();
            TextRange::new(
                TextSize::from(self.0.as_ref().borrow().newline.range().start()),
                TextSize::from(self.0.as_ref().borrow().stmts[stmts_len - 1].range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().borrow().newline.start_line_number()
        }
    }
    pub enum CoreStatemenIndentWrapperNode {
        CORRECTLY_INDENTED(StatementNode),
        INCORRECTLY_INDENTED(IncorrectlyIndentedStatementNode),
        LEADING_SKIPPED_TOKENS(SkippedTokensNode),
        TRAILING_SKIPPED_TOKENS(SkippedTokensNode),
        EXTRA_NEWLINES(SkippedTokensNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreStatemenIndentWrapperNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreStatemenIndentWrapperNode::CORRECTLY_INDENTED(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "CORRECTLY_INDENTED",
                        &__self_0,
                    )
                }
                CoreStatemenIndentWrapperNode::INCORRECTLY_INDENTED(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "INCORRECTLY_INDENTED",
                        &__self_0,
                    )
                }
                CoreStatemenIndentWrapperNode::LEADING_SKIPPED_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "LEADING_SKIPPED_TOKENS",
                        &__self_0,
                    )
                }
                CoreStatemenIndentWrapperNode::TRAILING_SKIPPED_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "TRAILING_SKIPPED_TOKENS",
                        &__self_0,
                    )
                }
                CoreStatemenIndentWrapperNode::EXTRA_NEWLINES(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "EXTRA_NEWLINES",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreStatemenIndentWrapperNode {
        #[inline]
        fn clone(&self) -> CoreStatemenIndentWrapperNode {
            match self {
                CoreStatemenIndentWrapperNode::CORRECTLY_INDENTED(__self_0) => {
                    CoreStatemenIndentWrapperNode::CORRECTLY_INDENTED(::core::clone::Clone::clone(
                        __self_0,
                    ))
                }
                CoreStatemenIndentWrapperNode::INCORRECTLY_INDENTED(__self_0) => {
                    CoreStatemenIndentWrapperNode::INCORRECTLY_INDENTED(
                        ::core::clone::Clone::clone(__self_0),
                    )
                }
                CoreStatemenIndentWrapperNode::LEADING_SKIPPED_TOKENS(__self_0) => {
                    CoreStatemenIndentWrapperNode::LEADING_SKIPPED_TOKENS(
                        ::core::clone::Clone::clone(__self_0),
                    )
                }
                CoreStatemenIndentWrapperNode::TRAILING_SKIPPED_TOKENS(__self_0) => {
                    CoreStatemenIndentWrapperNode::TRAILING_SKIPPED_TOKENS(
                        ::core::clone::Clone::clone(__self_0),
                    )
                }
                CoreStatemenIndentWrapperNode::EXTRA_NEWLINES(__self_0) => {
                    CoreStatemenIndentWrapperNode::EXTRA_NEWLINES(::core::clone::Clone::clone(
                        __self_0,
                    ))
                }
            }
        }
    }
    pub struct StatemenIndentWrapperNode(Rc<CoreStatemenIndentWrapperNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for StatemenIndentWrapperNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "StatemenIndentWrapperNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for StatemenIndentWrapperNode {
        #[inline]
        fn clone(&self) -> StatemenIndentWrapperNode {
            StatemenIndentWrapperNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl StatemenIndentWrapperNode {
        pub fn new_with_correctly_indented(stmt: &StatementNode) -> Self {
            let node = Rc::new(CoreStatemenIndentWrapperNode::CORRECTLY_INDENTED(
                stmt.clone(),
            ));
            StatemenIndentWrapperNode(node)
        }
        pub fn new_with_incorrectly_indented(
            stmt: &StatementNode,
            expected_indent: i64,
            received_indent: i64,
        ) -> Self {
            let node = Rc::new(CoreStatemenIndentWrapperNode::INCORRECTLY_INDENTED(
                IncorrectlyIndentedStatementNode::new(stmt, expected_indent, received_indent),
            ));
            StatemenIndentWrapperNode(node)
        }
        pub fn new_with_leading_skipped_tokens(skipped_tokens: &SkippedTokensNode) -> Self {
            let node = Rc::new(CoreStatemenIndentWrapperNode::LEADING_SKIPPED_TOKENS(
                skipped_tokens.clone(),
            ));
            StatemenIndentWrapperNode(node)
        }
        pub fn new_with_trailing_skipped_tokens(skipped_tokens: &SkippedTokensNode) -> Self {
            let node = Rc::new(CoreStatemenIndentWrapperNode::TRAILING_SKIPPED_TOKENS(
                skipped_tokens.clone(),
            ));
            StatemenIndentWrapperNode(node)
        }
        pub fn new_with_extra_newlines(skipped_tokens: &SkippedTokensNode) -> Self {
            let node = Rc::new(CoreStatemenIndentWrapperNode::EXTRA_NEWLINES(
                skipped_tokens.clone(),
            ));
            StatemenIndentWrapperNode(node)
        }
    }
    impl Node for StatemenIndentWrapperNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref() {
                CoreStatemenIndentWrapperNode::CORRECTLY_INDENTED(stmt) => TextRange::new(
                    TextSize::from(stmt.range().start()),
                    TextSize::from(stmt.range().end()),
                ),
                CoreStatemenIndentWrapperNode::INCORRECTLY_INDENTED(stmt) => TextRange::new(
                    TextSize::from(stmt.range().start()),
                    TextSize::from(stmt.range().end()),
                ),
                CoreStatemenIndentWrapperNode::LEADING_SKIPPED_TOKENS(skipped_tokens) => {
                    TextRange::new(
                        TextSize::from(skipped_tokens.range().start()),
                        TextSize::from(skipped_tokens.range().end()),
                    )
                }
                CoreStatemenIndentWrapperNode::TRAILING_SKIPPED_TOKENS(skipped_tokens) => {
                    TextRange::new(
                        TextSize::from(skipped_tokens.range().start()),
                        TextSize::from(skipped_tokens.range().end()),
                    )
                }
                CoreStatemenIndentWrapperNode::EXTRA_NEWLINES(skipped_tokens) => TextRange::new(
                    TextSize::from(skipped_tokens.range().start()),
                    TextSize::from(skipped_tokens.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref() {
                CoreStatemenIndentWrapperNode::CORRECTLY_INDENTED(stmt) => stmt.start_line_number(),
                CoreStatemenIndentWrapperNode::INCORRECTLY_INDENTED(incorrectly_indented_stmt) => {
                    incorrectly_indented_stmt.start_line_number()
                }
                CoreStatemenIndentWrapperNode::LEADING_SKIPPED_TOKENS(skipped_tokens) => {
                    skipped_tokens.start_line_number()
                }
                CoreStatemenIndentWrapperNode::TRAILING_SKIPPED_TOKENS(skipped_tokens) => {
                    skipped_tokens.start_line_number()
                }
                CoreStatemenIndentWrapperNode::EXTRA_NEWLINES(skipped_tokens) => {
                    skipped_tokens.start_line_number()
                }
            }
        }
    }
    pub struct CoreSkippedTokensNode {
        pub skipped_tokens: Vec<SkippedTokenNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreSkippedTokensNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "CoreSkippedTokensNode",
                "skipped_tokens",
                &&self.skipped_tokens,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreSkippedTokensNode {
        #[inline]
        fn clone(&self) -> CoreSkippedTokensNode {
            CoreSkippedTokensNode {
                skipped_tokens: ::core::clone::Clone::clone(&self.skipped_tokens),
            }
        }
    }
    pub struct SkippedTokensNode(Rc<CoreSkippedTokensNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for SkippedTokensNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "SkippedTokensNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for SkippedTokensNode {
        #[inline]
        fn clone(&self) -> SkippedTokensNode {
            SkippedTokensNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl SkippedTokensNode {
        pub fn new_with_leading_skipped_tokens(skipped_tokens: Vec<SkippedTokenNode>) -> Self {
            let node = Rc::new(CoreSkippedTokensNode { skipped_tokens });
            SkippedTokensNode(node)
        }
        pub fn new_with_trailing_skipped_tokens(skipped_tokens: Vec<SkippedTokenNode>) -> Self {
            let node = Rc::new(CoreSkippedTokensNode { skipped_tokens });
            SkippedTokensNode(node)
        }
        pub fn new_with_extra_newlines(skipped_tokens: Vec<SkippedTokenNode>) -> Self {
            let node = Rc::new(CoreSkippedTokensNode { skipped_tokens });
            SkippedTokensNode(node)
        }
    }
    impl Node for SkippedTokensNode {
        fn range(&self) -> TextRange {
            let core_skipped_tokens = &self.0.as_ref().skipped_tokens;
            TextRange::new(
                TextSize::from(core_skipped_tokens[0].range().start()),
                TextSize::from(
                    core_skipped_tokens[core_skipped_tokens.len() - 1]
                        .range()
                        .end(),
                ),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().skipped_tokens[0].start_line_number()
        }
    }
    pub enum CoreStatementNode {
        EXPRESSION(ExpressionStatementNode),
        ASSIGNMENT(AssignmentNode),
        VARIABLE_DECLARATION(VariableDeclarationNode),
        FUNCTION_DECLARATION(FunctionDeclarationNode),
        TYPE_DECLARATION(TypeDeclarationNode),
        STRUCT_STATEMENT(StructStatementNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreStatementNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreStatementNode::EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "EXPRESSION", &__self_0)
                }
                CoreStatementNode::ASSIGNMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ASSIGNMENT", &__self_0)
                }
                CoreStatementNode::VARIABLE_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "VARIABLE_DECLARATION",
                        &__self_0,
                    )
                }
                CoreStatementNode::FUNCTION_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "FUNCTION_DECLARATION",
                        &__self_0,
                    )
                }
                CoreStatementNode::TYPE_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "TYPE_DECLARATION",
                        &__self_0,
                    )
                }
                CoreStatementNode::STRUCT_STATEMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "STRUCT_STATEMENT",
                        &__self_0,
                    )
                }
                CoreStatementNode::MISSING_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "MISSING_TOKENS",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreStatementNode {
        #[inline]
        fn clone(&self) -> CoreStatementNode {
            match self {
                CoreStatementNode::EXPRESSION(__self_0) => {
                    CoreStatementNode::EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                CoreStatementNode::ASSIGNMENT(__self_0) => {
                    CoreStatementNode::ASSIGNMENT(::core::clone::Clone::clone(__self_0))
                }
                CoreStatementNode::VARIABLE_DECLARATION(__self_0) => {
                    CoreStatementNode::VARIABLE_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                CoreStatementNode::FUNCTION_DECLARATION(__self_0) => {
                    CoreStatementNode::FUNCTION_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                CoreStatementNode::TYPE_DECLARATION(__self_0) => {
                    CoreStatementNode::TYPE_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                CoreStatementNode::STRUCT_STATEMENT(__self_0) => {
                    CoreStatementNode::STRUCT_STATEMENT(::core::clone::Clone::clone(__self_0))
                }
                CoreStatementNode::MISSING_TOKENS(__self_0) => {
                    CoreStatementNode::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct StatementNode(Rc<CoreStatementNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for StatementNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "StatementNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for StatementNode {
        #[inline]
        fn clone(&self) -> StatementNode {
            StatementNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl StatementNode {
        pub fn new_with_expression(expr: &ExpressionNode, newline: &TokenNode) -> Self {
            let node = Rc::new(CoreStatementNode::EXPRESSION(ExpressionStatementNode::new(
                expr, newline,
            )));
            StatementNode(node)
        }
        pub fn new_with_assignment(assignment: &AssignmentNode) -> Self {
            let node = Rc::new(CoreStatementNode::ASSIGNMENT(assignment.clone()));
            StatementNode(node)
        }
        pub fn new_with_variable_declaration(variable_decl: &VariableDeclarationNode) -> Self {
            let node = Rc::new(CoreStatementNode::VARIABLE_DECLARATION(
                variable_decl.clone(),
            ));
            StatementNode(node)
        }
        pub fn new_with_function_declaration(function_decl: &FunctionDeclarationNode) -> Self {
            let node = Rc::new(CoreStatementNode::FUNCTION_DECLARATION(
                function_decl.clone(),
            ));
            StatementNode(node)
        }
        pub fn new_with_type_declaration(type_decl: &TypeDeclarationNode) -> Self {
            let node = Rc::new(CoreStatementNode::TYPE_DECLARATION(type_decl.clone()));
            StatementNode(node)
        }
        pub fn new_with_struct_stmt(struct_stmt: &StructStatementNode) -> Self {
            let node = Rc::new(CoreStatementNode::STRUCT_STATEMENT(struct_stmt.clone()));
            StatementNode(node)
        }
    }
    impl Node for StatementNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref() {
                CoreStatementNode::EXPRESSION(expr_stmt) => TextRange::new(
                    TextSize::from(expr_stmt.range().start()),
                    TextSize::from(expr_stmt.range().end()),
                ),
                CoreStatementNode::ASSIGNMENT(assignment) => TextRange::new(
                    TextSize::from(assignment.range().start()),
                    TextSize::from(assignment.range().end()),
                ),
                CoreStatementNode::VARIABLE_DECLARATION(variable_decl) => TextRange::new(
                    TextSize::from(variable_decl.range().start()),
                    TextSize::from(variable_decl.range().end()),
                ),
                CoreStatementNode::FUNCTION_DECLARATION(func_decl) => TextRange::new(
                    TextSize::from(func_decl.range().start()),
                    TextSize::from(func_decl.range().end()),
                ),
                CoreStatementNode::TYPE_DECLARATION(type_decl) => TextRange::new(
                    TextSize::from(type_decl.range().start()),
                    TextSize::from(type_decl.range().end()),
                ),
                CoreStatementNode::STRUCT_STATEMENT(struct_stmt) => TextRange::new(
                    TextSize::from(struct_stmt.range().start()),
                    TextSize::from(struct_stmt.range().end()),
                ),
                CoreStatementNode::MISSING_TOKENS(missing_tokens) => TextRange::new(
                    TextSize::from(missing_tokens.range().start()),
                    TextSize::from(missing_tokens.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref() {
                CoreStatementNode::EXPRESSION(expr_stmt) => expr_stmt.start_line_number(),
                CoreStatementNode::ASSIGNMENT(assignment) => assignment.start_line_number(),
                CoreStatementNode::VARIABLE_DECLARATION(variable_decl) => {
                    variable_decl.start_line_number()
                }
                CoreStatementNode::FUNCTION_DECLARATION(func_decl) => func_decl.start_line_number(),
                CoreStatementNode::TYPE_DECLARATION(type_decl) => type_decl.start_line_number(),
                CoreStatementNode::STRUCT_STATEMENT(struct_stmt) => struct_stmt.start_line_number(),
                CoreStatementNode::MISSING_TOKENS(missing_tokens) => {
                    missing_tokens.start_line_number()
                }
            }
        }
    }
    impl ErrornousNode for StatementNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
        ) -> Self {
            StatementNode(Rc::new(CoreStatementNode::MISSING_TOKENS(
                MissingTokenNode::new(expected_symbols, received_token),
            )))
        }
    }
    pub struct CoreIncorrectlyIndentedStatementNode {
        pub stmt: StatementNode,
        pub expected_indent: i64,
        pub received_indent: i64,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreIncorrectlyIndentedStatementNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "CoreIncorrectlyIndentedStatementNode",
                "stmt",
                &&self.stmt,
                "expected_indent",
                &&self.expected_indent,
                "received_indent",
                &&self.received_indent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreIncorrectlyIndentedStatementNode {
        #[inline]
        fn clone(&self) -> CoreIncorrectlyIndentedStatementNode {
            CoreIncorrectlyIndentedStatementNode {
                stmt: ::core::clone::Clone::clone(&self.stmt),
                expected_indent: ::core::clone::Clone::clone(&self.expected_indent),
                received_indent: ::core::clone::Clone::clone(&self.received_indent),
            }
        }
    }
    pub struct IncorrectlyIndentedStatementNode(Rc<CoreIncorrectlyIndentedStatementNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for IncorrectlyIndentedStatementNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "IncorrectlyIndentedStatementNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for IncorrectlyIndentedStatementNode {
        #[inline]
        fn clone(&self) -> IncorrectlyIndentedStatementNode {
            IncorrectlyIndentedStatementNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl IncorrectlyIndentedStatementNode {
        fn new(stmt: &StatementNode, expected_indent: i64, received_indent: i64) -> Self {
            let node = Rc::new(CoreIncorrectlyIndentedStatementNode {
                stmt: stmt.clone(),
                expected_indent,
                received_indent,
            });
            IncorrectlyIndentedStatementNode(node)
        }
    }
    impl Node for IncorrectlyIndentedStatementNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().stmt.range().start()),
                TextSize::from(self.0.as_ref().stmt.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().stmt.start_line_number()
        }
    }
    pub struct CoreExpressionStatementNode {
        pub expr: ExpressionNode,
        pub newline: TokenNode,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreExpressionStatementNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreExpressionStatementNode",
                "expr",
                &&self.expr,
                "newline",
                &&self.newline,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreExpressionStatementNode {
        #[inline]
        fn clone(&self) -> CoreExpressionStatementNode {
            CoreExpressionStatementNode {
                expr: ::core::clone::Clone::clone(&self.expr),
                newline: ::core::clone::Clone::clone(&self.newline),
            }
        }
    }
    pub struct ExpressionStatementNode(Rc<CoreExpressionStatementNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for ExpressionStatementNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "ExpressionStatementNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ExpressionStatementNode {
        #[inline]
        fn clone(&self) -> ExpressionStatementNode {
            ExpressionStatementNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl ExpressionStatementNode {
        fn new(expr: &ExpressionNode, newline: &TokenNode) -> Self {
            let node = Rc::new(CoreExpressionStatementNode {
                expr: expr.clone(),
                newline: newline.clone(),
            });
            ExpressionStatementNode(node)
        }
    }
    impl Node for ExpressionStatementNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().expr.range().start()),
                TextSize::from(self.0.as_ref().newline.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().expr.start_line_number()
        }
    }
    pub enum CoreAssignmentNode {
        OK(OkAssignmentNode),
        INVALID_L_VALUE(InvalidLValueNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreAssignmentNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreAssignmentNode::OK(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK", &__self_0)
                }
                CoreAssignmentNode::INVALID_L_VALUE(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "INVALID_L_VALUE",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreAssignmentNode {
        #[inline]
        fn clone(&self) -> CoreAssignmentNode {
            match self {
                CoreAssignmentNode::OK(__self_0) => {
                    CoreAssignmentNode::OK(::core::clone::Clone::clone(__self_0))
                }
                CoreAssignmentNode::INVALID_L_VALUE(__self_0) => {
                    CoreAssignmentNode::INVALID_L_VALUE(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct AssignmentNode(Rc<CoreAssignmentNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for AssignmentNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "AssignmentNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for AssignmentNode {
        #[inline]
        fn clone(&self) -> AssignmentNode {
            AssignmentNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl AssignmentNode {
        pub fn new(l_atom: &AtomNode, r_assign: &RAssignmentNode, equal: &TokenNode) -> Self {
            let node = Rc::new(CoreAssignmentNode::OK(OkAssignmentNode::new(
                l_atom, r_assign, equal,
            )));
            AssignmentNode(node)
        }
        pub fn new_with_invalid_l_value(
            l_expr: &ExpressionNode,
            r_assign: &RAssignmentNode,
            equal: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreAssignmentNode::INVALID_L_VALUE(InvalidLValueNode::new(
                l_expr, r_assign, equal,
            )));
            AssignmentNode(node)
        }
    }
    impl Node for AssignmentNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref() {
                CoreAssignmentNode::OK(ok_assignment) => TextRange::new(
                    TextSize::from(ok_assignment.range().start()),
                    TextSize::from(ok_assignment.range().end()),
                ),
                CoreAssignmentNode::INVALID_L_VALUE(invalid_l_value) => TextRange::new(
                    TextSize::from(invalid_l_value.range().start()),
                    TextSize::from(invalid_l_value.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref() {
                CoreAssignmentNode::OK(ok_assignment) => ok_assignment.start_line_number(),
                CoreAssignmentNode::INVALID_L_VALUE(invalid_l_value) => {
                    invalid_l_value.start_line_number()
                }
            }
        }
    }
    pub struct CoreOkAssignmentNode {
        pub equal: TokenNode,
        pub l_atom: AtomNode,
        pub r_assign: RAssignmentNode,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreOkAssignmentNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "CoreOkAssignmentNode",
                "equal",
                &&self.equal,
                "l_atom",
                &&self.l_atom,
                "r_assign",
                &&self.r_assign,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreOkAssignmentNode {
        #[inline]
        fn clone(&self) -> CoreOkAssignmentNode {
            CoreOkAssignmentNode {
                equal: ::core::clone::Clone::clone(&self.equal),
                l_atom: ::core::clone::Clone::clone(&self.l_atom),
                r_assign: ::core::clone::Clone::clone(&self.r_assign),
            }
        }
    }
    pub struct OkAssignmentNode(Rc<CoreOkAssignmentNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for OkAssignmentNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OkAssignmentNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for OkAssignmentNode {
        #[inline]
        fn clone(&self) -> OkAssignmentNode {
            OkAssignmentNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl OkAssignmentNode {
        pub fn new(l_atom: &AtomNode, r_assign: &RAssignmentNode, equal: &TokenNode) -> Self {
            let node = Rc::new(CoreOkAssignmentNode {
                equal: equal.clone(),
                l_atom: l_atom.clone(),
                r_assign: r_assign.clone(),
            });
            OkAssignmentNode(node)
        }
    }
    impl Node for OkAssignmentNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().l_atom.range().start()),
                TextSize::from(self.0.as_ref().r_assign.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().l_atom.start_line_number()
        }
    }
    pub struct CoreInvalidLValueNode {
        pub l_expr: ExpressionNode,
        pub equal: TokenNode,
        pub r_assign: RAssignmentNode,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreInvalidLValueNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "CoreInvalidLValueNode",
                "l_expr",
                &&self.l_expr,
                "equal",
                &&self.equal,
                "r_assign",
                &&self.r_assign,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreInvalidLValueNode {
        #[inline]
        fn clone(&self) -> CoreInvalidLValueNode {
            CoreInvalidLValueNode {
                l_expr: ::core::clone::Clone::clone(&self.l_expr),
                equal: ::core::clone::Clone::clone(&self.equal),
                r_assign: ::core::clone::Clone::clone(&self.r_assign),
            }
        }
    }
    pub struct InvalidLValueNode(Rc<CoreInvalidLValueNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for InvalidLValueNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "InvalidLValueNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for InvalidLValueNode {
        #[inline]
        fn clone(&self) -> InvalidLValueNode {
            InvalidLValueNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl InvalidLValueNode {
        pub fn new(l_expr: &ExpressionNode, r_assign: &RAssignmentNode, equal: &TokenNode) -> Self {
            let node = Rc::new(CoreInvalidLValueNode {
                l_expr: l_expr.clone(),
                equal: equal.clone(),
                r_assign: r_assign.clone(),
            });
            InvalidLValueNode(node)
        }
    }
    impl Node for InvalidLValueNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().l_expr.range().start()),
                TextSize::from(self.0.as_ref().r_assign.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().l_expr.start_line_number()
        }
    }
    pub struct CoreStructStatementNode {
        pub newline: TokenNode,
        pub name_type_spec: NameTypeSpecNode,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreStructStatementNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreStructStatementNode",
                "newline",
                &&self.newline,
                "name_type_spec",
                &&self.name_type_spec,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreStructStatementNode {
        #[inline]
        fn clone(&self) -> CoreStructStatementNode {
            CoreStructStatementNode {
                newline: ::core::clone::Clone::clone(&self.newline),
                name_type_spec: ::core::clone::Clone::clone(&self.name_type_spec),
            }
        }
    }
    pub struct StructStatementNode(Rc<CoreStructStatementNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for StructStatementNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "StructStatementNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for StructStatementNode {
        #[inline]
        fn clone(&self) -> StructStatementNode {
            StructStatementNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl StructStatementNode {
        pub fn new(
            param_name: &TokenNode,
            param_type: &TypeExpressionNode,
            colon: &TokenNode,
            newline: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreStructStatementNode {
                newline: newline.clone(),
                name_type_spec: NameTypeSpecNode::new(param_name, param_type, colon),
            });
            StructStatementNode(node)
        }
    }
    impl Node for StructStatementNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().name_type_spec.range().start()),
                TextSize::from(self.0.as_ref().newline.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().name_type_spec.start_line_number()
        }
    }
    pub enum CoreTypeDeclarationNode {
        STRUCT(StructDeclarationNode),
        LAMBDA(LambdaDeclarationNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreTypeDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreTypeDeclarationNode::STRUCT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "STRUCT", &__self_0)
                }
                CoreTypeDeclarationNode::LAMBDA(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "LAMBDA", &__self_0)
                }
                CoreTypeDeclarationNode::MISSING_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "MISSING_TOKENS",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreTypeDeclarationNode {
        #[inline]
        fn clone(&self) -> CoreTypeDeclarationNode {
            match self {
                CoreTypeDeclarationNode::STRUCT(__self_0) => {
                    CoreTypeDeclarationNode::STRUCT(::core::clone::Clone::clone(__self_0))
                }
                CoreTypeDeclarationNode::LAMBDA(__self_0) => {
                    CoreTypeDeclarationNode::LAMBDA(::core::clone::Clone::clone(__self_0))
                }
                CoreTypeDeclarationNode::MISSING_TOKENS(__self_0) => {
                    CoreTypeDeclarationNode::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct TypeDeclarationNode(Rc<CoreTypeDeclarationNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for TypeDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "TypeDeclarationNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for TypeDeclarationNode {
        #[inline]
        fn clone(&self) -> TypeDeclarationNode {
            TypeDeclarationNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl TypeDeclarationNode {
        pub fn new_with_struct(
            name: &TokenNode,
            block: &BlockNode,
            type_keyword: &TokenNode,
            colon: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreTypeDeclarationNode::STRUCT(StructDeclarationNode::new(
                name,
                block,
                type_keyword,
                colon,
            )));
            TypeDeclarationNode(node)
        }
        pub fn new_with_lambda(lambda: &LambdaDeclarationNode) -> Self {
            let node = Rc::new(CoreTypeDeclarationNode::LAMBDA(lambda.clone()));
            TypeDeclarationNode(node)
        }
    }
    impl Node for TypeDeclarationNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref() {
                CoreTypeDeclarationNode::STRUCT(struct_decl) => TextRange::new(
                    TextSize::from(struct_decl.range().start()),
                    TextSize::from(struct_decl.range().end()),
                ),
                CoreTypeDeclarationNode::LAMBDA(lambda_decl) => TextRange::new(
                    TextSize::from(lambda_decl.range().start()),
                    TextSize::from(lambda_decl.range().end()),
                ),
                CoreTypeDeclarationNode::MISSING_TOKENS(missing_tokens) => TextRange::new(
                    TextSize::from(missing_tokens.range().start()),
                    TextSize::from(missing_tokens.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref() {
                CoreTypeDeclarationNode::STRUCT(struct_decl) => struct_decl.start_line_number(),
                CoreTypeDeclarationNode::LAMBDA(lambda_decl) => lambda_decl.start_line_number(),
                CoreTypeDeclarationNode::MISSING_TOKENS(missing_tokens) => {
                    missing_tokens.start_line_number()
                }
            }
        }
    }
    impl ErrornousNode for TypeDeclarationNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
        ) -> Self {
            TypeDeclarationNode(Rc::new(CoreTypeDeclarationNode::MISSING_TOKENS(
                MissingTokenNode::new(expected_symbols, received_token),
            )))
        }
    }
    pub struct CoreStructDeclarationNode {
        pub type_keyword: TokenNode,
        pub colon: TokenNode,
        pub name: TokenNode,
        pub block: BlockNode,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreStructDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreStructDeclarationNode",
                "type_keyword",
                &&self.type_keyword,
                "colon",
                &&self.colon,
                "name",
                &&self.name,
                "block",
                &&self.block,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreStructDeclarationNode {
        #[inline]
        fn clone(&self) -> CoreStructDeclarationNode {
            CoreStructDeclarationNode {
                type_keyword: ::core::clone::Clone::clone(&self.type_keyword),
                colon: ::core::clone::Clone::clone(&self.colon),
                name: ::core::clone::Clone::clone(&self.name),
                block: ::core::clone::Clone::clone(&self.block),
            }
        }
    }
    pub struct StructDeclarationNode(Rc<CoreStructDeclarationNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for StructDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "StructDeclarationNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for StructDeclarationNode {
        #[inline]
        fn clone(&self) -> StructDeclarationNode {
            StructDeclarationNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl StructDeclarationNode {
        pub fn new(
            name: &TokenNode,
            block: &BlockNode,
            type_keyword: &TokenNode,
            colon: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreStructDeclarationNode {
                type_keyword: type_keyword.clone(),
                colon: colon.clone(),
                name: name.clone(),
                block: block.clone(),
            });
            StructDeclarationNode(node)
        }
    }
    impl Node for StructDeclarationNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().type_keyword.range().start()),
                TextSize::from(self.0.as_ref().block.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().type_keyword.start_line_number()
        }
    }
    pub enum CoreLambdaDeclarationNode {
        OK(OkLambdaDeclarationNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreLambdaDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreLambdaDeclarationNode::OK(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK", &__self_0)
                }
                CoreLambdaDeclarationNode::MISSING_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "MISSING_TOKENS",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreLambdaDeclarationNode {
        #[inline]
        fn clone(&self) -> CoreLambdaDeclarationNode {
            match self {
                CoreLambdaDeclarationNode::OK(__self_0) => {
                    CoreLambdaDeclarationNode::OK(::core::clone::Clone::clone(__self_0))
                }
                CoreLambdaDeclarationNode::MISSING_TOKENS(__self_0) => {
                    CoreLambdaDeclarationNode::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    impl Node for LambdaDeclarationNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref() {
                CoreLambdaDeclarationNode::OK(x) => TextRange::new(
                    TextSize::from(x.range().start()),
                    TextSize::from(x.range().end()),
                ),
                CoreLambdaDeclarationNode::MISSING_TOKENS(x) => TextRange::new(
                    TextSize::from(x.range().start()),
                    TextSize::from(x.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref() {
                CoreLambdaDeclarationNode::OK(x) => x.start_line_number(),
                CoreLambdaDeclarationNode::MISSING_TOKENS(x) => x.start_line_number(),
            }
        }
    }
    pub struct LambdaDeclarationNode(Rc<CoreLambdaDeclarationNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for LambdaDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "LambdaDeclarationNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for LambdaDeclarationNode {
        #[inline]
        fn clone(&self) -> LambdaDeclarationNode {
            LambdaDeclarationNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl LambdaDeclarationNode {
        pub fn new(
            name: &TokenNode,
            args: Option<&NameTypeSpecsNode>,
            return_type: Option<&TypeExpressionNode>,
            type_keyword: &TokenNode,
            colon: &TokenNode,
            lparen: &TokenNode,
            rparen: &TokenNode,
            right_arrow: Option<&TokenNode>,
            newline: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreLambdaDeclarationNode::OK(OkLambdaDeclarationNode::new(
                name,
                args,
                return_type,
                type_keyword,
                colon,
                lparen,
                rparen,
                right_arrow,
                newline,
            )));
            LambdaDeclarationNode(node)
        }
    }
    impl ErrornousNode for LambdaDeclarationNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
        ) -> Self {
            LambdaDeclarationNode(Rc::new(CoreLambdaDeclarationNode::MISSING_TOKENS(
                MissingTokenNode::new(expected_symbols, received_token),
            )))
        }
    }
    pub struct CoreOkLambdaDeclarationNode {
        pub type_keyword: TokenNode,
        pub colon: TokenNode,
        pub lparen: TokenNode,
        pub rparen: TokenNode,
        pub right_arrow: Option<TokenNode>,
        pub newline: TokenNode,
        pub name: TokenNode,
        pub args: Option<NameTypeSpecsNode>,
        pub return_type: Option<TypeExpressionNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreOkLambdaDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            let names: &'static _ = &[
                "type_keyword",
                "colon",
                "lparen",
                "rparen",
                "right_arrow",
                "newline",
                "name",
                "args",
                "return_type",
            ];
            let values: &[&dyn ::core::fmt::Debug] = &[
                &&self.type_keyword,
                &&self.colon,
                &&self.lparen,
                &&self.rparen,
                &&self.right_arrow,
                &&self.newline,
                &&self.name,
                &&self.args,
                &&self.return_type,
            ];
            ::core::fmt::Formatter::debug_struct_fields_finish(
                f,
                "CoreOkLambdaDeclarationNode",
                names,
                values,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreOkLambdaDeclarationNode {
        #[inline]
        fn clone(&self) -> CoreOkLambdaDeclarationNode {
            CoreOkLambdaDeclarationNode {
                type_keyword: ::core::clone::Clone::clone(&self.type_keyword),
                colon: ::core::clone::Clone::clone(&self.colon),
                lparen: ::core::clone::Clone::clone(&self.lparen),
                rparen: ::core::clone::Clone::clone(&self.rparen),
                right_arrow: ::core::clone::Clone::clone(&self.right_arrow),
                newline: ::core::clone::Clone::clone(&self.newline),
                name: ::core::clone::Clone::clone(&self.name),
                args: ::core::clone::Clone::clone(&self.args),
                return_type: ::core::clone::Clone::clone(&self.return_type),
            }
        }
    }
    pub struct OkLambdaDeclarationNode(Rc<CoreOkLambdaDeclarationNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for OkLambdaDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "OkLambdaDeclarationNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for OkLambdaDeclarationNode {
        #[inline]
        fn clone(&self) -> OkLambdaDeclarationNode {
            OkLambdaDeclarationNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl OkLambdaDeclarationNode {
        pub fn new(
            name: &TokenNode,
            args: Option<&NameTypeSpecsNode>,
            return_type: Option<&TypeExpressionNode>,
            type_keyword: &TokenNode,
            colon: &TokenNode,
            lparen: &TokenNode,
            rparen: &TokenNode,
            right_arrow: Option<&TokenNode>,
            newline: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreOkLambdaDeclarationNode {
                lparen: lparen.clone(),
                rparen: rparen.clone(),
                right_arrow: match right_arrow {
                    Some(val) => Some(val.clone()),
                    None => None,
                },
                newline: newline.clone(),
                type_keyword: type_keyword.clone(),
                colon: colon.clone(),
                name: name.clone(),
                args: match args {
                    Some(val) => Some(val.clone()),
                    None => None,
                },
                return_type: match return_type {
                    Some(val) => Some(val.clone()),
                    None => None,
                },
            });
            OkLambdaDeclarationNode(node)
        }
    }
    impl Node for OkLambdaDeclarationNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().type_keyword.range().start()),
                TextSize::from(self.0.as_ref().newline.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().type_keyword.start_line_number()
        }
    }
    pub enum CoreFunctionDeclarationNode {
        OK(OkFunctionDeclarationNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreFunctionDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreFunctionDeclarationNode::OK(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK", &__self_0)
                }
                CoreFunctionDeclarationNode::MISSING_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "MISSING_TOKENS",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreFunctionDeclarationNode {
        #[inline]
        fn clone(&self) -> CoreFunctionDeclarationNode {
            match self {
                CoreFunctionDeclarationNode::OK(__self_0) => {
                    CoreFunctionDeclarationNode::OK(::core::clone::Clone::clone(__self_0))
                }
                CoreFunctionDeclarationNode::MISSING_TOKENS(__self_0) => {
                    CoreFunctionDeclarationNode::MISSING_TOKENS(::core::clone::Clone::clone(
                        __self_0,
                    ))
                }
            }
        }
    }
    pub struct FunctionDeclarationNode(Rc<CoreFunctionDeclarationNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for FunctionDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "FunctionDeclarationNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for FunctionDeclarationNode {
        #[inline]
        fn clone(&self) -> FunctionDeclarationNode {
            FunctionDeclarationNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl FunctionDeclarationNode {
        pub fn new(
            name: Option<&TokenNode>,
            args: Option<&NameTypeSpecsNode>,
            return_type: Option<&TypeExpressionNode>,
            block: &BlockNode,
            func_keyword: &FuncKeywordKind,
            lparen: &TokenNode,
            rparen: &TokenNode,
            right_arrow: Option<&TokenNode>,
            colon: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreFunctionDeclarationNode::OK(
                OkFunctionDeclarationNode::new(
                    name,
                    args,
                    return_type,
                    block,
                    func_keyword,
                    lparen,
                    rparen,
                    right_arrow,
                    colon,
                ),
            ));
            FunctionDeclarationNode(node)
        }
    }
    impl Node for FunctionDeclarationNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref() {
                CoreFunctionDeclarationNode::OK(ok_func_decl) => TextRange::new(
                    TextSize::from(ok_func_decl.range().start()),
                    TextSize::from(ok_func_decl.range().end()),
                ),
                CoreFunctionDeclarationNode::MISSING_TOKENS(missing_tokens) => TextRange::new(
                    TextSize::from(missing_tokens.range().start()),
                    TextSize::from(missing_tokens.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref() {
                CoreFunctionDeclarationNode::OK(ok_func_decl) => ok_func_decl.start_line_number(),
                CoreFunctionDeclarationNode::MISSING_TOKENS(missing_tokens) => {
                    missing_tokens.start_line_number()
                }
            }
        }
    }
    impl ErrornousNode for FunctionDeclarationNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
        ) -> Self {
            FunctionDeclarationNode(Rc::new(CoreFunctionDeclarationNode::MISSING_TOKENS(
                MissingTokenNode::new(expected_symbols, received_token),
            )))
        }
    }
    pub struct CoreOkFunctionDeclarationNode {
        pub func_keyword: FuncKeywordKind,
        pub lparen: TokenNode,
        pub rparen: TokenNode,
        pub right_arrow: Option<TokenNode>,
        pub colon: TokenNode,
        pub name: Option<TokenNode>,
        pub args: Option<NameTypeSpecsNode>,
        pub return_type: Option<TypeExpressionNode>,
        pub block: BlockNode,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreOkFunctionDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            let names: &'static _ = &[
                "func_keyword",
                "lparen",
                "rparen",
                "right_arrow",
                "colon",
                "name",
                "args",
                "return_type",
                "block",
            ];
            let values: &[&dyn ::core::fmt::Debug] = &[
                &&self.func_keyword,
                &&self.lparen,
                &&self.rparen,
                &&self.right_arrow,
                &&self.colon,
                &&self.name,
                &&self.args,
                &&self.return_type,
                &&self.block,
            ];
            ::core::fmt::Formatter::debug_struct_fields_finish(
                f,
                "CoreOkFunctionDeclarationNode",
                names,
                values,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreOkFunctionDeclarationNode {
        #[inline]
        fn clone(&self) -> CoreOkFunctionDeclarationNode {
            CoreOkFunctionDeclarationNode {
                func_keyword: ::core::clone::Clone::clone(&self.func_keyword),
                lparen: ::core::clone::Clone::clone(&self.lparen),
                rparen: ::core::clone::Clone::clone(&self.rparen),
                right_arrow: ::core::clone::Clone::clone(&self.right_arrow),
                colon: ::core::clone::Clone::clone(&self.colon),
                name: ::core::clone::Clone::clone(&self.name),
                args: ::core::clone::Clone::clone(&self.args),
                return_type: ::core::clone::Clone::clone(&self.return_type),
                block: ::core::clone::Clone::clone(&self.block),
            }
        }
    }
    pub enum FuncKeywordKind {
        DEF(TokenNode),
        FUNC(TokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for FuncKeywordKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                FuncKeywordKind::DEF(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "DEF", &__self_0)
                }
                FuncKeywordKind::FUNC(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "FUNC", &__self_0)
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for FuncKeywordKind {
        #[inline]
        fn clone(&self) -> FuncKeywordKind {
            match self {
                FuncKeywordKind::DEF(__self_0) => {
                    FuncKeywordKind::DEF(::core::clone::Clone::clone(__self_0))
                }
                FuncKeywordKind::FUNC(__self_0) => {
                    FuncKeywordKind::FUNC(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct OkFunctionDeclarationNode(Rc<CoreOkFunctionDeclarationNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for OkFunctionDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "OkFunctionDeclarationNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for OkFunctionDeclarationNode {
        #[inline]
        fn clone(&self) -> OkFunctionDeclarationNode {
            OkFunctionDeclarationNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl OkFunctionDeclarationNode {
        pub fn new(
            name: Option<&TokenNode>,
            args: Option<&NameTypeSpecsNode>,
            return_type: Option<&TypeExpressionNode>,
            block: &BlockNode,
            func_keyword: &FuncKeywordKind,
            lparen: &TokenNode,
            rparen: &TokenNode,
            right_arrow: Option<&TokenNode>,
            colon: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreOkFunctionDeclarationNode {
                func_keyword: func_keyword.clone(),
                lparen: lparen.clone(),
                rparen: rparen.clone(),
                right_arrow: match right_arrow {
                    Some(val) => Some(val.clone()),
                    None => None,
                },
                colon: colon.clone(),
                name: match name {
                    Some(val) => Some(val.clone()),
                    None => None,
                },
                args: match args {
                    Some(val) => Some(val.clone()),
                    None => None,
                },
                return_type: match return_type {
                    Some(val) => Some(val.clone()),
                    None => None,
                },
                block: block.clone(),
            });
            OkFunctionDeclarationNode(node)
        }
    }
    impl Node for OkFunctionDeclarationNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref().func_keyword {
                FuncKeywordKind::DEF(token) => TextRange::new(
                    TextSize::from(token.range().start()),
                    TextSize::from(self.0.as_ref().block.range().end()),
                ),
                FuncKeywordKind::FUNC(token) => TextRange::new(
                    TextSize::from(token.range().start()),
                    TextSize::from(self.0.as_ref().block.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref().func_keyword {
                FuncKeywordKind::DEF(token) => token.start_line_number(),
                FuncKeywordKind::FUNC(token) => token.start_line_number(),
            }
        }
    }
    pub struct CoreVariableDeclarationNode {
        pub let_keyword: TokenNode,
        pub equal: TokenNode,
        pub name: TokenNode,
        pub r_assign: RAssignmentNode,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreVariableDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreVariableDeclarationNode",
                "let_keyword",
                &&self.let_keyword,
                "equal",
                &&self.equal,
                "name",
                &&self.name,
                "r_assign",
                &&self.r_assign,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreVariableDeclarationNode {
        #[inline]
        fn clone(&self) -> CoreVariableDeclarationNode {
            CoreVariableDeclarationNode {
                let_keyword: ::core::clone::Clone::clone(&self.let_keyword),
                equal: ::core::clone::Clone::clone(&self.equal),
                name: ::core::clone::Clone::clone(&self.name),
                r_assign: ::core::clone::Clone::clone(&self.r_assign),
            }
        }
    }
    pub struct VariableDeclarationNode(Rc<CoreVariableDeclarationNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for VariableDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "VariableDeclarationNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for VariableDeclarationNode {
        #[inline]
        fn clone(&self) -> VariableDeclarationNode {
            VariableDeclarationNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl VariableDeclarationNode {
        pub fn new(
            name: &TokenNode,
            r_assign: &RAssignmentNode,
            let_keyword: &TokenNode,
            equal: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreVariableDeclarationNode {
                let_keyword: let_keyword.clone(),
                equal: equal.clone(),
                name: name.clone(),
                r_assign: r_assign.clone(),
            });
            VariableDeclarationNode(node)
        }
    }
    impl Node for VariableDeclarationNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().let_keyword.range().start()),
                TextSize::from(self.0.as_ref().r_assign.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().let_keyword.start_line_number()
        }
    }
    pub enum CoreNameTypeSpecsNode {
        OK(OkNameTypeSpecsNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreNameTypeSpecsNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreNameTypeSpecsNode::OK(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK", &__self_0)
                }
                CoreNameTypeSpecsNode::MISSING_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "MISSING_TOKENS",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreNameTypeSpecsNode {
        #[inline]
        fn clone(&self) -> CoreNameTypeSpecsNode {
            match self {
                CoreNameTypeSpecsNode::OK(__self_0) => {
                    CoreNameTypeSpecsNode::OK(::core::clone::Clone::clone(__self_0))
                }
                CoreNameTypeSpecsNode::MISSING_TOKENS(__self_0) => {
                    CoreNameTypeSpecsNode::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct NameTypeSpecsNode(Rc<CoreNameTypeSpecsNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for NameTypeSpecsNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "NameTypeSpecsNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for NameTypeSpecsNode {
        #[inline]
        fn clone(&self) -> NameTypeSpecsNode {
            NameTypeSpecsNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl NameTypeSpecsNode {
        pub fn new(ok_name_type_specs: &OkNameTypeSpecsNode) -> Self {
            let node = Rc::new(CoreNameTypeSpecsNode::OK(ok_name_type_specs.clone()));
            NameTypeSpecsNode(node)
        }
    }
    impl Node for NameTypeSpecsNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref() {
                CoreNameTypeSpecsNode::OK(ok_name_type_specs) => TextRange::new(
                    TextSize::from(ok_name_type_specs.range().start()),
                    TextSize::from(ok_name_type_specs.range().end()),
                ),
                CoreNameTypeSpecsNode::MISSING_TOKENS(missing_tokens) => TextRange::new(
                    TextSize::from(missing_tokens.range().start()),
                    TextSize::from(missing_tokens.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref() {
                CoreNameTypeSpecsNode::OK(ok_name_type_specs) => {
                    ok_name_type_specs.start_line_number()
                }
                CoreNameTypeSpecsNode::MISSING_TOKENS(missing_tokens) => {
                    missing_tokens.start_line_number()
                }
            }
        }
    }
    impl ErrornousNode for NameTypeSpecsNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
        ) -> Self {
            NameTypeSpecsNode(Rc::new(CoreNameTypeSpecsNode::MISSING_TOKENS(
                MissingTokenNode::new(expected_symbols, received_token),
            )))
        }
    }
    pub struct CoreOkNameTypeSpecsNode {
        pub comma: Option<TokenNode>,
        pub arg: NameTypeSpecNode,
        pub remaining_args: Option<NameTypeSpecsNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreOkNameTypeSpecsNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "CoreOkNameTypeSpecsNode",
                "comma",
                &&self.comma,
                "arg",
                &&self.arg,
                "remaining_args",
                &&self.remaining_args,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreOkNameTypeSpecsNode {
        #[inline]
        fn clone(&self) -> CoreOkNameTypeSpecsNode {
            CoreOkNameTypeSpecsNode {
                comma: ::core::clone::Clone::clone(&self.comma),
                arg: ::core::clone::Clone::clone(&self.arg),
                remaining_args: ::core::clone::Clone::clone(&self.remaining_args),
            }
        }
    }
    pub struct OkNameTypeSpecsNode(Rc<CoreOkNameTypeSpecsNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for OkNameTypeSpecsNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OkNameTypeSpecsNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for OkNameTypeSpecsNode {
        #[inline]
        fn clone(&self) -> OkNameTypeSpecsNode {
            OkNameTypeSpecsNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl OkNameTypeSpecsNode {
        pub fn new_with_args(
            arg: &NameTypeSpecNode,
            remaining_args: &NameTypeSpecsNode,
            comma: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreOkNameTypeSpecsNode {
                comma: Some(comma.clone()),
                arg: arg.clone(),
                remaining_args: Some(remaining_args.clone()),
            });
            OkNameTypeSpecsNode(node)
        }
        pub fn new_with_single_arg(arg: &NameTypeSpecNode) -> Self {
            let node = Rc::new(CoreOkNameTypeSpecsNode {
                comma: None,
                arg: arg.clone(),
                remaining_args: None,
            });
            OkNameTypeSpecsNode(node)
        }
    }
    impl Node for OkNameTypeSpecsNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref().remaining_args {
                Some(remaining_args) => TextRange::new(
                    TextSize::from(self.0.as_ref().arg.range().start()),
                    TextSize::from(remaining_args.range().end()),
                ),
                None => TextRange::new(
                    TextSize::from(self.0.as_ref().arg.range().start()),
                    TextSize::from(self.0.as_ref().arg.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().arg.start_line_number()
        }
    }
    pub struct CoreNameTypeSpecNode {
        pub colon: TokenNode,
        pub name: TokenNode,
        pub data_type: TypeExpressionNode,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreNameTypeSpecNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "CoreNameTypeSpecNode",
                "colon",
                &&self.colon,
                "name",
                &&self.name,
                "data_type",
                &&self.data_type,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreNameTypeSpecNode {
        #[inline]
        fn clone(&self) -> CoreNameTypeSpecNode {
            CoreNameTypeSpecNode {
                colon: ::core::clone::Clone::clone(&self.colon),
                name: ::core::clone::Clone::clone(&self.name),
                data_type: ::core::clone::Clone::clone(&self.data_type),
            }
        }
    }
    pub struct NameTypeSpecNode(Rc<CoreNameTypeSpecNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for NameTypeSpecNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "NameTypeSpecNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for NameTypeSpecNode {
        #[inline]
        fn clone(&self) -> NameTypeSpecNode {
            NameTypeSpecNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl NameTypeSpecNode {
        pub fn new(
            param_name: &TokenNode,
            param_type: &TypeExpressionNode,
            colon: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreNameTypeSpecNode {
                colon: colon.clone(),
                name: param_name.clone(),
                data_type: param_type.clone(),
            });
            NameTypeSpecNode(node)
        }
    }
    impl Node for NameTypeSpecNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().name.range().start()),
                TextSize::from(self.0.as_ref().data_type.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().name.start_line_number()
        }
    }
    pub enum CoreTypeExpressionNode {
        ATOMIC(AtomicTypeNode),
        USER_DEFINED(UserDefinedTypeNode),
        ARRAY(ArrayTypeNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreTypeExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreTypeExpressionNode::ATOMIC(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ATOMIC", &__self_0)
                }
                CoreTypeExpressionNode::USER_DEFINED(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "USER_DEFINED", &__self_0)
                }
                CoreTypeExpressionNode::ARRAY(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ARRAY", &__self_0)
                }
                CoreTypeExpressionNode::MISSING_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "MISSING_TOKENS",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreTypeExpressionNode {
        #[inline]
        fn clone(&self) -> CoreTypeExpressionNode {
            match self {
                CoreTypeExpressionNode::ATOMIC(__self_0) => {
                    CoreTypeExpressionNode::ATOMIC(::core::clone::Clone::clone(__self_0))
                }
                CoreTypeExpressionNode::USER_DEFINED(__self_0) => {
                    CoreTypeExpressionNode::USER_DEFINED(::core::clone::Clone::clone(__self_0))
                }
                CoreTypeExpressionNode::ARRAY(__self_0) => {
                    CoreTypeExpressionNode::ARRAY(::core::clone::Clone::clone(__self_0))
                }
                CoreTypeExpressionNode::MISSING_TOKENS(__self_0) => {
                    CoreTypeExpressionNode::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct TypeExpressionNode(Rc<CoreTypeExpressionNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for TypeExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "TypeExpressionNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for TypeExpressionNode {
        #[inline]
        fn clone(&self) -> TypeExpressionNode {
            TypeExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl TypeExpressionNode {
        pub fn new_with_atomic_type(atomic_type: &TokenNode) -> Self {
            let node = Rc::new(CoreTypeExpressionNode::ATOMIC(AtomicTypeNode::new(
                atomic_type,
            )));
            TypeExpressionNode(node)
        }
        pub fn new_with_user_defined_type(identifier: &TokenNode) -> Self {
            let node = Rc::new(CoreTypeExpressionNode::USER_DEFINED(
                UserDefinedTypeNode::new(identifier),
            ));
            TypeExpressionNode(node)
        }
        pub fn new_with_array_type(
            array_size: &TokenNode,
            sub_type: &TypeExpressionNode,
            lsquare: &TokenNode,
            rsquare: &TokenNode,
            semicolon: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreTypeExpressionNode::ARRAY(ArrayTypeNode::new(
                array_size, sub_type, lsquare, rsquare, semicolon,
            )));
            TypeExpressionNode(node)
        }
        pub fn get_type_obj(&self, code: &Code) -> Option<Type> {
            match &self.0.as_ref() {
                CoreTypeExpressionNode::ATOMIC(atomic_type) => atomic_type.get_type_obj(code),
                CoreTypeExpressionNode::USER_DEFINED(user_defined_type) => {
                    user_defined_type.get_type_obj(code)
                }
                CoreTypeExpressionNode::ARRAY(array_type) => array_type.get_type_obj(code),
                _ => None,
            }
        }
    }
    impl Node for TypeExpressionNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref() {
                CoreTypeExpressionNode::ATOMIC(atomic) => TextRange::new(
                    TextSize::from(atomic.range().start()),
                    TextSize::from(atomic.range().end()),
                ),
                CoreTypeExpressionNode::USER_DEFINED(user_defined) => TextRange::new(
                    TextSize::from(user_defined.range().start()),
                    TextSize::from(user_defined.range().end()),
                ),
                CoreTypeExpressionNode::ARRAY(array) => TextRange::new(
                    TextSize::from(array.range().start()),
                    TextSize::from(array.range().end()),
                ),
                CoreTypeExpressionNode::MISSING_TOKENS(missing_tokens) => TextRange::new(
                    TextSize::from(missing_tokens.range().start()),
                    TextSize::from(missing_tokens.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref() {
                CoreTypeExpressionNode::ATOMIC(atomic) => atomic.start_line_number(),
                CoreTypeExpressionNode::USER_DEFINED(user_defined) => {
                    user_defined.start_line_number()
                }
                CoreTypeExpressionNode::ARRAY(array) => array.start_line_number(),
                CoreTypeExpressionNode::MISSING_TOKENS(missing_tokens) => {
                    missing_tokens.start_line_number()
                }
            }
        }
    }
    impl ErrornousNode for TypeExpressionNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
        ) -> Self {
            TypeExpressionNode(Rc::new(CoreTypeExpressionNode::MISSING_TOKENS(
                MissingTokenNode::new(expected_symbols, received_token),
            )))
        }
    }
    pub struct CoreAtomicTypeNode {
        pub kind: TokenNode,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreAtomicTypeNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "CoreAtomicTypeNode",
                "kind",
                &&self.kind,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreAtomicTypeNode {
        #[inline]
        fn clone(&self) -> CoreAtomicTypeNode {
            CoreAtomicTypeNode {
                kind: ::core::clone::Clone::clone(&self.kind),
            }
        }
    }
    pub struct AtomicTypeNode(Rc<CoreAtomicTypeNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for AtomicTypeNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "AtomicTypeNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for AtomicTypeNode {
        #[inline]
        fn clone(&self) -> AtomicTypeNode {
            AtomicTypeNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl AtomicTypeNode {
        pub fn new(token: &TokenNode) -> Self {
            let node = Rc::new(CoreAtomicTypeNode {
                kind: token.clone(),
            });
            AtomicTypeNode(node)
        }
        pub fn get_type_obj(&self, code: &Code) -> Option<Type> {
            match self.0.as_ref().kind.get_ok() {
                Some(ok_atomic_type) => {
                    let atomic_type_str = ok_atomic_type.token_value(code);
                    return Atomic::new_with_type_str(&atomic_type_str);
                }
                None => return None,
            }
        }
    }
    impl Node for AtomicTypeNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().kind.range().start()),
                TextSize::from(self.0.as_ref().kind.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().kind.start_line_number()
        }
    }
    pub struct CoreArrayTypeNode {
        pub lsquare: TokenNode,
        pub rsquare: TokenNode,
        pub semicolon: TokenNode,
        pub sub_type: TypeExpressionNode,
        pub size: TokenNode,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreArrayTypeNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field5_finish(
                f,
                "CoreArrayTypeNode",
                "lsquare",
                &&self.lsquare,
                "rsquare",
                &&self.rsquare,
                "semicolon",
                &&self.semicolon,
                "sub_type",
                &&self.sub_type,
                "size",
                &&self.size,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreArrayTypeNode {
        #[inline]
        fn clone(&self) -> CoreArrayTypeNode {
            CoreArrayTypeNode {
                lsquare: ::core::clone::Clone::clone(&self.lsquare),
                rsquare: ::core::clone::Clone::clone(&self.rsquare),
                semicolon: ::core::clone::Clone::clone(&self.semicolon),
                sub_type: ::core::clone::Clone::clone(&self.sub_type),
                size: ::core::clone::Clone::clone(&self.size),
            }
        }
    }
    pub struct ArrayTypeNode(Rc<CoreArrayTypeNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for ArrayTypeNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ArrayTypeNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ArrayTypeNode {
        #[inline]
        fn clone(&self) -> ArrayTypeNode {
            ArrayTypeNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl ArrayTypeNode {
        pub fn new(
            size: &TokenNode,
            sub_type: &TypeExpressionNode,
            lsquare: &TokenNode,
            rsquare: &TokenNode,
            semicolon: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreArrayTypeNode {
                lsquare: lsquare.clone(),
                rsquare: rsquare.clone(),
                semicolon: semicolon.clone(),
                sub_type: sub_type.clone(),
                size: size.clone(),
            });
            ArrayTypeNode(node)
        }
        pub fn get_type_obj(&self, code: &Code) -> Option<Type> {
            match self.0.as_ref().sub_type.get_type_obj(code) {
                Some(sub_type_obj) => match self.0.as_ref().size.get_ok() {
                    Some(size) => {
                        let size = match size.token_value(code).parse::<usize>() {
                            Ok(size) => size,
                            Err(_) => return None,
                        };
                        return Some(Array::new(size, sub_type_obj));
                    }
                    None => return None,
                },
                None => return None,
            }
        }
    }
    impl Node for ArrayTypeNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().lsquare.range().start()),
                TextSize::from(self.0.as_ref().rsquare.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().lsquare.start_line_number()
        }
    }
    pub struct CoreUserDefinedTypeNode {
        pub name: TokenNode,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreUserDefinedTypeNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "CoreUserDefinedTypeNode",
                "name",
                &&self.name,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreUserDefinedTypeNode {
        #[inline]
        fn clone(&self) -> CoreUserDefinedTypeNode {
            CoreUserDefinedTypeNode {
                name: ::core::clone::Clone::clone(&self.name),
            }
        }
    }
    pub struct UserDefinedTypeNode(Rc<CoreUserDefinedTypeNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for UserDefinedTypeNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "UserDefinedTypeNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for UserDefinedTypeNode {
        #[inline]
        fn clone(&self) -> UserDefinedTypeNode {
            UserDefinedTypeNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl UserDefinedTypeNode {
        pub fn new(identifier: &TokenNode) -> Self {
            let node = Rc::new(CoreUserDefinedTypeNode {
                name: identifier.clone(),
            });
            UserDefinedTypeNode(node)
        }
        pub fn get_type_obj(&self, code: &Code) -> Option<Type> {
            match self.0.as_ref().name.get_ok() {
                Some(ok_token_node) => {
                    Some(Type::new_with_user_defined(ok_token_node.token_value(code)))
                }
                None => None,
            }
        }
    }
    impl Node for UserDefinedTypeNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().name.range().start()),
                TextSize::from(self.0.as_ref().name.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().name.start_line_number()
        }
    }
    pub enum CoreTokenNode {
        OK(OkTokenNode),
        MISSING_TOKENS(MissingTokenNode),
        SKIPPED(SkippedTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreTokenNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreTokenNode::OK(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK", &__self_0)
                }
                CoreTokenNode::MISSING_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "MISSING_TOKENS",
                        &__self_0,
                    )
                }
                CoreTokenNode::SKIPPED(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "SKIPPED", &__self_0)
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreTokenNode {
        #[inline]
        fn clone(&self) -> CoreTokenNode {
            match self {
                CoreTokenNode::OK(__self_0) => {
                    CoreTokenNode::OK(::core::clone::Clone::clone(__self_0))
                }
                CoreTokenNode::MISSING_TOKENS(__self_0) => {
                    CoreTokenNode::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
                CoreTokenNode::SKIPPED(__self_0) => {
                    CoreTokenNode::SKIPPED(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct TokenNode(Rc<CoreTokenNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for TokenNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "TokenNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for TokenNode {
        #[inline]
        fn clone(&self) -> TokenNode {
            TokenNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl TokenNode {
        pub fn new_with_ok_token(token: &Token, kind: OkTokenKind) -> Self {
            let node = Rc::new(CoreTokenNode::OK(OkTokenNode::new(token, kind)));
            TokenNode(node)
        }
        pub fn new_with_skipped_token(skipped_token: &Token) -> Self {
            let node = Rc::new(CoreTokenNode::SKIPPED(SkippedTokenNode::new(skipped_token)));
            TokenNode(node)
        }
        pub fn is_ok(&self) -> Option<TokenNode> {
            match &self.0.as_ref() {
                CoreTokenNode::OK(_) => Some(self.clone()),
                _ => None,
            }
        }
        pub fn get_ok(&self) -> Option<OkTokenNode> {
            match &self.0.as_ref() {
                CoreTokenNode::OK(ok_token_node) => Some(ok_token_node.clone()),
                _ => None,
            }
        }
        pub fn is_binary_operator(&self) -> Option<BinaryOperatorKind> {
            match &self.0.as_ref() {
                CoreTokenNode::OK(ok_token) => match ok_token.is_binary_operator() {
                    Some(operator) => return Some(operator),
                    None => None,
                },
                _ => None,
            }
        }
    }
    impl Node for TokenNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref() {
                CoreTokenNode::OK(ok_token) => TextRange::new(
                    TextSize::from(ok_token.range().start()),
                    TextSize::from(ok_token.range().end()),
                ),
                CoreTokenNode::MISSING_TOKENS(missing_tokens) => TextRange::new(
                    TextSize::from(missing_tokens.range().start()),
                    TextSize::from(missing_tokens.range().end()),
                ),
                CoreTokenNode::SKIPPED(skipped_tokens) => TextRange::new(
                    TextSize::from(skipped_tokens.range().start()),
                    TextSize::from(skipped_tokens.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref() {
                CoreTokenNode::OK(ok_token) => ok_token.start_line_number(),
                CoreTokenNode::MISSING_TOKENS(missing_tokens) => missing_tokens.start_line_number(),
                CoreTokenNode::SKIPPED(skipped_tokens) => skipped_tokens.start_line_number(),
            }
        }
    }
    impl ErrornousNode for TokenNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
        ) -> Self {
            TokenNode(Rc::new(CoreTokenNode::MISSING_TOKENS(
                MissingTokenNode::new(expected_symbols, received_token),
            )))
        }
    }
    pub struct CoreOkTokenNode {
        pub token: Token,
        pub kind: OkTokenKind,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreOkTokenNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreOkTokenNode",
                "token",
                &&self.token,
                "kind",
                &&self.kind,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreOkTokenNode {
        #[inline]
        fn clone(&self) -> CoreOkTokenNode {
            CoreOkTokenNode {
                token: ::core::clone::Clone::clone(&self.token),
                kind: ::core::clone::Clone::clone(&self.kind),
            }
        }
    }
    pub enum OkTokenKind {
        IDENTIFIER(Option<SymbolData>),
        NON_IDENTIFIER,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for OkTokenKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                OkTokenKind::IDENTIFIER(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "IDENTIFIER", &__self_0)
                }
                OkTokenKind::NON_IDENTIFIER => {
                    ::core::fmt::Formatter::write_str(f, "NON_IDENTIFIER")
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for OkTokenKind {
        #[inline]
        fn clone(&self) -> OkTokenKind {
            match self {
                OkTokenKind::IDENTIFIER(__self_0) => {
                    OkTokenKind::IDENTIFIER(::core::clone::Clone::clone(__self_0))
                }
                OkTokenKind::NON_IDENTIFIER => OkTokenKind::NON_IDENTIFIER,
            }
        }
    }
    pub struct OkTokenNode(Rc<CoreOkTokenNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for OkTokenNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OkTokenNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for OkTokenNode {
        #[inline]
        fn clone(&self) -> OkTokenNode {
            OkTokenNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl OkTokenNode {
        pub fn new(token: &Token, kind: OkTokenKind) -> Self {
            OkTokenNode(Rc::new(CoreOkTokenNode {
                token: token.clone(),
                kind,
            }))
        }
        pub fn is_binary_operator(&self) -> Option<BinaryOperatorKind> {
            match &self.0.as_ref().token.core_token {
                CoreToken::NOT_EQUAL => Some(BinaryOperatorKind::NOT_EQUAL),
                CoreToken::DOUBLE_EQUAL => Some(BinaryOperatorKind::DOUBLE_EQUAL),
                CoreToken::RBRACKET => Some(BinaryOperatorKind::GREATER),
                CoreToken::GREATER_EQUAL => Some(BinaryOperatorKind::GREATER_EQUAL),
                CoreToken::LBRACKET => Some(BinaryOperatorKind::LESS),
                CoreToken::LESS_EQUAL => Some(BinaryOperatorKind::LESS_EQUAL),
                CoreToken::DASH => Some(BinaryOperatorKind::MINUS),
                CoreToken::PLUS => Some(BinaryOperatorKind::PLUS),
                CoreToken::SLASH => Some(BinaryOperatorKind::DIVIDE),
                CoreToken::STAR => Some(BinaryOperatorKind::MULTIPLY),
                CoreToken::AND => Some(BinaryOperatorKind::AND),
                CoreToken::OR => Some(BinaryOperatorKind::OR),
                _ => None,
            }
        }
        pub fn token_value(&self, code: &Code) -> String {
            self.0.as_ref().token.token_value(code)
        }
        pub fn is_identifier(&self) -> bool {
            match self.0.as_ref().kind {
                OkTokenKind::IDENTIFIER(_) => true,
                _ => false,
            }
        }
    }
    impl Node for OkTokenNode {
        fn range(&self) -> TextRange {
            self.0.as_ref().token.range
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().token.line_number
        }
    }
    pub struct CoreMissingTokenNode {
        pub expected_symbols: Rc<Vec<&'static str>>,
        pub received_token: Token,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreMissingTokenNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreMissingTokenNode",
                "expected_symbols",
                &&self.expected_symbols,
                "received_token",
                &&self.received_token,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreMissingTokenNode {
        #[inline]
        fn clone(&self) -> CoreMissingTokenNode {
            CoreMissingTokenNode {
                expected_symbols: ::core::clone::Clone::clone(&self.expected_symbols),
                received_token: ::core::clone::Clone::clone(&self.received_token),
            }
        }
    }
    pub struct MissingTokenNode(Rc<CoreMissingTokenNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for MissingTokenNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "MissingTokenNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for MissingTokenNode {
        #[inline]
        fn clone(&self) -> MissingTokenNode {
            MissingTokenNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl MissingTokenNode {
        pub fn new(expected_symbols: &Rc<Vec<&'static str>>, received_token: &Token) -> Self {
            let node = Rc::new(CoreMissingTokenNode {
                expected_symbols: expected_symbols.clone(),
                received_token: received_token.clone(),
            });
            MissingTokenNode(node)
        }
    }
    impl Node for MissingTokenNode {
        fn range(&self) -> TextRange {
            let received_token = &self.0.as_ref().received_token;
            TextRange::new(received_token.range.start(), received_token.range.start())
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().received_token.line_number
        }
    }
    pub struct CoreSkippedTokenNode {
        pub skipped_token: Token,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreSkippedTokenNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field1_finish(
                f,
                "CoreSkippedTokenNode",
                "skipped_token",
                &&self.skipped_token,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreSkippedTokenNode {
        #[inline]
        fn clone(&self) -> CoreSkippedTokenNode {
            CoreSkippedTokenNode {
                skipped_token: ::core::clone::Clone::clone(&self.skipped_token),
            }
        }
    }
    pub struct SkippedTokenNode(Rc<CoreSkippedTokenNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for SkippedTokenNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "SkippedTokenNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for SkippedTokenNode {
        #[inline]
        fn clone(&self) -> SkippedTokenNode {
            SkippedTokenNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl SkippedTokenNode {
        pub fn new(skipped_token: &Token) -> Self {
            let node = Rc::new(CoreSkippedTokenNode {
                skipped_token: skipped_token.clone(),
            });
            SkippedTokenNode(node)
        }
        pub fn index(&self) -> usize {
            self.0.as_ref().skipped_token.index()
        }
        pub fn line_number(&self) -> usize {
            self.0.as_ref().skipped_token.line_number
        }
    }
    impl Node for SkippedTokenNode {
        fn range(&self) -> TextRange {
            self.0.as_ref().skipped_token.range
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().skipped_token.line_number
        }
    }
    pub enum CoreExpressionNode {
        UNARY(UnaryExpressionNode),
        BINARY(BinaryExpressionNode),
        COMPARISON(ComparisonNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreExpressionNode::UNARY(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "UNARY", &__self_0)
                }
                CoreExpressionNode::BINARY(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "BINARY", &__self_0)
                }
                CoreExpressionNode::COMPARISON(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "COMPARISON", &__self_0)
                }
                CoreExpressionNode::MISSING_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "MISSING_TOKENS",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreExpressionNode {
        #[inline]
        fn clone(&self) -> CoreExpressionNode {
            match self {
                CoreExpressionNode::UNARY(__self_0) => {
                    CoreExpressionNode::UNARY(::core::clone::Clone::clone(__self_0))
                }
                CoreExpressionNode::BINARY(__self_0) => {
                    CoreExpressionNode::BINARY(::core::clone::Clone::clone(__self_0))
                }
                CoreExpressionNode::COMPARISON(__self_0) => {
                    CoreExpressionNode::COMPARISON(::core::clone::Clone::clone(__self_0))
                }
                CoreExpressionNode::MISSING_TOKENS(__self_0) => {
                    CoreExpressionNode::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct ExpressionNode(Rc<CoreExpressionNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for ExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ExpressionNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ExpressionNode {
        #[inline]
        fn clone(&self) -> ExpressionNode {
            ExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl ExpressionNode {
        pub fn new_with_unary(unary_expr: &UnaryExpressionNode) -> Self {
            let node = Rc::new(CoreExpressionNode::UNARY(unary_expr.clone()));
            ExpressionNode(node)
        }
        pub fn new_with_binary(
            operator: &TokenNode,
            left_expr: &ExpressionNode,
            right_expr: &ExpressionNode,
        ) -> Self {
            let operator_kind = match operator.is_binary_operator() {
                Some(operator_kind) => operator_kind,
                None => ::core::panicking::unreachable_display(
                    &"any node passed in this method as operator should be a valid operator",
                ),
            };
            let node = Rc::new(CoreExpressionNode::BINARY(BinaryExpressionNode::new(
                operator_kind,
                operator,
                left_expr,
                right_expr,
            )));
            ExpressionNode(node)
        }
        pub fn new_with_comparison(
            operands: Vec<ExpressionNode>,
            operators: Vec<TokenNode>,
        ) -> Self {
            let node = Rc::new(CoreExpressionNode::COMPARISON(ComparisonNode::new(
                operands, operators,
            )));
            ExpressionNode(node)
        }
        pub fn is_valid_l_value(&self) -> Option<AtomNode> {
            match &self.0.as_ref() {
                CoreExpressionNode::UNARY(unary_expr_node) => match &unary_expr_node.0.as_ref() {
                    CoreUnaryExpressionNode::ATOMIC(atomic_expr_node) => {
                        match &atomic_expr_node.0.as_ref() {
                            CoreAtomicExpressionNode::ATOM(atom_node) => {
                                if atom_node.is_valid_l_value() {
                                    return Some(atom_node.clone());
                                } else {
                                    return None;
                                }
                            }
                            _ => return None,
                        }
                    }
                    _ => return None,
                },
                _ => return None,
            }
        }
    }
    impl Node for ExpressionNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref() {
                CoreExpressionNode::UNARY(unary_expr) => TextRange::new(
                    TextSize::from(unary_expr.range().start()),
                    TextSize::from(unary_expr.range().end()),
                ),
                CoreExpressionNode::BINARY(binary_expr) => TextRange::new(
                    TextSize::from(binary_expr.range().start()),
                    TextSize::from(binary_expr.range().end()),
                ),
                CoreExpressionNode::COMPARISON(comp_expr) => TextRange::new(
                    TextSize::from(comp_expr.range().start()),
                    TextSize::from(comp_expr.range().end()),
                ),
                CoreExpressionNode::MISSING_TOKENS(missing_tokens) => TextRange::new(
                    TextSize::from(missing_tokens.range().start()),
                    TextSize::from(missing_tokens.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref() {
                CoreExpressionNode::UNARY(unary_expr) => unary_expr.start_line_number(),
                CoreExpressionNode::BINARY(binary_expr) => binary_expr.start_line_number(),
                CoreExpressionNode::COMPARISON(comp_expr) => comp_expr.start_line_number(),
                CoreExpressionNode::MISSING_TOKENS(missing_tokens) => {
                    missing_tokens.start_line_number()
                }
            }
        }
    }
    impl ErrornousNode for ExpressionNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
        ) -> Self {
            ExpressionNode(Rc::new(CoreExpressionNode::MISSING_TOKENS(
                MissingTokenNode::new(expected_symbols, received_token),
            )))
        }
    }
    pub struct CoreComparisonNode {
        pub operands: Vec<ExpressionNode>,
        pub operators: Vec<TokenNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreComparisonNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreComparisonNode",
                "operands",
                &&self.operands,
                "operators",
                &&self.operators,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreComparisonNode {
        #[inline]
        fn clone(&self) -> CoreComparisonNode {
            CoreComparisonNode {
                operands: ::core::clone::Clone::clone(&self.operands),
                operators: ::core::clone::Clone::clone(&self.operators),
            }
        }
    }
    pub struct ComparisonNode(Rc<CoreComparisonNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for ComparisonNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ComparisonNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ComparisonNode {
        #[inline]
        fn clone(&self) -> ComparisonNode {
            ComparisonNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl ComparisonNode {
        pub fn new(operands: Vec<ExpressionNode>, operators: Vec<TokenNode>) -> Self {
            let node = Rc::new(CoreComparisonNode {
                operands,
                operators,
            });
            ComparisonNode(node)
        }
    }
    impl Node for ComparisonNode {
        fn range(&self) -> TextRange {
            let core_node = self.0.as_ref();
            TextRange::new(
                TextSize::from(core_node.operands[0].range().start()),
                TextSize::from(
                    core_node.operands[core_node.operands.len() - 1]
                        .range()
                        .end(),
                ),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().operands[0].start_line_number()
        }
    }
    pub enum CoreAtomicExpressionNode {
        BOOL_VALUE(TokenNode),
        INTEGER(TokenNode),
        FLOATING_POINT_NUMBER(TokenNode),
        LITERAL(TokenNode),
        PARENTHESISED_EXPRESSION(ParenthesisedExpressionNode),
        ATOM(AtomNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreAtomicExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreAtomicExpressionNode::BOOL_VALUE(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "BOOL_VALUE", &__self_0)
                }
                CoreAtomicExpressionNode::INTEGER(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "INTEGER", &__self_0)
                }
                CoreAtomicExpressionNode::FLOATING_POINT_NUMBER(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "FLOATING_POINT_NUMBER",
                        &__self_0,
                    )
                }
                CoreAtomicExpressionNode::LITERAL(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "LITERAL", &__self_0)
                }
                CoreAtomicExpressionNode::PARENTHESISED_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "PARENTHESISED_EXPRESSION",
                        &__self_0,
                    )
                }
                CoreAtomicExpressionNode::ATOM(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ATOM", &__self_0)
                }
                CoreAtomicExpressionNode::MISSING_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "MISSING_TOKENS",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreAtomicExpressionNode {
        #[inline]
        fn clone(&self) -> CoreAtomicExpressionNode {
            match self {
                CoreAtomicExpressionNode::BOOL_VALUE(__self_0) => {
                    CoreAtomicExpressionNode::BOOL_VALUE(::core::clone::Clone::clone(__self_0))
                }
                CoreAtomicExpressionNode::INTEGER(__self_0) => {
                    CoreAtomicExpressionNode::INTEGER(::core::clone::Clone::clone(__self_0))
                }
                CoreAtomicExpressionNode::FLOATING_POINT_NUMBER(__self_0) => {
                    CoreAtomicExpressionNode::FLOATING_POINT_NUMBER(::core::clone::Clone::clone(
                        __self_0,
                    ))
                }
                CoreAtomicExpressionNode::LITERAL(__self_0) => {
                    CoreAtomicExpressionNode::LITERAL(::core::clone::Clone::clone(__self_0))
                }
                CoreAtomicExpressionNode::PARENTHESISED_EXPRESSION(__self_0) => {
                    CoreAtomicExpressionNode::PARENTHESISED_EXPRESSION(::core::clone::Clone::clone(
                        __self_0,
                    ))
                }
                CoreAtomicExpressionNode::ATOM(__self_0) => {
                    CoreAtomicExpressionNode::ATOM(::core::clone::Clone::clone(__self_0))
                }
                CoreAtomicExpressionNode::MISSING_TOKENS(__self_0) => {
                    CoreAtomicExpressionNode::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct AtomicExpressionNode(Rc<CoreAtomicExpressionNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for AtomicExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "AtomicExpressionNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for AtomicExpressionNode {
        #[inline]
        fn clone(&self) -> AtomicExpressionNode {
            AtomicExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl AtomicExpressionNode {
        pub fn new_with_bool(bool_value: &TokenNode) -> Self {
            let node = Rc::new(CoreAtomicExpressionNode::BOOL_VALUE(bool_value.clone()));
            AtomicExpressionNode(node)
        }
        pub fn new_with_integer(integer_value: &TokenNode) -> Self {
            let node = Rc::new(CoreAtomicExpressionNode::INTEGER(integer_value.clone()));
            AtomicExpressionNode(node)
        }
        pub fn new_with_floating_point_number(floating_point_value: &TokenNode) -> Self {
            let node = Rc::new(CoreAtomicExpressionNode::FLOATING_POINT_NUMBER(
                floating_point_value.clone(),
            ));
            AtomicExpressionNode(node)
        }
        pub fn new_with_literal(literal_value: &TokenNode) -> Self {
            let node = Rc::new(CoreAtomicExpressionNode::LITERAL(literal_value.clone()));
            AtomicExpressionNode(node)
        }
        pub fn new_with_parenthesised_expr(
            expr: &ExpressionNode,
            lparen: &TokenNode,
            rparen: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreAtomicExpressionNode::PARENTHESISED_EXPRESSION(
                ParenthesisedExpressionNode::new(expr, lparen, rparen),
            ));
            AtomicExpressionNode(node)
        }
        pub fn new_with_atom(atom: &AtomNode) -> Self {
            let node = Rc::new(CoreAtomicExpressionNode::ATOM(atom.clone()));
            AtomicExpressionNode(node)
        }
    }
    impl Node for AtomicExpressionNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref() {
                CoreAtomicExpressionNode::BOOL_VALUE(token) => TextRange::new(
                    TextSize::from(token.range().start()),
                    TextSize::from(token.range().end()),
                ),
                CoreAtomicExpressionNode::INTEGER(token) => TextRange::new(
                    TextSize::from(token.range().start()),
                    TextSize::from(token.range().end()),
                ),
                CoreAtomicExpressionNode::FLOATING_POINT_NUMBER(token) => TextRange::new(
                    TextSize::from(token.range().start()),
                    TextSize::from(token.range().end()),
                ),
                CoreAtomicExpressionNode::LITERAL(token) => TextRange::new(
                    TextSize::from(token.range().start()),
                    TextSize::from(token.range().end()),
                ),
                CoreAtomicExpressionNode::PARENTHESISED_EXPRESSION(parenthesised_expr) => {
                    TextRange::new(
                        TextSize::from(parenthesised_expr.range().start()),
                        TextSize::from(parenthesised_expr.range().end()),
                    )
                }
                CoreAtomicExpressionNode::ATOM(atom) => TextRange::new(
                    TextSize::from(atom.range().start()),
                    TextSize::from(atom.range().end()),
                ),
                CoreAtomicExpressionNode::MISSING_TOKENS(missing_tokens) => TextRange::new(
                    TextSize::from(missing_tokens.range().start()),
                    TextSize::from(missing_tokens.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref() {
                CoreAtomicExpressionNode::BOOL_VALUE(token) => token.start_line_number(),
                CoreAtomicExpressionNode::INTEGER(token) => token.start_line_number(),
                CoreAtomicExpressionNode::FLOATING_POINT_NUMBER(token) => token.start_line_number(),
                CoreAtomicExpressionNode::LITERAL(token) => token.start_line_number(),
                CoreAtomicExpressionNode::PARENTHESISED_EXPRESSION(parenthesised_expr) => {
                    parenthesised_expr.start_line_number()
                }
                CoreAtomicExpressionNode::ATOM(atom) => atom.start_line_number(),
                CoreAtomicExpressionNode::MISSING_TOKENS(missing_tokens) => {
                    missing_tokens.start_line_number()
                }
            }
        }
    }
    impl ErrornousNode for AtomicExpressionNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
        ) -> Self {
            AtomicExpressionNode(Rc::new(CoreAtomicExpressionNode::MISSING_TOKENS(
                MissingTokenNode::new(expected_symbols, received_token),
            )))
        }
    }
    pub struct CoreParenthesisedExpressionNode {
        pub lparen: TokenNode,
        pub rparen: TokenNode,
        pub expr: ExpressionNode,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreParenthesisedExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "CoreParenthesisedExpressionNode",
                "lparen",
                &&self.lparen,
                "rparen",
                &&self.rparen,
                "expr",
                &&self.expr,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreParenthesisedExpressionNode {
        #[inline]
        fn clone(&self) -> CoreParenthesisedExpressionNode {
            CoreParenthesisedExpressionNode {
                lparen: ::core::clone::Clone::clone(&self.lparen),
                rparen: ::core::clone::Clone::clone(&self.rparen),
                expr: ::core::clone::Clone::clone(&self.expr),
            }
        }
    }
    pub struct ParenthesisedExpressionNode(Rc<CoreParenthesisedExpressionNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for ParenthesisedExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "ParenthesisedExpressionNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ParenthesisedExpressionNode {
        #[inline]
        fn clone(&self) -> ParenthesisedExpressionNode {
            ParenthesisedExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl ParenthesisedExpressionNode {
        pub fn new(expr: &ExpressionNode, lparen: &TokenNode, rparen: &TokenNode) -> Self {
            let node = Rc::new(CoreParenthesisedExpressionNode {
                lparen: lparen.clone(),
                rparen: rparen.clone(),
                expr: expr.clone(),
            });
            ParenthesisedExpressionNode(node)
        }
    }
    impl Node for ParenthesisedExpressionNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().lparen.range().start()),
                TextSize::from(self.0.as_ref().rparen.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().lparen.start_line_number()
        }
    }
    pub enum CoreUnaryExpressionNode {
        ATOMIC(AtomicExpressionNode),
        UNARY(OnlyUnaryExpressionNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreUnaryExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreUnaryExpressionNode::ATOMIC(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ATOMIC", &__self_0)
                }
                CoreUnaryExpressionNode::UNARY(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "UNARY", &__self_0)
                }
                CoreUnaryExpressionNode::MISSING_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "MISSING_TOKENS",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreUnaryExpressionNode {
        #[inline]
        fn clone(&self) -> CoreUnaryExpressionNode {
            match self {
                CoreUnaryExpressionNode::ATOMIC(__self_0) => {
                    CoreUnaryExpressionNode::ATOMIC(::core::clone::Clone::clone(__self_0))
                }
                CoreUnaryExpressionNode::UNARY(__self_0) => {
                    CoreUnaryExpressionNode::UNARY(::core::clone::Clone::clone(__self_0))
                }
                CoreUnaryExpressionNode::MISSING_TOKENS(__self_0) => {
                    CoreUnaryExpressionNode::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub enum UnaryOperatorKind {
        PLUS,
        MINUS,
        NOT,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for UnaryOperatorKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                UnaryOperatorKind::PLUS => ::core::fmt::Formatter::write_str(f, "PLUS"),
                UnaryOperatorKind::MINUS => ::core::fmt::Formatter::write_str(f, "MINUS"),
                UnaryOperatorKind::NOT => ::core::fmt::Formatter::write_str(f, "NOT"),
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for UnaryOperatorKind {
        #[inline]
        fn clone(&self) -> UnaryOperatorKind {
            match self {
                UnaryOperatorKind::PLUS => UnaryOperatorKind::PLUS,
                UnaryOperatorKind::MINUS => UnaryOperatorKind::MINUS,
                UnaryOperatorKind::NOT => UnaryOperatorKind::NOT,
            }
        }
    }
    pub struct UnaryExpressionNode(Rc<CoreUnaryExpressionNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for UnaryExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "UnaryExpressionNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for UnaryExpressionNode {
        #[inline]
        fn clone(&self) -> UnaryExpressionNode {
            UnaryExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl UnaryExpressionNode {
        pub fn new_with_atomic(atomic_expr: &AtomicExpressionNode) -> Self {
            let node = Rc::new(CoreUnaryExpressionNode::ATOMIC(atomic_expr.clone()));
            UnaryExpressionNode(node)
        }
        pub fn new_with_unary(
            unary_expr: &UnaryExpressionNode,
            operator: &TokenNode,
            operator_kind: UnaryOperatorKind,
        ) -> Self {
            let node = Rc::new(CoreUnaryExpressionNode::UNARY(
                OnlyUnaryExpressionNode::new(operator, unary_expr, operator_kind),
            ));
            UnaryExpressionNode(node)
        }
    }
    impl Node for UnaryExpressionNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref() {
                CoreUnaryExpressionNode::ATOMIC(atomic) => TextRange::new(
                    TextSize::from(atomic.range().start()),
                    TextSize::from(atomic.range().end()),
                ),
                CoreUnaryExpressionNode::UNARY(only_unary) => TextRange::new(
                    TextSize::from(only_unary.range().start()),
                    TextSize::from(only_unary.range().end()),
                ),
                CoreUnaryExpressionNode::MISSING_TOKENS(missing_tokens) => TextRange::new(
                    TextSize::from(missing_tokens.range().start()),
                    TextSize::from(missing_tokens.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref() {
                CoreUnaryExpressionNode::ATOMIC(atomic) => atomic.start_line_number(),
                CoreUnaryExpressionNode::UNARY(only_unary) => only_unary.start_line_number(),
                CoreUnaryExpressionNode::MISSING_TOKENS(missing_tokens) => {
                    missing_tokens.start_line_number()
                }
            }
        }
    }
    impl ErrornousNode for UnaryExpressionNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
        ) -> Self {
            UnaryExpressionNode(Rc::new(CoreUnaryExpressionNode::MISSING_TOKENS(
                MissingTokenNode::new(expected_symbols, received_token),
            )))
        }
    }
    pub struct CoreOnlyUnaryExpressionNode {
        pub operator: TokenNode,
        pub unary_expr: UnaryExpressionNode,
        pub operator_kind: UnaryOperatorKind,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreOnlyUnaryExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "CoreOnlyUnaryExpressionNode",
                "operator",
                &&self.operator,
                "unary_expr",
                &&self.unary_expr,
                "operator_kind",
                &&self.operator_kind,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreOnlyUnaryExpressionNode {
        #[inline]
        fn clone(&self) -> CoreOnlyUnaryExpressionNode {
            CoreOnlyUnaryExpressionNode {
                operator: ::core::clone::Clone::clone(&self.operator),
                unary_expr: ::core::clone::Clone::clone(&self.unary_expr),
                operator_kind: ::core::clone::Clone::clone(&self.operator_kind),
            }
        }
    }
    pub struct OnlyUnaryExpressionNode(Rc<CoreOnlyUnaryExpressionNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for OnlyUnaryExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "OnlyUnaryExpressionNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for OnlyUnaryExpressionNode {
        #[inline]
        fn clone(&self) -> OnlyUnaryExpressionNode {
            OnlyUnaryExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl OnlyUnaryExpressionNode {
        pub fn new(
            operator: &TokenNode,
            unary_expr: &UnaryExpressionNode,
            operator_kind: UnaryOperatorKind,
        ) -> Self {
            let node = Rc::new(CoreOnlyUnaryExpressionNode {
                operator: operator.clone(),
                unary_expr: unary_expr.clone(),
                operator_kind,
            });
            OnlyUnaryExpressionNode(node)
        }
    }
    impl Node for OnlyUnaryExpressionNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().operator.range().start()),
                TextSize::from(self.0.as_ref().unary_expr.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().operator.start_line_number()
        }
    }
    pub struct CoreBinaryExpressionNode {
        pub operator_kind: BinaryOperatorKind,
        pub operator: TokenNode,
        pub left_expr: ExpressionNode,
        pub right_expr: ExpressionNode,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreBinaryExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreBinaryExpressionNode",
                "operator_kind",
                &&self.operator_kind,
                "operator",
                &&self.operator,
                "left_expr",
                &&self.left_expr,
                "right_expr",
                &&self.right_expr,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreBinaryExpressionNode {
        #[inline]
        fn clone(&self) -> CoreBinaryExpressionNode {
            CoreBinaryExpressionNode {
                operator_kind: ::core::clone::Clone::clone(&self.operator_kind),
                operator: ::core::clone::Clone::clone(&self.operator),
                left_expr: ::core::clone::Clone::clone(&self.left_expr),
                right_expr: ::core::clone::Clone::clone(&self.right_expr),
            }
        }
    }
    pub enum BinaryOperatorKind {
        NOT_EQUAL,
        DOUBLE_EQUAL,
        GREATER,
        GREATER_EQUAL,
        LESS,
        LESS_EQUAL,
        MINUS,
        PLUS,
        DIVIDE,
        MULTIPLY,
        AND,
        OR,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for BinaryOperatorKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                BinaryOperatorKind::NOT_EQUAL => ::core::fmt::Formatter::write_str(f, "NOT_EQUAL"),
                BinaryOperatorKind::DOUBLE_EQUAL => {
                    ::core::fmt::Formatter::write_str(f, "DOUBLE_EQUAL")
                }
                BinaryOperatorKind::GREATER => ::core::fmt::Formatter::write_str(f, "GREATER"),
                BinaryOperatorKind::GREATER_EQUAL => {
                    ::core::fmt::Formatter::write_str(f, "GREATER_EQUAL")
                }
                BinaryOperatorKind::LESS => ::core::fmt::Formatter::write_str(f, "LESS"),
                BinaryOperatorKind::LESS_EQUAL => {
                    ::core::fmt::Formatter::write_str(f, "LESS_EQUAL")
                }
                BinaryOperatorKind::MINUS => ::core::fmt::Formatter::write_str(f, "MINUS"),
                BinaryOperatorKind::PLUS => ::core::fmt::Formatter::write_str(f, "PLUS"),
                BinaryOperatorKind::DIVIDE => ::core::fmt::Formatter::write_str(f, "DIVIDE"),
                BinaryOperatorKind::MULTIPLY => ::core::fmt::Formatter::write_str(f, "MULTIPLY"),
                BinaryOperatorKind::AND => ::core::fmt::Formatter::write_str(f, "AND"),
                BinaryOperatorKind::OR => ::core::fmt::Formatter::write_str(f, "OR"),
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for BinaryOperatorKind {
        #[inline]
        fn clone(&self) -> BinaryOperatorKind {
            match self {
                BinaryOperatorKind::NOT_EQUAL => BinaryOperatorKind::NOT_EQUAL,
                BinaryOperatorKind::DOUBLE_EQUAL => BinaryOperatorKind::DOUBLE_EQUAL,
                BinaryOperatorKind::GREATER => BinaryOperatorKind::GREATER,
                BinaryOperatorKind::GREATER_EQUAL => BinaryOperatorKind::GREATER_EQUAL,
                BinaryOperatorKind::LESS => BinaryOperatorKind::LESS,
                BinaryOperatorKind::LESS_EQUAL => BinaryOperatorKind::LESS_EQUAL,
                BinaryOperatorKind::MINUS => BinaryOperatorKind::MINUS,
                BinaryOperatorKind::PLUS => BinaryOperatorKind::PLUS,
                BinaryOperatorKind::DIVIDE => BinaryOperatorKind::DIVIDE,
                BinaryOperatorKind::MULTIPLY => BinaryOperatorKind::MULTIPLY,
                BinaryOperatorKind::AND => BinaryOperatorKind::AND,
                BinaryOperatorKind::OR => BinaryOperatorKind::OR,
            }
        }
    }
    pub struct BinaryExpressionNode(Rc<CoreBinaryExpressionNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for BinaryExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "BinaryExpressionNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for BinaryExpressionNode {
        #[inline]
        fn clone(&self) -> BinaryExpressionNode {
            BinaryExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl BinaryExpressionNode {
        pub fn new(
            operator_kind: BinaryOperatorKind,
            operator: &TokenNode,
            left_expr: &ExpressionNode,
            right_expr: &ExpressionNode,
        ) -> Self {
            let node = Rc::new(CoreBinaryExpressionNode {
                operator_kind,
                operator: operator.clone(),
                left_expr: left_expr.clone(),
                right_expr: right_expr.clone(),
            });
            BinaryExpressionNode(node)
        }
    }
    impl Node for BinaryExpressionNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().left_expr.range().start()),
                TextSize::from(self.0.as_ref().right_expr.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().left_expr.start_line_number()
        }
    }
    pub enum CoreParamsNode {
        OK(OkParamsNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreParamsNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreParamsNode::OK(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK", &__self_0)
                }
                CoreParamsNode::MISSING_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "MISSING_TOKENS",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreParamsNode {
        #[inline]
        fn clone(&self) -> CoreParamsNode {
            match self {
                CoreParamsNode::OK(__self_0) => {
                    CoreParamsNode::OK(::core::clone::Clone::clone(__self_0))
                }
                CoreParamsNode::MISSING_TOKENS(__self_0) => {
                    CoreParamsNode::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct ParamsNode(Rc<CoreParamsNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for ParamsNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ParamsNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ParamsNode {
        #[inline]
        fn clone(&self) -> ParamsNode {
            ParamsNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl ParamsNode {
        pub fn new(ok_params_node: &OkParamsNode) -> Self {
            let node = Rc::new(CoreParamsNode::OK(ok_params_node.clone()));
            ParamsNode(node)
        }
    }
    impl Node for ParamsNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref() {
                CoreParamsNode::OK(ok_params) => TextRange::new(
                    TextSize::from(ok_params.range().start()),
                    TextSize::from(ok_params.range().end()),
                ),
                CoreParamsNode::MISSING_TOKENS(missing_tokens) => TextRange::new(
                    TextSize::from(missing_tokens.range().start()),
                    TextSize::from(missing_tokens.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref() {
                CoreParamsNode::OK(ok_params) => ok_params.start_line_number(),
                CoreParamsNode::MISSING_TOKENS(missing_tokens) => {
                    missing_tokens.start_line_number()
                }
            }
        }
    }
    impl ErrornousNode for ParamsNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
        ) -> Self {
            ParamsNode(Rc::new(CoreParamsNode::MISSING_TOKENS(
                MissingTokenNode::new(expected_symbols, received_token),
            )))
        }
    }
    pub struct CoreOkParamsNode {
        pub comma: Option<TokenNode>,
        pub param: ExpressionNode,
        pub remaining_params: Option<ParamsNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreOkParamsNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "CoreOkParamsNode",
                "comma",
                &&self.comma,
                "param",
                &&self.param,
                "remaining_params",
                &&self.remaining_params,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreOkParamsNode {
        #[inline]
        fn clone(&self) -> CoreOkParamsNode {
            CoreOkParamsNode {
                comma: ::core::clone::Clone::clone(&self.comma),
                param: ::core::clone::Clone::clone(&self.param),
                remaining_params: ::core::clone::Clone::clone(&self.remaining_params),
            }
        }
    }
    pub struct OkParamsNode(Rc<CoreOkParamsNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for OkParamsNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OkParamsNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for OkParamsNode {
        #[inline]
        fn clone(&self) -> OkParamsNode {
            OkParamsNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl OkParamsNode {
        pub fn new_with_single_param(param: &ExpressionNode) -> Self {
            let node = Rc::new(CoreOkParamsNode {
                comma: None,
                param: param.clone(),
                remaining_params: None,
            });
            OkParamsNode(node)
        }
        pub fn new_with_params(
            param: &ExpressionNode,
            remaining_params: &ParamsNode,
            comma: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreOkParamsNode {
                comma: Some(comma.clone()),
                param: param.clone(),
                remaining_params: Some(remaining_params.clone()),
            });
            OkParamsNode(node)
        }
    }
    impl Node for OkParamsNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref().remaining_params {
                Some(remaining_params) => TextRange::new(
                    TextSize::from(self.0.as_ref().param.range().start()),
                    TextSize::from(remaining_params.range().end()),
                ),
                None => TextRange::new(
                    TextSize::from(self.0.as_ref().param.range().start()),
                    TextSize::from(self.0.as_ref().param.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().param.start_line_number()
        }
    }
    pub struct CoreCallExpressionNode {
        pub lparen: TokenNode,
        pub rparen: TokenNode,
        pub function_name: TokenNode,
        pub params: Option<ParamsNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreCallExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreCallExpressionNode",
                "lparen",
                &&self.lparen,
                "rparen",
                &&self.rparen,
                "function_name",
                &&self.function_name,
                "params",
                &&self.params,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreCallExpressionNode {
        #[inline]
        fn clone(&self) -> CoreCallExpressionNode {
            CoreCallExpressionNode {
                lparen: ::core::clone::Clone::clone(&self.lparen),
                rparen: ::core::clone::Clone::clone(&self.rparen),
                function_name: ::core::clone::Clone::clone(&self.function_name),
                params: ::core::clone::Clone::clone(&self.params),
            }
        }
    }
    pub struct CallExpressionNode(Rc<CoreCallExpressionNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for CallExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "CallExpressionNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CallExpressionNode {
        #[inline]
        fn clone(&self) -> CallExpressionNode {
            CallExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl CallExpressionNode {
        pub fn new(
            function_name: &TokenNode,
            params: Option<&ParamsNode>,
            lparen: &TokenNode,
            rparen: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreCallExpressionNode {
                lparen: lparen.clone(),
                rparen: rparen.clone(),
                function_name: function_name.clone(),
                params: match params {
                    Some(val) => Some(val.clone()),
                    None => None,
                },
            });
            CallExpressionNode(node)
        }
    }
    impl Node for CallExpressionNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().function_name.range().start()),
                TextSize::from(self.0.as_ref().rparen.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().function_name.start_line_number()
        }
    }
    pub struct CoreClassMethodCallNode {
        pub lparen: TokenNode,
        pub rparen: TokenNode,
        pub double_colon: TokenNode,
        pub class_name: TokenNode,
        pub class_method_name: TokenNode,
        pub params: Option<ParamsNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreClassMethodCallNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            let names: &'static _ = &[
                "lparen",
                "rparen",
                "double_colon",
                "class_name",
                "class_method_name",
                "params",
            ];
            let values: &[&dyn ::core::fmt::Debug] = &[
                &&self.lparen,
                &&self.rparen,
                &&self.double_colon,
                &&self.class_name,
                &&self.class_method_name,
                &&self.params,
            ];
            ::core::fmt::Formatter::debug_struct_fields_finish(
                f,
                "CoreClassMethodCallNode",
                names,
                values,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreClassMethodCallNode {
        #[inline]
        fn clone(&self) -> CoreClassMethodCallNode {
            CoreClassMethodCallNode {
                lparen: ::core::clone::Clone::clone(&self.lparen),
                rparen: ::core::clone::Clone::clone(&self.rparen),
                double_colon: ::core::clone::Clone::clone(&self.double_colon),
                class_name: ::core::clone::Clone::clone(&self.class_name),
                class_method_name: ::core::clone::Clone::clone(&self.class_method_name),
                params: ::core::clone::Clone::clone(&self.params),
            }
        }
    }
    pub struct ClassMethodCallNode(Rc<CoreClassMethodCallNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for ClassMethodCallNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ClassMethodCallNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for ClassMethodCallNode {
        #[inline]
        fn clone(&self) -> ClassMethodCallNode {
            ClassMethodCallNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl ClassMethodCallNode {
        pub fn new(
            class_name: &TokenNode,
            class_method_name: &TokenNode,
            params: Option<&ParamsNode>,
            double_colon: &TokenNode,
            lparen: &TokenNode,
            rparen: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreClassMethodCallNode {
                lparen: lparen.clone(),
                rparen: rparen.clone(),
                double_colon: double_colon.clone(),
                class_name: class_name.clone(),
                class_method_name: class_method_name.clone(),
                params: match params {
                    Some(val) => Some(val.clone()),
                    None => None,
                },
            });
            ClassMethodCallNode(node)
        }
    }
    impl Node for ClassMethodCallNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().class_name.range().start()),
                TextSize::from(self.0.as_ref().rparen.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().class_name.start_line_number()
        }
    }
    pub enum CoreAtomNode {
        ATOM_START(AtomStartNode),
        CALL(CallNode),
        PROPERTRY_ACCESS(PropertyAccessNode),
        METHOD_ACCESS(MethodAccessNode),
        INDEX_ACCESS(IndexAccessNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreAtomNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreAtomNode::ATOM_START(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ATOM_START", &__self_0)
                }
                CoreAtomNode::CALL(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "CALL", &__self_0)
                }
                CoreAtomNode::PROPERTRY_ACCESS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "PROPERTRY_ACCESS",
                        &__self_0,
                    )
                }
                CoreAtomNode::METHOD_ACCESS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "METHOD_ACCESS", &__self_0)
                }
                CoreAtomNode::INDEX_ACCESS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "INDEX_ACCESS", &__self_0)
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreAtomNode {
        #[inline]
        fn clone(&self) -> CoreAtomNode {
            match self {
                CoreAtomNode::ATOM_START(__self_0) => {
                    CoreAtomNode::ATOM_START(::core::clone::Clone::clone(__self_0))
                }
                CoreAtomNode::CALL(__self_0) => {
                    CoreAtomNode::CALL(::core::clone::Clone::clone(__self_0))
                }
                CoreAtomNode::PROPERTRY_ACCESS(__self_0) => {
                    CoreAtomNode::PROPERTRY_ACCESS(::core::clone::Clone::clone(__self_0))
                }
                CoreAtomNode::METHOD_ACCESS(__self_0) => {
                    CoreAtomNode::METHOD_ACCESS(::core::clone::Clone::clone(__self_0))
                }
                CoreAtomNode::INDEX_ACCESS(__self_0) => {
                    CoreAtomNode::INDEX_ACCESS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct AtomNode(Rc<CoreAtomNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for AtomNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "AtomNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for AtomNode {
        #[inline]
        fn clone(&self) -> AtomNode {
            AtomNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl AtomNode {
        pub fn new_with_atom_start(atom_start: &AtomStartNode) -> Self {
            let node = Rc::new(CoreAtomNode::ATOM_START(atom_start.clone()));
            AtomNode(node)
        }
        pub fn new_with_call(
            atom: &AtomNode,
            params: Option<&ParamsNode>,
            lparen: &TokenNode,
            rparen: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreAtomNode::CALL(CallNode::new(
                atom, params, lparen, rparen,
            )));
            AtomNode(node)
        }
        pub fn new_with_propertry_access(
            atom: &AtomNode,
            propertry: &TokenNode,
            dot: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreAtomNode::PROPERTRY_ACCESS(PropertyAccessNode::new(
                atom, propertry, dot,
            )));
            AtomNode(node)
        }
        pub fn new_with_method_access(
            atom: &AtomNode,
            method_name: &TokenNode,
            params: Option<&ParamsNode>,
            lparen: &TokenNode,
            rparen: &TokenNode,
            dot: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreAtomNode::METHOD_ACCESS(MethodAccessNode::new(
                atom,
                method_name,
                params,
                lparen,
                rparen,
                dot,
            )));
            AtomNode(node)
        }
        pub fn new_with_index_access(
            atom: &AtomNode,
            index: &ExpressionNode,
            lsquare: &TokenNode,
            rsquare: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreAtomNode::INDEX_ACCESS(IndexAccessNode::new(
                atom, index, lsquare, rsquare,
            )));
            AtomNode(node)
        }
        pub fn is_valid_l_value(&self) -> bool {
            match &self.0.as_ref() {
                CoreAtomNode::ATOM_START(atom_start_node) => atom_start_node.is_valid_l_value(),
                CoreAtomNode::CALL(_) => false,
                CoreAtomNode::METHOD_ACCESS(_) => false,
                CoreAtomNode::INDEX_ACCESS(atom_index_access_node) => {
                    let atom = &atom_index_access_node.0.as_ref().atom;
                    return atom.is_valid_l_value();
                }
                CoreAtomNode::PROPERTRY_ACCESS(atom_property_access_node) => {
                    let atom = &atom_property_access_node.0.as_ref().atom;
                    return atom.is_valid_l_value();
                }
            }
        }
    }
    impl Node for AtomNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref() {
                CoreAtomNode::ATOM_START(atom_start) => TextRange::new(
                    TextSize::from(atom_start.range().start()),
                    TextSize::from(atom_start.range().end()),
                ),
                CoreAtomNode::CALL(call) => TextRange::new(
                    TextSize::from(call.range().start()),
                    TextSize::from(call.range().end()),
                ),
                CoreAtomNode::PROPERTRY_ACCESS(property_access) => TextRange::new(
                    TextSize::from(property_access.range().start()),
                    TextSize::from(property_access.range().end()),
                ),
                CoreAtomNode::METHOD_ACCESS(method_access) => TextRange::new(
                    TextSize::from(method_access.range().start()),
                    TextSize::from(method_access.range().end()),
                ),
                CoreAtomNode::INDEX_ACCESS(index_access) => TextRange::new(
                    TextSize::from(index_access.range().start()),
                    TextSize::from(index_access.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref() {
                CoreAtomNode::ATOM_START(atom_start) => atom_start.start_line_number(),
                CoreAtomNode::CALL(call) => call.start_line_number(),
                CoreAtomNode::PROPERTRY_ACCESS(property_access) => {
                    property_access.start_line_number()
                }
                CoreAtomNode::METHOD_ACCESS(method_access) => method_access.start_line_number(),
                CoreAtomNode::INDEX_ACCESS(index_access) => index_access.start_line_number(),
            }
        }
    }
    pub enum CoreAtomStartNode {
        IDENTIFIER(TokenNode),
        FUNCTION_CALL(CallExpressionNode),
        CLASS_METHOD_CALL(ClassMethodCallNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreAtomStartNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreAtomStartNode::IDENTIFIER(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "IDENTIFIER", &__self_0)
                }
                CoreAtomStartNode::FUNCTION_CALL(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "FUNCTION_CALL", &__self_0)
                }
                CoreAtomStartNode::CLASS_METHOD_CALL(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "CLASS_METHOD_CALL",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreAtomStartNode {
        #[inline]
        fn clone(&self) -> CoreAtomStartNode {
            match self {
                CoreAtomStartNode::IDENTIFIER(__self_0) => {
                    CoreAtomStartNode::IDENTIFIER(::core::clone::Clone::clone(__self_0))
                }
                CoreAtomStartNode::FUNCTION_CALL(__self_0) => {
                    CoreAtomStartNode::FUNCTION_CALL(::core::clone::Clone::clone(__self_0))
                }
                CoreAtomStartNode::CLASS_METHOD_CALL(__self_0) => {
                    CoreAtomStartNode::CLASS_METHOD_CALL(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct AtomStartNode(Rc<CoreAtomStartNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for AtomStartNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "AtomStartNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for AtomStartNode {
        #[inline]
        fn clone(&self) -> AtomStartNode {
            AtomStartNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl AtomStartNode {
        pub fn new_with_identifier(token: &TokenNode) -> Self {
            let node = Rc::new(CoreAtomStartNode::IDENTIFIER(token.clone()));
            AtomStartNode(node)
        }
        pub fn new_with_function_call(call_expr: &CallExpressionNode) -> Self {
            let node = Rc::new(CoreAtomStartNode::FUNCTION_CALL(call_expr.clone()));
            AtomStartNode(node)
        }
        pub fn new_with_class_method_call(
            class_name: &TokenNode,
            class_method_name: &TokenNode,
            params: Option<&ParamsNode>,
            double_colon: &TokenNode,
            lparen: &TokenNode,
            rparen: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreAtomStartNode::CLASS_METHOD_CALL(
                ClassMethodCallNode::new(
                    class_name,
                    class_method_name,
                    params,
                    double_colon,
                    lparen,
                    rparen,
                ),
            ));
            AtomStartNode(node)
        }
        pub fn is_valid_l_value(&self) -> bool {
            match &self.0.as_ref() {
                CoreAtomStartNode::IDENTIFIER(_) => true,
                _ => false,
            }
        }
    }
    impl Node for AtomStartNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref() {
                CoreAtomStartNode::IDENTIFIER(token) => TextRange::new(
                    TextSize::from(token.range().start()),
                    TextSize::from(token.range().end()),
                ),
                CoreAtomStartNode::FUNCTION_CALL(function_call) => TextRange::new(
                    TextSize::from(function_call.range().start()),
                    TextSize::from(function_call.range().end()),
                ),
                CoreAtomStartNode::CLASS_METHOD_CALL(class_method_call) => TextRange::new(
                    TextSize::from(class_method_call.range().start()),
                    TextSize::from(class_method_call.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref() {
                CoreAtomStartNode::IDENTIFIER(token) => token.start_line_number(),
                CoreAtomStartNode::FUNCTION_CALL(function_call) => {
                    function_call.start_line_number()
                }
                CoreAtomStartNode::CLASS_METHOD_CALL(class_method_call) => {
                    class_method_call.start_line_number()
                }
            }
        }
    }
    pub struct CoreCallNode {
        pub atom: AtomNode,
        pub lparen: TokenNode,
        pub rparen: TokenNode,
        pub params: Option<ParamsNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreCallNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreCallNode",
                "atom",
                &&self.atom,
                "lparen",
                &&self.lparen,
                "rparen",
                &&self.rparen,
                "params",
                &&self.params,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreCallNode {
        #[inline]
        fn clone(&self) -> CoreCallNode {
            CoreCallNode {
                atom: ::core::clone::Clone::clone(&self.atom),
                lparen: ::core::clone::Clone::clone(&self.lparen),
                rparen: ::core::clone::Clone::clone(&self.rparen),
                params: ::core::clone::Clone::clone(&self.params),
            }
        }
    }
    pub struct CallNode(Rc<CoreCallNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for CallNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "CallNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CallNode {
        #[inline]
        fn clone(&self) -> CallNode {
            CallNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl CallNode {
        fn new(
            atom: &AtomNode,
            params: Option<&ParamsNode>,
            lparen: &TokenNode,
            rparen: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreCallNode {
                atom: atom.clone(),
                lparen: lparen.clone(),
                rparen: rparen.clone(),
                params: match params {
                    Some(val) => Some(val.clone()),
                    None => None,
                },
            });
            CallNode(node)
        }
    }
    impl Node for CallNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().atom.range().start()),
                TextSize::from(self.0.as_ref().rparen.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().atom.start_line_number()
        }
    }
    pub struct CorePropertyAccessNode {
        pub dot: TokenNode,
        pub atom: AtomNode,
        pub propertry: TokenNode,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CorePropertyAccessNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "CorePropertyAccessNode",
                "dot",
                &&self.dot,
                "atom",
                &&self.atom,
                "propertry",
                &&self.propertry,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CorePropertyAccessNode {
        #[inline]
        fn clone(&self) -> CorePropertyAccessNode {
            CorePropertyAccessNode {
                dot: ::core::clone::Clone::clone(&self.dot),
                atom: ::core::clone::Clone::clone(&self.atom),
                propertry: ::core::clone::Clone::clone(&self.propertry),
            }
        }
    }
    pub struct PropertyAccessNode(Rc<CorePropertyAccessNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for PropertyAccessNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "PropertyAccessNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for PropertyAccessNode {
        #[inline]
        fn clone(&self) -> PropertyAccessNode {
            PropertyAccessNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl PropertyAccessNode {
        fn new(atom: &AtomNode, propertry: &TokenNode, dot: &TokenNode) -> Self {
            let node = Rc::new(CorePropertyAccessNode {
                dot: dot.clone(),
                atom: atom.clone(),
                propertry: propertry.clone(),
            });
            PropertyAccessNode(node)
        }
    }
    impl Node for PropertyAccessNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().atom.range().start()),
                TextSize::from(self.0.as_ref().propertry.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().atom.start_line_number()
        }
    }
    pub struct CoreMethodAccessNode {
        pub lparen: TokenNode,
        pub rparen: TokenNode,
        pub dot: TokenNode,
        pub atom: AtomNode,
        pub method_name: TokenNode,
        pub params: Option<ParamsNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreMethodAccessNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            let names: &'static _ = &["lparen", "rparen", "dot", "atom", "method_name", "params"];
            let values: &[&dyn ::core::fmt::Debug] = &[
                &&self.lparen,
                &&self.rparen,
                &&self.dot,
                &&self.atom,
                &&self.method_name,
                &&self.params,
            ];
            ::core::fmt::Formatter::debug_struct_fields_finish(
                f,
                "CoreMethodAccessNode",
                names,
                values,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreMethodAccessNode {
        #[inline]
        fn clone(&self) -> CoreMethodAccessNode {
            CoreMethodAccessNode {
                lparen: ::core::clone::Clone::clone(&self.lparen),
                rparen: ::core::clone::Clone::clone(&self.rparen),
                dot: ::core::clone::Clone::clone(&self.dot),
                atom: ::core::clone::Clone::clone(&self.atom),
                method_name: ::core::clone::Clone::clone(&self.method_name),
                params: ::core::clone::Clone::clone(&self.params),
            }
        }
    }
    pub struct MethodAccessNode(Rc<CoreMethodAccessNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for MethodAccessNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "MethodAccessNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for MethodAccessNode {
        #[inline]
        fn clone(&self) -> MethodAccessNode {
            MethodAccessNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl MethodAccessNode {
        pub fn new(
            atom: &AtomNode,
            method_name: &TokenNode,
            params: Option<&ParamsNode>,
            lparen: &TokenNode,
            rparen: &TokenNode,
            dot: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreMethodAccessNode {
                lparen: lparen.clone(),
                rparen: rparen.clone(),
                dot: dot.clone(),
                atom: atom.clone(),
                method_name: method_name.clone(),
                params: match params {
                    Some(val) => Some(val.clone()),
                    None => None,
                },
            });
            MethodAccessNode(node)
        }
    }
    impl Node for MethodAccessNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().atom.range().start()),
                TextSize::from(self.0.as_ref().rparen.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().atom.start_line_number()
        }
    }
    pub struct CoreIndexAccessNode {
        pub lsquare: TokenNode,
        pub rsquare: TokenNode,
        pub atom: AtomNode,
        pub index: ExpressionNode,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreIndexAccessNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreIndexAccessNode",
                "lsquare",
                &&self.lsquare,
                "rsquare",
                &&self.rsquare,
                "atom",
                &&self.atom,
                "index",
                &&self.index,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreIndexAccessNode {
        #[inline]
        fn clone(&self) -> CoreIndexAccessNode {
            CoreIndexAccessNode {
                lsquare: ::core::clone::Clone::clone(&self.lsquare),
                rsquare: ::core::clone::Clone::clone(&self.rsquare),
                atom: ::core::clone::Clone::clone(&self.atom),
                index: ::core::clone::Clone::clone(&self.index),
            }
        }
    }
    pub struct IndexAccessNode(Rc<CoreIndexAccessNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for IndexAccessNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "IndexAccessNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for IndexAccessNode {
        #[inline]
        fn clone(&self) -> IndexAccessNode {
            IndexAccessNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl IndexAccessNode {
        pub fn new(
            atom: &AtomNode,
            index: &ExpressionNode,
            lsquare: &TokenNode,
            rsquare: &TokenNode,
        ) -> Self {
            let node = Rc::new(CoreIndexAccessNode {
                lsquare: lsquare.clone(),
                rsquare: rsquare.clone(),
                atom: atom.clone(),
                index: index.clone(),
            });
            IndexAccessNode(node)
        }
    }
    impl Node for IndexAccessNode {
        fn range(&self) -> TextRange {
            TextRange::new(
                TextSize::from(self.0.as_ref().atom.range().start()),
                TextSize::from(self.0.as_ref().rsquare.range().end()),
            )
        }
        fn start_line_number(&self) -> usize {
            self.0.as_ref().atom.start_line_number()
        }
    }
    pub enum CoreRAssignmentNode {
        LAMBDA(FunctionDeclarationNode),
        EXPRESSION(ExpressionStatementNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreRAssignmentNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                CoreRAssignmentNode::LAMBDA(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "LAMBDA", &__self_0)
                }
                CoreRAssignmentNode::EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "EXPRESSION", &__self_0)
                }
                CoreRAssignmentNode::MISSING_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "MISSING_TOKENS",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreRAssignmentNode {
        #[inline]
        fn clone(&self) -> CoreRAssignmentNode {
            match self {
                CoreRAssignmentNode::LAMBDA(__self_0) => {
                    CoreRAssignmentNode::LAMBDA(::core::clone::Clone::clone(__self_0))
                }
                CoreRAssignmentNode::EXPRESSION(__self_0) => {
                    CoreRAssignmentNode::EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                CoreRAssignmentNode::MISSING_TOKENS(__self_0) => {
                    CoreRAssignmentNode::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct RAssignmentNode(Rc<CoreRAssignmentNode>);
    #[automatically_derived]
    impl ::core::fmt::Debug for RAssignmentNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "RAssignmentNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for RAssignmentNode {
        #[inline]
        fn clone(&self) -> RAssignmentNode {
            RAssignmentNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl RAssignmentNode {
        pub fn new_with_lambda(lambda_decl: &FunctionDeclarationNode) -> Self {
            let node = Rc::new(CoreRAssignmentNode::LAMBDA(lambda_decl.clone()));
            RAssignmentNode(node)
        }
        pub fn new_with_expr(expr: &ExpressionNode, newline: &TokenNode) -> Self {
            let node = Rc::new(CoreRAssignmentNode::EXPRESSION(
                ExpressionStatementNode::new(expr, newline),
            ));
            RAssignmentNode(node)
        }
    }
    impl Node for RAssignmentNode {
        fn range(&self) -> TextRange {
            match &self.0.as_ref() {
                CoreRAssignmentNode::LAMBDA(func_decl) => TextRange::new(
                    TextSize::from(func_decl.range().start()),
                    TextSize::from(func_decl.range().end()),
                ),
                CoreRAssignmentNode::EXPRESSION(expr_stmt) => TextRange::new(
                    TextSize::from(expr_stmt.range().start()),
                    TextSize::from(expr_stmt.range().end()),
                ),
                CoreRAssignmentNode::MISSING_TOKENS(missing_tokens) => TextRange::new(
                    TextSize::from(missing_tokens.range().start()),
                    TextSize::from(missing_tokens.range().end()),
                ),
            }
        }
        fn start_line_number(&self) -> usize {
            match &self.0.as_ref() {
                CoreRAssignmentNode::LAMBDA(func_decl) => func_decl.start_line_number(),
                CoreRAssignmentNode::EXPRESSION(expr_stmt) => expr_stmt.start_line_number(),
                CoreRAssignmentNode::MISSING_TOKENS(missing_tokens) => {
                    missing_tokens.start_line_number()
                }
            }
        }
    }
    impl ErrornousNode for RAssignmentNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
        ) -> Self {
            RAssignmentNode(Rc::new(CoreRAssignmentNode::MISSING_TOKENS(
                MissingTokenNode::new(expected_symbols, received_token),
            )))
        }
    }
}
