pub mod ast {
    #[macro_use]
    use jarvil_macros::set_parent;
    #[macro_use]
    use jarvil_macros::NodeUtils;
    use crate::scope::core::SymbolData;
    use crate::types::atomic::Atomic;
    use crate::{
        code::Code,
        lexer::token::{CoreToken, Token},
        scope::core::Namespace,
        types::{array::Array, core::Type},
    };
    use std::{
        cell::{Ref, RefCell, RefMut},
        rc::{Rc, Weak},
    };
    pub trait Node {
        fn set_parent(&self, parent_node: WeakASTNode);
    }
    pub trait ErrornousNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
            lookahead: usize,
        ) -> Self;
    }
    pub enum ASTNode {
        BLOCK(BlockNode),
        STATEMENT_INDENT_WRAPPER(StatemenIndentWrapperNode),
        SKIPPED_TOKENS(SkippedTokens),
        STATEMENT(StatementNode),
        ASSIGNMENT(AssignmentNode),
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
        LOGICAL_EXPRESSION(LogicalExpressionNode),
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
                ASTNode::STATEMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "STATEMENT", &__self_0)
                }
                ASTNode::ASSIGNMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ASSIGNMENT", &__self_0)
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
                ASTNode::LOGICAL_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "LOGICAL_EXPRESSION",
                        &__self_0,
                    )
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
                ASTNode::STATEMENT(__self_0) => {
                    ASTNode::STATEMENT(::core::clone::Clone::clone(__self_0))
                }
                ASTNode::ASSIGNMENT(__self_0) => {
                    ASTNode::ASSIGNMENT(::core::clone::Clone::clone(__self_0))
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
                ASTNode::LOGICAL_EXPRESSION(__self_0) => {
                    ASTNode::LOGICAL_EXPRESSION(::core::clone::Clone::clone(__self_0))
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
    pub struct WeakBlockNode(pub Weak<RefCell<CoreBlockNode>>);
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
    pub struct WeakStatemenIndentWrapperNode(pub Weak<RefCell<CoreStatemenIndentWrapperNode>>);
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
    pub struct WeakSkippedTokens(pub Weak<RefCell<CoreSkippedTokens>>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakSkippedTokens {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "WeakSkippedTokens", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakSkippedTokens {
        #[inline]
        fn clone(&self) -> WeakSkippedTokens {
            WeakSkippedTokens(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakStatementNode(pub Weak<RefCell<CoreStatementNode>>);
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
    pub struct WeakAssignmentNode(pub Weak<RefCell<CoreAssignmentNode>>);
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
    pub struct WeakStructStatementNode(pub Weak<RefCell<CoreStructStatementNode>>);
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
    pub struct WeakTypeDeclarationNode(pub Weak<RefCell<CoreTypeDeclarationNode>>);
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
    pub struct WeakStructDeclarationNode(pub Weak<RefCell<CoreStructDeclarationNode>>);
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
    pub struct WeakLambdaDeclarationNode(pub Weak<RefCell<CoreLambdaDeclarationNode>>);
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
    pub struct WeakOkLambdaDeclarationNode(pub Weak<RefCell<CoreOkLambdaDeclarationNode>>);
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
    pub struct WeakFunctionDeclarationNode(pub Weak<RefCell<CoreFunctionDeclarationNode>>);
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
    pub struct WeakOkFunctionDeclarationNode(pub Weak<RefCell<CoreOkFunctionDeclarationNode>>);
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
    pub struct WeakVariableDeclarationNode(pub Weak<RefCell<CoreVariableDeclarationNode>>);
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
    pub struct WeakRAssignmentNode(pub Weak<RefCell<CoreRAssignmentNode>>);
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
    pub struct WeakNameTypeSpecsNode(pub Weak<RefCell<CoreNameTypeSpecsNode>>);
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
    pub struct WeakOkNameTypeSpecsNode(pub Weak<RefCell<CoreOkNameTypeSpecsNode>>);
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
    pub struct WeakNameTypeSpecNode(pub Weak<RefCell<CoreNameTypeSpecNode>>);
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
    pub struct WeakTypeExpressionNode(pub Weak<RefCell<CoreTypeExpressionNode>>);
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
    pub struct WeakAtomicTypeNode(pub Weak<RefCell<CoreAtomicTypeNode>>);
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
    pub struct WeakArrayTypeNode(pub Weak<RefCell<CoreArrayTypeNode>>);
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
    pub struct WeakUserDefinedTypeNode(pub Weak<RefCell<CoreUserDefinedTypeNode>>);
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
    pub struct WeakExpressionNode(pub Weak<RefCell<CoreExpressionNode>>);
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
    pub struct WeakAtomicExpressionNode(pub Weak<RefCell<CoreAtomicExpressionNode>>);
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
    pub struct WeakParenthesisedExpressionNode(pub Weak<RefCell<CoreParenthesisedExpressionNode>>);
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
    pub struct WeakUnaryExpressionNode(pub Weak<RefCell<CoreUnaryExpressionNode>>);
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
    pub struct WeakOnlyUnaryExpressionNode(pub Weak<RefCell<CoreOnlyUnaryExpressionNode>>);
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
    pub struct WeakBinaryExpressionNode(pub Weak<RefCell<CoreBinaryExpressionNode>>);
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
    pub struct WeakLogicalExpressionNode(pub Weak<RefCell<CoreLogicalExpressionNode>>);
    #[automatically_derived]
    impl ::core::fmt::Debug for WeakLogicalExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(
                f,
                "WeakLogicalExpressionNode",
                &&self.0,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for WeakLogicalExpressionNode {
        #[inline]
        fn clone(&self) -> WeakLogicalExpressionNode {
            WeakLogicalExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    pub struct WeakParamsNode(pub Weak<RefCell<CoreParamsNode>>);
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
    pub struct WeakOkParamsNode(pub Weak<RefCell<CoreOkParamsNode>>);
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
    pub struct WeakCallExpressionNode(pub Weak<RefCell<CoreCallExpressionNode>>);
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
    pub struct WeakClassMethodCallNode(pub Weak<RefCell<CoreClassMethodCallNode>>);
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
    pub struct WeakAtomNode(pub Weak<RefCell<CoreAtomNode>>);
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
    pub struct WeakAtomStartNode(pub Weak<RefCell<CoreAtomStartNode>>);
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
    pub struct WeakCallNode(pub Weak<RefCell<CoreCallNode>>);
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
    pub struct WeakPropertyAccessNode(pub Weak<RefCell<CorePropertyAccessNode>>);
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
    pub struct WeakMethodAccessNode(pub Weak<RefCell<CoreMethodAccessNode>>);
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
    pub struct WeakIndexAccessNode(pub Weak<RefCell<CoreIndexAccessNode>>);
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
    pub struct WeakTokenNode(pub Weak<RefCell<CoreTokenNode>>);
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
    pub struct WeakOkTokenNode(pub Weak<RefCell<CoreOkTokenNode>>);
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
    pub struct WeakMissingTokenNode(pub Weak<RefCell<CoreMissingTokenNode>>);
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
    pub struct WeakSkippedTokenNode(pub Weak<RefCell<CoreSkippedTokenNode>>);
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
        SKIPPED_TOKENS(WeakSkippedTokens),
        STATEMENT(WeakStatementNode),
        ASSIGNMENT(WeakAssignmentNode),
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
        LOGICAL_EXPRESSION(WeakLogicalExpressionNode),
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
                WeakASTNode::STATEMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "STATEMENT", &__self_0)
                }
                WeakASTNode::ASSIGNMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ASSIGNMENT", &__self_0)
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
                WeakASTNode::LOGICAL_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "LOGICAL_EXPRESSION",
                        &__self_0,
                    )
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
                WeakASTNode::STATEMENT(__self_0) => {
                    WeakASTNode::STATEMENT(::core::clone::Clone::clone(__self_0))
                }
                WeakASTNode::ASSIGNMENT(__self_0) => {
                    WeakASTNode::ASSIGNMENT(::core::clone::Clone::clone(__self_0))
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
                WeakASTNode::LOGICAL_EXPRESSION(__self_0) => {
                    WeakASTNode::LOGICAL_EXPRESSION(::core::clone::Clone::clone(__self_0))
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
        pub fn new_with_SkippedTokens(x: &SkippedTokens) -> Self {
            ASTNode::SKIPPED_TOKENS(x.clone())
        }
        pub fn new_with_StatementNode(x: &StatementNode) -> Self {
            ASTNode::STATEMENT(x.clone())
        }
        pub fn new_with_AssignmentNode(x: &AssignmentNode) -> Self {
            ASTNode::ASSIGNMENT(x.clone())
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
        pub fn new_with_LogicalExpressionNode(x: &LogicalExpressionNode) -> Self {
            ASTNode::LOGICAL_EXPRESSION(x.clone())
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
        newline: TokenNode,
        pub stmts: Rc<RefCell<Vec<StatemenIndentWrapperNode>>>,
        scope: Option<Namespace>,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreBlockNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreBlockNode",
                "newline",
                &&self.newline,
                "stmts",
                &&self.stmts,
                "scope",
                &&self.scope,
                "parent",
                &&self.parent,
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
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct BlockNode(pub Rc<RefCell<CoreBlockNode>>);
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
        pub fn new(
            stmts: &Rc<RefCell<Vec<StatemenIndentWrapperNode>>>,
            newline: &TokenNode,
        ) -> Self {
            let node = Rc::new(RefCell::new(CoreBlockNode {
                newline: newline.clone(),
                stmts: stmts.clone(),
                scope: None,
                parent: None,
            }));
            newline.set_parent(WeakASTNode::BLOCK(WeakBlockNode(Rc::downgrade(&node))));
            for stmt in &*stmts.as_ref().borrow() {
                stmt.set_parent(WeakASTNode::BLOCK(WeakBlockNode(Rc::downgrade(&node))));
            }
            BlockNode(node)
        }
        pub fn set_scope(&self, scope: &Namespace) {
            self.core_ref_mut().scope = Some(scope.clone());
        }
        pub fn core_ref(&self) -> Ref<CoreBlockNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreBlockNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for BlockNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreStatemenIndentWrapperNode {
        pub kind: StatementIndentWrapperKind,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreStatemenIndentWrapperNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreStatemenIndentWrapperNode",
                "kind",
                &&self.kind,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreStatemenIndentWrapperNode {
        #[inline]
        fn clone(&self) -> CoreStatemenIndentWrapperNode {
            CoreStatemenIndentWrapperNode {
                kind: ::core::clone::Clone::clone(&self.kind),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub enum StatementIndentWrapperKind {
        CORRECTLY_INDENTED(StatementNode),
        INCORRECTLY_INDENTED((StatementNode, (i64, i64))),
        LEADING_SKIPPED_TOKENS(SkippedTokens),
        TRAILING_SKIPPED_TOKENS(SkippedTokens),
        EXTRA_NEWLINES(SkippedTokens),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for StatementIndentWrapperKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                StatementIndentWrapperKind::CORRECTLY_INDENTED(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "CORRECTLY_INDENTED",
                        &__self_0,
                    )
                }
                StatementIndentWrapperKind::INCORRECTLY_INDENTED(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "INCORRECTLY_INDENTED",
                        &__self_0,
                    )
                }
                StatementIndentWrapperKind::LEADING_SKIPPED_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "LEADING_SKIPPED_TOKENS",
                        &__self_0,
                    )
                }
                StatementIndentWrapperKind::TRAILING_SKIPPED_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "TRAILING_SKIPPED_TOKENS",
                        &__self_0,
                    )
                }
                StatementIndentWrapperKind::EXTRA_NEWLINES(__self_0) => {
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
    impl ::core::clone::Clone for StatementIndentWrapperKind {
        #[inline]
        fn clone(&self) -> StatementIndentWrapperKind {
            match self {
                StatementIndentWrapperKind::CORRECTLY_INDENTED(__self_0) => {
                    StatementIndentWrapperKind::CORRECTLY_INDENTED(::core::clone::Clone::clone(
                        __self_0,
                    ))
                }
                StatementIndentWrapperKind::INCORRECTLY_INDENTED(__self_0) => {
                    StatementIndentWrapperKind::INCORRECTLY_INDENTED(::core::clone::Clone::clone(
                        __self_0,
                    ))
                }
                StatementIndentWrapperKind::LEADING_SKIPPED_TOKENS(__self_0) => {
                    StatementIndentWrapperKind::LEADING_SKIPPED_TOKENS(::core::clone::Clone::clone(
                        __self_0,
                    ))
                }
                StatementIndentWrapperKind::TRAILING_SKIPPED_TOKENS(__self_0) => {
                    StatementIndentWrapperKind::TRAILING_SKIPPED_TOKENS(
                        ::core::clone::Clone::clone(__self_0),
                    )
                }
                StatementIndentWrapperKind::EXTRA_NEWLINES(__self_0) => {
                    StatementIndentWrapperKind::EXTRA_NEWLINES(::core::clone::Clone::clone(
                        __self_0,
                    ))
                }
            }
        }
    }
    pub struct StatemenIndentWrapperNode(pub Rc<RefCell<CoreStatemenIndentWrapperNode>>);
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
            let node = Rc::new(RefCell::new(CoreStatemenIndentWrapperNode {
                kind: StatementIndentWrapperKind::CORRECTLY_INDENTED(stmt.clone()),
                parent: None,
            }));
            stmt.set_parent(WeakASTNode::STATEMENT_INDENT_WRAPPER(
                WeakStatemenIndentWrapperNode(Rc::downgrade(&node)),
            ));
            StatemenIndentWrapperNode(node)
        }
        pub fn new_with_incorrectly_indented(
            stmt: &StatementNode,
            expected_indent: i64,
            received_indent: i64,
        ) -> Self {
            let node = Rc::new(RefCell::new(CoreStatemenIndentWrapperNode {
                kind: StatementIndentWrapperKind::INCORRECTLY_INDENTED((
                    stmt.clone(),
                    (expected_indent, received_indent),
                )),
                parent: None,
            }));
            stmt.set_parent(WeakASTNode::STATEMENT_INDENT_WRAPPER(
                WeakStatemenIndentWrapperNode(Rc::downgrade(&node)),
            ));
            StatemenIndentWrapperNode(node)
        }
        pub fn new_with_leading_skipped_tokens(skipped_tokens: &SkippedTokens) -> Self {
            let node = Rc::new(RefCell::new(CoreStatemenIndentWrapperNode {
                kind: StatementIndentWrapperKind::LEADING_SKIPPED_TOKENS(skipped_tokens.clone()),
                parent: None,
            }));
            skipped_tokens.set_parent(WeakASTNode::STATEMENT_INDENT_WRAPPER(
                WeakStatemenIndentWrapperNode(Rc::downgrade(&node)),
            ));
            StatemenIndentWrapperNode(node)
        }
        pub fn new_with_trailing_skipped_tokens(skipped_tokens: &SkippedTokens) -> Self {
            let node = Rc::new(RefCell::new(CoreStatemenIndentWrapperNode {
                kind: StatementIndentWrapperKind::TRAILING_SKIPPED_TOKENS(skipped_tokens.clone()),
                parent: None,
            }));
            skipped_tokens.set_parent(WeakASTNode::STATEMENT_INDENT_WRAPPER(
                WeakStatemenIndentWrapperNode(Rc::downgrade(&node)),
            ));
            StatemenIndentWrapperNode(node)
        }
        pub fn new_with_extra_newlines(skipped_tokens: &SkippedTokens) -> Self {
            let node = Rc::new(RefCell::new(CoreStatemenIndentWrapperNode {
                kind: StatementIndentWrapperKind::EXTRA_NEWLINES(skipped_tokens.clone()),
                parent: None,
            }));
            skipped_tokens.set_parent(WeakASTNode::STATEMENT_INDENT_WRAPPER(
                WeakStatemenIndentWrapperNode(Rc::downgrade(&node)),
            ));
            StatemenIndentWrapperNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreStatemenIndentWrapperNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreStatemenIndentWrapperNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for StatemenIndentWrapperNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreSkippedTokens {
        pub skipped_tokens: Rc<Vec<SkippedTokenNode>>,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreSkippedTokens {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreSkippedTokens",
                "skipped_tokens",
                &&self.skipped_tokens,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreSkippedTokens {
        #[inline]
        fn clone(&self) -> CoreSkippedTokens {
            CoreSkippedTokens {
                skipped_tokens: ::core::clone::Clone::clone(&self.skipped_tokens),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct SkippedTokens(pub Rc<RefCell<CoreSkippedTokens>>);
    #[automatically_derived]
    impl ::core::fmt::Debug for SkippedTokens {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "SkippedTokens", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for SkippedTokens {
        #[inline]
        fn clone(&self) -> SkippedTokens {
            SkippedTokens(::core::clone::Clone::clone(&self.0))
        }
    }
    impl SkippedTokens {
        pub fn new_with_leading_skipped_tokens(skipped_tokens: &Rc<Vec<SkippedTokenNode>>) -> Self {
            let node = Rc::new(RefCell::new(CoreSkippedTokens {
                skipped_tokens: skipped_tokens.clone(),
                parent: None,
            }));
            for skipped_token in skipped_tokens.as_ref() {
                skipped_token.set_parent(WeakASTNode::SKIPPED_TOKENS(WeakSkippedTokens(
                    Rc::downgrade(&node),
                )));
            }
            SkippedTokens(node)
        }
        pub fn new_with_trailing_skipped_tokens(
            skipped_tokens: &Rc<Vec<SkippedTokenNode>>,
        ) -> Self {
            let node = Rc::new(RefCell::new(CoreSkippedTokens {
                skipped_tokens: skipped_tokens.clone(),
                parent: None,
            }));
            for skipped_token in skipped_tokens.as_ref() {
                skipped_token.set_parent(WeakASTNode::SKIPPED_TOKENS(WeakSkippedTokens(
                    Rc::downgrade(&node),
                )));
            }
            SkippedTokens(node)
        }
        pub fn new_with_extra_newlines(extra_newlines: &Rc<Vec<SkippedTokenNode>>) -> Self {
            let node = Rc::new(RefCell::new(CoreSkippedTokens {
                skipped_tokens: extra_newlines.clone(),
                parent: None,
            }));
            for skipped_token in extra_newlines.as_ref() {
                skipped_token.set_parent(WeakASTNode::SKIPPED_TOKENS(WeakSkippedTokens(
                    Rc::downgrade(&node),
                )));
            }
            SkippedTokens(node)
        }
        pub fn core_ref(&self) -> Ref<CoreSkippedTokens> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreSkippedTokens> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for SkippedTokens {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreStatementNode {
        pub kind: StatementKind,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreStatementNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreStatementNode",
                "kind",
                &&self.kind,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreStatementNode {
        #[inline]
        fn clone(&self) -> CoreStatementNode {
            CoreStatementNode {
                kind: ::core::clone::Clone::clone(&self.kind),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub enum StatementKind {
        EXPRESSION((ExpressionNode, TokenNode)),
        ASSIGNMENT(AssignmentNode),
        VARIABLE_DECLARATION(VariableDeclarationNode),
        FUNCTION_DECLARATION(FunctionDeclarationNode),
        TYPE_DECLARATION(TypeDeclarationNode),
        STRUCT_STATEMENT(StructStatementNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for StatementKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                StatementKind::EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "EXPRESSION", &__self_0)
                }
                StatementKind::ASSIGNMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ASSIGNMENT", &__self_0)
                }
                StatementKind::VARIABLE_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "VARIABLE_DECLARATION",
                        &__self_0,
                    )
                }
                StatementKind::FUNCTION_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "FUNCTION_DECLARATION",
                        &__self_0,
                    )
                }
                StatementKind::TYPE_DECLARATION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "TYPE_DECLARATION",
                        &__self_0,
                    )
                }
                StatementKind::STRUCT_STATEMENT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "STRUCT_STATEMENT",
                        &__self_0,
                    )
                }
                StatementKind::MISSING_TOKENS(__self_0) => {
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
    impl ::core::clone::Clone for StatementKind {
        #[inline]
        fn clone(&self) -> StatementKind {
            match self {
                StatementKind::EXPRESSION(__self_0) => {
                    StatementKind::EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                StatementKind::ASSIGNMENT(__self_0) => {
                    StatementKind::ASSIGNMENT(::core::clone::Clone::clone(__self_0))
                }
                StatementKind::VARIABLE_DECLARATION(__self_0) => {
                    StatementKind::VARIABLE_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                StatementKind::FUNCTION_DECLARATION(__self_0) => {
                    StatementKind::FUNCTION_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                StatementKind::TYPE_DECLARATION(__self_0) => {
                    StatementKind::TYPE_DECLARATION(::core::clone::Clone::clone(__self_0))
                }
                StatementKind::STRUCT_STATEMENT(__self_0) => {
                    StatementKind::STRUCT_STATEMENT(::core::clone::Clone::clone(__self_0))
                }
                StatementKind::MISSING_TOKENS(__self_0) => {
                    StatementKind::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct StatementNode(pub Rc<RefCell<CoreStatementNode>>);
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
            let node = Rc::new(RefCell::new(CoreStatementNode {
                kind: StatementKind::EXPRESSION((expr.clone(), newline.clone())),
                parent: None,
            }));
            expr.set_parent(WeakASTNode::STATEMENT(WeakStatementNode(Rc::downgrade(
                &node,
            ))));
            newline.set_parent(WeakASTNode::STATEMENT(WeakStatementNode(Rc::downgrade(
                &node,
            ))));
            StatementNode(node)
        }
        pub fn new_with_assignment(assignment: &AssignmentNode) -> Self {
            let node = Rc::new(RefCell::new(CoreStatementNode {
                kind: StatementKind::ASSIGNMENT(assignment.clone()),
                parent: None,
            }));
            assignment.set_parent(WeakASTNode::STATEMENT(WeakStatementNode(Rc::downgrade(
                &node,
            ))));
            StatementNode(node)
        }
        pub fn new_with_variable_declaration(variable_decl: &VariableDeclarationNode) -> Self {
            let node = Rc::new(RefCell::new(CoreStatementNode {
                kind: StatementKind::VARIABLE_DECLARATION(variable_decl.clone()),
                parent: None,
            }));
            variable_decl.set_parent(WeakASTNode::STATEMENT(WeakStatementNode(Rc::downgrade(
                &node,
            ))));
            StatementNode(node)
        }
        pub fn new_with_function_declaration(function_decl: &FunctionDeclarationNode) -> Self {
            let node = Rc::new(RefCell::new(CoreStatementNode {
                kind: StatementKind::FUNCTION_DECLARATION(function_decl.clone()),
                parent: None,
            }));
            function_decl.set_parent(WeakASTNode::STATEMENT(WeakStatementNode(Rc::downgrade(
                &node,
            ))));
            StatementNode(node)
        }
        pub fn new_with_type_declaration(type_decl: &TypeDeclarationNode) -> Self {
            let node = Rc::new(RefCell::new(CoreStatementNode {
                kind: StatementKind::TYPE_DECLARATION(type_decl.clone()),
                parent: None,
            }));
            type_decl.set_parent(WeakASTNode::STATEMENT(WeakStatementNode(Rc::downgrade(
                &node,
            ))));
            StatementNode(node)
        }
        pub fn new_with_struct_stmt(struct_stmt: &StructStatementNode) -> Self {
            let node = Rc::new(RefCell::new(CoreStatementNode {
                kind: StatementKind::STRUCT_STATEMENT(struct_stmt.clone()),
                parent: None,
            }));
            struct_stmt.set_parent(WeakASTNode::STATEMENT(WeakStatementNode(Rc::downgrade(
                &node,
            ))));
            StatementNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreStatementNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreStatementNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for StatementNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    impl ErrornousNode for StatementNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
            lookahead: usize,
        ) -> Self {
            StatementNode(Rc::new(RefCell::new(CoreStatementNode {
                kind: StatementKind::MISSING_TOKENS(MissingTokenNode::new(
                    expected_symbols,
                    received_token,
                    lookahead,
                )),
                parent: None,
            })))
        }
    }
    pub struct CoreAssignmentNode {
        equal: TokenNode,
        l_atom: AtomNode,
        r_assign: RAssignmentNode,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreAssignmentNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreAssignmentNode",
                "equal",
                &&self.equal,
                "l_atom",
                &&self.l_atom,
                "r_assign",
                &&self.r_assign,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreAssignmentNode {
        #[inline]
        fn clone(&self) -> CoreAssignmentNode {
            CoreAssignmentNode {
                equal: ::core::clone::Clone::clone(&self.equal),
                l_atom: ::core::clone::Clone::clone(&self.l_atom),
                r_assign: ::core::clone::Clone::clone(&self.r_assign),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct AssignmentNode(Rc<RefCell<CoreAssignmentNode>>);
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
            let node = Rc::new(RefCell::new(CoreAssignmentNode {
                equal: equal.clone(),
                l_atom: l_atom.clone(),
                r_assign: r_assign.clone(),
                parent: None,
            }));
            l_atom.set_parent(WeakASTNode::ASSIGNMENT(WeakAssignmentNode(Rc::downgrade(
                &node,
            ))));
            r_assign.set_parent(WeakASTNode::ASSIGNMENT(WeakAssignmentNode(Rc::downgrade(
                &node,
            ))));
            equal.set_parent(WeakASTNode::ASSIGNMENT(WeakAssignmentNode(Rc::downgrade(
                &node,
            ))));
            AssignmentNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreAssignmentNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreAssignmentNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for AssignmentNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreStructStatementNode {
        newline: TokenNode,
        name_type_spec: NameTypeSpecNode,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreStructStatementNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "CoreStructStatementNode",
                "newline",
                &&self.newline,
                "name_type_spec",
                &&self.name_type_spec,
                "parent",
                &&self.parent,
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
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct StructStatementNode(Rc<RefCell<CoreStructStatementNode>>);
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
            let node = Rc::new(RefCell::new(CoreStructStatementNode {
                newline: newline.clone(),
                name_type_spec: NameTypeSpecNode::new(param_name, param_type, colon),
                parent: None,
            }));
            StructStatementNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreStructStatementNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreStructStatementNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for StructStatementNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreTypeDeclarationNode {
        kind: TypeDeclarationKind,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreTypeDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreTypeDeclarationNode",
                "kind",
                &&self.kind,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreTypeDeclarationNode {
        #[inline]
        fn clone(&self) -> CoreTypeDeclarationNode {
            CoreTypeDeclarationNode {
                kind: ::core::clone::Clone::clone(&self.kind),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub enum TypeDeclarationKind {
        STRUCT(StructDeclarationNode),
        LAMBDA(LambdaDeclarationNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for TypeDeclarationKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                TypeDeclarationKind::STRUCT(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "STRUCT", &__self_0)
                }
                TypeDeclarationKind::LAMBDA(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "LAMBDA", &__self_0)
                }
                TypeDeclarationKind::MISSING_TOKENS(__self_0) => {
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
    impl ::core::clone::Clone for TypeDeclarationKind {
        #[inline]
        fn clone(&self) -> TypeDeclarationKind {
            match self {
                TypeDeclarationKind::STRUCT(__self_0) => {
                    TypeDeclarationKind::STRUCT(::core::clone::Clone::clone(__self_0))
                }
                TypeDeclarationKind::LAMBDA(__self_0) => {
                    TypeDeclarationKind::LAMBDA(::core::clone::Clone::clone(__self_0))
                }
                TypeDeclarationKind::MISSING_TOKENS(__self_0) => {
                    TypeDeclarationKind::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct TypeDeclarationNode(Rc<RefCell<CoreTypeDeclarationNode>>);
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
            TypeDeclarationNode(Rc::new(RefCell::new(CoreTypeDeclarationNode {
                kind: TypeDeclarationKind::STRUCT(StructDeclarationNode::new(
                    name,
                    block,
                    type_keyword,
                    colon,
                )),
                parent: None,
            })))
        }
        pub fn new_with_lambda(lambda: &LambdaDeclarationNode) -> Self {
            let node = Rc::new(RefCell::new(CoreTypeDeclarationNode {
                kind: TypeDeclarationKind::LAMBDA(lambda.clone()),
                parent: None,
            }));
            lambda.set_parent(WeakASTNode::TYPE_DECLARATION(WeakTypeDeclarationNode(
                Rc::downgrade(&node),
            )));
            TypeDeclarationNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreTypeDeclarationNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreTypeDeclarationNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for TypeDeclarationNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    impl ErrornousNode for TypeDeclarationNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
            lookahead: usize,
        ) -> Self {
            TypeDeclarationNode(Rc::new(RefCell::new(CoreTypeDeclarationNode {
                kind: TypeDeclarationKind::MISSING_TOKENS(MissingTokenNode::new(
                    expected_symbols,
                    received_token,
                    lookahead,
                )),
                parent: None,
            })))
        }
    }
    pub struct CoreStructDeclarationNode {
        type_keyword: TokenNode,
        colon: TokenNode,
        name: TokenNode,
        block: BlockNode,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreStructDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field5_finish(
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
                "parent",
                &&self.parent,
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
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct StructDeclarationNode(Rc<RefCell<CoreStructDeclarationNode>>);
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
            let node = Rc::new(RefCell::new(CoreStructDeclarationNode {
                type_keyword: type_keyword.clone(),
                colon: colon.clone(),
                name: name.clone(),
                block: block.clone(),
                parent: None,
            }));
            name.set_parent(WeakASTNode::STRUCT_DECLARATION(WeakStructDeclarationNode(
                Rc::downgrade(&node),
            )));
            block.set_parent(WeakASTNode::STRUCT_DECLARATION(WeakStructDeclarationNode(
                Rc::downgrade(&node),
            )));
            type_keyword.set_parent(WeakASTNode::STRUCT_DECLARATION(WeakStructDeclarationNode(
                Rc::downgrade(&node),
            )));
            colon.set_parent(WeakASTNode::STRUCT_DECLARATION(WeakStructDeclarationNode(
                Rc::downgrade(&node),
            )));
            StructDeclarationNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreStructDeclarationNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreStructDeclarationNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for StructDeclarationNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreLambdaDeclarationNode {
        kind: LambdaDeclarationKind,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreLambdaDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreLambdaDeclarationNode",
                "kind",
                &&self.kind,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreLambdaDeclarationNode {
        #[inline]
        fn clone(&self) -> CoreLambdaDeclarationNode {
            CoreLambdaDeclarationNode {
                kind: ::core::clone::Clone::clone(&self.kind),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub enum LambdaDeclarationKind {
        OK(OkLambdaDeclarationNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for LambdaDeclarationKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                LambdaDeclarationKind::OK(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK", &__self_0)
                }
                LambdaDeclarationKind::MISSING_TOKENS(__self_0) => {
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
    impl ::core::clone::Clone for LambdaDeclarationKind {
        #[inline]
        fn clone(&self) -> LambdaDeclarationKind {
            match self {
                LambdaDeclarationKind::OK(__self_0) => {
                    LambdaDeclarationKind::OK(::core::clone::Clone::clone(__self_0))
                }
                LambdaDeclarationKind::MISSING_TOKENS(__self_0) => {
                    LambdaDeclarationKind::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct LambdaDeclarationNode(Rc<RefCell<CoreLambdaDeclarationNode>>);
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
            LambdaDeclarationNode(Rc::new(RefCell::new(CoreLambdaDeclarationNode {
                kind: LambdaDeclarationKind::OK(OkLambdaDeclarationNode::new(
                    name,
                    args,
                    return_type,
                    type_keyword,
                    colon,
                    lparen,
                    rparen,
                    right_arrow,
                    newline,
                )),
                parent: None,
            })))
        }
        pub fn core_ref(&self) -> Ref<CoreLambdaDeclarationNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreLambdaDeclarationNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for LambdaDeclarationNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    impl ErrornousNode for LambdaDeclarationNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
            lookahead: usize,
        ) -> Self {
            LambdaDeclarationNode(Rc::new(RefCell::new(CoreLambdaDeclarationNode {
                kind: LambdaDeclarationKind::MISSING_TOKENS(MissingTokenNode::new(
                    expected_symbols,
                    received_token,
                    lookahead,
                )),
                parent: None,
            })))
        }
    }
    pub struct CoreOkLambdaDeclarationNode {
        type_keyword: TokenNode,
        colon: TokenNode,
        lparen: TokenNode,
        rparen: TokenNode,
        right_arrow: Option<TokenNode>,
        newline: TokenNode,
        name: TokenNode,
        args: Option<NameTypeSpecsNode>,
        return_type: Option<TypeExpressionNode>,
        parent: Option<WeakASTNode>,
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
                "parent",
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
                &&self.parent,
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
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct OkLambdaDeclarationNode(Rc<RefCell<CoreOkLambdaDeclarationNode>>);
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
            let node = Rc::new(RefCell::new(CoreOkLambdaDeclarationNode {
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
                parent: None,
            }));
            name.set_parent(WeakASTNode::OK_LAMBDA_DECLARATION(
                WeakOkLambdaDeclarationNode(Rc::downgrade(&node)),
            ));
            type_keyword.set_parent(WeakASTNode::OK_LAMBDA_DECLARATION(
                WeakOkLambdaDeclarationNode(Rc::downgrade(&node)),
            ));
            colon.set_parent(WeakASTNode::OK_LAMBDA_DECLARATION(
                WeakOkLambdaDeclarationNode(Rc::downgrade(&node)),
            ));
            lparen.set_parent(WeakASTNode::OK_LAMBDA_DECLARATION(
                WeakOkLambdaDeclarationNode(Rc::downgrade(&node)),
            ));
            rparen.set_parent(WeakASTNode::OK_LAMBDA_DECLARATION(
                WeakOkLambdaDeclarationNode(Rc::downgrade(&node)),
            ));
            newline.set_parent(WeakASTNode::OK_LAMBDA_DECLARATION(
                WeakOkLambdaDeclarationNode(Rc::downgrade(&node)),
            ));
            match args {
                Some(args) => {
                    args.set_parent(WeakASTNode::OK_LAMBDA_DECLARATION(
                        WeakOkLambdaDeclarationNode(Rc::downgrade(&node)),
                    ));
                }
                None => {}
            }
            match return_type {
                Some(return_type) => {
                    return_type.set_parent(WeakASTNode::OK_LAMBDA_DECLARATION(
                        WeakOkLambdaDeclarationNode(Rc::downgrade(&node)),
                    ));
                }
                None => {}
            }
            match right_arrow {
                Some(right_arrow) => {
                    right_arrow.set_parent(WeakASTNode::OK_LAMBDA_DECLARATION(
                        WeakOkLambdaDeclarationNode(Rc::downgrade(&node)),
                    ));
                }
                None => {}
            };
            OkLambdaDeclarationNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreOkLambdaDeclarationNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreOkLambdaDeclarationNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for OkLambdaDeclarationNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreFunctionDeclarationNode {
        pub kind: FunctionDeclarationKind,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreFunctionDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreFunctionDeclarationNode",
                "kind",
                &&self.kind,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreFunctionDeclarationNode {
        #[inline]
        fn clone(&self) -> CoreFunctionDeclarationNode {
            CoreFunctionDeclarationNode {
                kind: ::core::clone::Clone::clone(&self.kind),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub enum FunctionDeclarationKind {
        OK(OkFunctionDeclarationNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for FunctionDeclarationKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                FunctionDeclarationKind::OK(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK", &__self_0)
                }
                FunctionDeclarationKind::MISSING_TOKENS(__self_0) => {
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
    impl ::core::clone::Clone for FunctionDeclarationKind {
        #[inline]
        fn clone(&self) -> FunctionDeclarationKind {
            match self {
                FunctionDeclarationKind::OK(__self_0) => {
                    FunctionDeclarationKind::OK(::core::clone::Clone::clone(__self_0))
                }
                FunctionDeclarationKind::MISSING_TOKENS(__self_0) => {
                    FunctionDeclarationKind::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct FunctionDeclarationNode(pub Rc<RefCell<CoreFunctionDeclarationNode>>);
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
            let node = Rc::new(RefCell::new(CoreFunctionDeclarationNode {
                kind: FunctionDeclarationKind::OK(OkFunctionDeclarationNode::new(
                    name,
                    args,
                    return_type,
                    block,
                    func_keyword,
                    lparen,
                    rparen,
                    right_arrow,
                    colon,
                )),
                parent: None,
            }));
            FunctionDeclarationNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreFunctionDeclarationNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreFunctionDeclarationNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for FunctionDeclarationNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    impl ErrornousNode for FunctionDeclarationNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
            lookahead: usize,
        ) -> Self {
            FunctionDeclarationNode(Rc::new(RefCell::new(CoreFunctionDeclarationNode {
                kind: FunctionDeclarationKind::MISSING_TOKENS(MissingTokenNode::new(
                    expected_symbols,
                    received_token,
                    lookahead,
                )),
                parent: None,
            })))
        }
    }
    pub struct CoreOkFunctionDeclarationNode {
        func_keyword: FuncKeywordKind,
        lparen: TokenNode,
        rparen: TokenNode,
        right_arrow: Option<TokenNode>,
        colon: TokenNode,
        pub name: Option<TokenNode>,
        pub args: Option<NameTypeSpecsNode>,
        pub return_type: Option<TypeExpressionNode>,
        pub block: BlockNode,
        parent: Option<WeakASTNode>,
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
                "parent",
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
                &&self.parent,
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
                parent: ::core::clone::Clone::clone(&self.parent),
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
    pub struct OkFunctionDeclarationNode(pub Rc<RefCell<CoreOkFunctionDeclarationNode>>);
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
            let node = Rc::new(RefCell::new(CoreOkFunctionDeclarationNode {
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
                parent: None,
            }));
            block.set_parent(WeakASTNode::OK_FUNCTION_DECLARATION(
                WeakOkFunctionDeclarationNode(Rc::downgrade(&node)),
            ));
            lparen.set_parent(WeakASTNode::OK_FUNCTION_DECLARATION(
                WeakOkFunctionDeclarationNode(Rc::downgrade(&node)),
            ));
            rparen.set_parent(WeakASTNode::OK_FUNCTION_DECLARATION(
                WeakOkFunctionDeclarationNode(Rc::downgrade(&node)),
            ));
            colon.set_parent(WeakASTNode::OK_FUNCTION_DECLARATION(
                WeakOkFunctionDeclarationNode(Rc::downgrade(&node)),
            ));
            match name {
                Some(name) => {
                    name.set_parent(WeakASTNode::OK_FUNCTION_DECLARATION(
                        WeakOkFunctionDeclarationNode(Rc::downgrade(&node)),
                    ));
                }
                None => {}
            }
            match args {
                Some(args) => {
                    args.set_parent(WeakASTNode::OK_FUNCTION_DECLARATION(
                        WeakOkFunctionDeclarationNode(Rc::downgrade(&node)),
                    ));
                }
                None => {}
            }
            match return_type {
                Some(return_type) => {
                    return_type.set_parent(WeakASTNode::OK_FUNCTION_DECLARATION(
                        WeakOkFunctionDeclarationNode(Rc::downgrade(&node)),
                    ));
                }
                None => {}
            }
            match right_arrow {
                Some(right_arrow) => {
                    right_arrow.set_parent(WeakASTNode::OK_FUNCTION_DECLARATION(
                        WeakOkFunctionDeclarationNode(Rc::downgrade(&node)),
                    ));
                }
                None => {}
            };
            match func_keyword {
                FuncKeywordKind::DEF(def_node) => {
                    def_node.set_parent(WeakASTNode::OK_FUNCTION_DECLARATION(
                        WeakOkFunctionDeclarationNode(Rc::downgrade(&node)),
                    ));
                }
                FuncKeywordKind::FUNC(func_node) => {
                    func_node.set_parent(WeakASTNode::OK_FUNCTION_DECLARATION(
                        WeakOkFunctionDeclarationNode(Rc::downgrade(&node)),
                    ));
                }
            }
            OkFunctionDeclarationNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreOkFunctionDeclarationNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreOkFunctionDeclarationNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for OkFunctionDeclarationNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreVariableDeclarationNode {
        let_keyword: TokenNode,
        equal: TokenNode,
        pub name: TokenNode,
        pub r_assign: RAssignmentNode,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreVariableDeclarationNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field5_finish(
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
                "parent",
                &&self.parent,
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
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct VariableDeclarationNode(pub Rc<RefCell<CoreVariableDeclarationNode>>);
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
            let node = Rc::new(RefCell::new(CoreVariableDeclarationNode {
                let_keyword: let_keyword.clone(),
                equal: equal.clone(),
                name: name.clone(),
                r_assign: r_assign.clone(),
                parent: None,
            }));
            name.set_parent(WeakASTNode::VARIABLE_DECLARATION(
                WeakVariableDeclarationNode(Rc::downgrade(&node)),
            ));
            r_assign.set_parent(WeakASTNode::VARIABLE_DECLARATION(
                WeakVariableDeclarationNode(Rc::downgrade(&node)),
            ));
            let_keyword.set_parent(WeakASTNode::VARIABLE_DECLARATION(
                WeakVariableDeclarationNode(Rc::downgrade(&node)),
            ));
            equal.set_parent(WeakASTNode::VARIABLE_DECLARATION(
                WeakVariableDeclarationNode(Rc::downgrade(&node)),
            ));
            VariableDeclarationNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreVariableDeclarationNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreVariableDeclarationNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for VariableDeclarationNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreNameTypeSpecsNode {
        kind: NameTypeSpecsKind,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreNameTypeSpecsNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreNameTypeSpecsNode",
                "kind",
                &&self.kind,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreNameTypeSpecsNode {
        #[inline]
        fn clone(&self) -> CoreNameTypeSpecsNode {
            CoreNameTypeSpecsNode {
                kind: ::core::clone::Clone::clone(&self.kind),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub enum NameTypeSpecsKind {
        OK(OkNameTypeSpecsNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for NameTypeSpecsKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                NameTypeSpecsKind::OK(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK", &__self_0)
                }
                NameTypeSpecsKind::MISSING_TOKENS(__self_0) => {
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
    impl ::core::clone::Clone for NameTypeSpecsKind {
        #[inline]
        fn clone(&self) -> NameTypeSpecsKind {
            match self {
                NameTypeSpecsKind::OK(__self_0) => {
                    NameTypeSpecsKind::OK(::core::clone::Clone::clone(__self_0))
                }
                NameTypeSpecsKind::MISSING_TOKENS(__self_0) => {
                    NameTypeSpecsKind::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct NameTypeSpecsNode(Rc<RefCell<CoreNameTypeSpecsNode>>);
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
            let node = Rc::new(RefCell::new(CoreNameTypeSpecsNode {
                kind: NameTypeSpecsKind::OK(ok_name_type_specs.clone()),
                parent: None,
            }));
            ok_name_type_specs.set_parent(WeakASTNode::NAME_TYPE_SPECS(WeakNameTypeSpecsNode(
                Rc::downgrade(&node),
            )));
            NameTypeSpecsNode(node)
        }
        pub fn get_name_type_spec_objs(
            &self,
            code: &Code,
        ) -> Vec<(Option<Rc<String>>, Option<Type>)> {
            match &self.core_ref().kind {
                NameTypeSpecsKind::OK(ok_name_type_specs) => {
                    ok_name_type_specs.get_name_type_spec_objs(code)
                }
                _ => ::alloc::vec::Vec::new(),
            }
        }
        pub fn core_ref(&self) -> Ref<CoreNameTypeSpecsNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreNameTypeSpecsNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for NameTypeSpecsNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    impl ErrornousNode for NameTypeSpecsNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
            lookahead: usize,
        ) -> Self {
            NameTypeSpecsNode(Rc::new(RefCell::new(CoreNameTypeSpecsNode {
                kind: NameTypeSpecsKind::MISSING_TOKENS(MissingTokenNode::new(
                    expected_symbols,
                    received_token,
                    lookahead,
                )),
                parent: None,
            })))
        }
    }
    pub struct CoreOkNameTypeSpecsNode {
        comma: Option<TokenNode>,
        arg: NameTypeSpecNode,
        remaining_args: Option<NameTypeSpecsNode>,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreOkNameTypeSpecsNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreOkNameTypeSpecsNode",
                "comma",
                &&self.comma,
                "arg",
                &&self.arg,
                "remaining_args",
                &&self.remaining_args,
                "parent",
                &&self.parent,
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
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct OkNameTypeSpecsNode(Rc<RefCell<CoreOkNameTypeSpecsNode>>);
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
            let node = Rc::new(RefCell::new(CoreOkNameTypeSpecsNode {
                comma: Some(comma.clone()),
                arg: arg.clone(),
                remaining_args: Some(remaining_args.clone()),
                parent: None,
            }));
            arg.set_parent(WeakASTNode::OK_NAME_TYPE_SPECS(WeakOkNameTypeSpecsNode(
                Rc::downgrade(&node),
            )));
            remaining_args.set_parent(WeakASTNode::OK_NAME_TYPE_SPECS(WeakOkNameTypeSpecsNode(
                Rc::downgrade(&node),
            )));
            comma.set_parent(WeakASTNode::OK_NAME_TYPE_SPECS(WeakOkNameTypeSpecsNode(
                Rc::downgrade(&node),
            )));
            OkNameTypeSpecsNode(node)
        }
        pub fn new_with_single_arg(arg: &NameTypeSpecNode) -> Self {
            let node = Rc::new(RefCell::new(CoreOkNameTypeSpecsNode {
                comma: None,
                arg: arg.clone(),
                remaining_args: None,
                parent: None,
            }));
            arg.set_parent(WeakASTNode::OK_NAME_TYPE_SPECS(WeakOkNameTypeSpecsNode(
                Rc::downgrade(&node),
            )));
            OkNameTypeSpecsNode(node)
        }
        pub fn get_name_type_spec_objs(
            &self,
            code: &Code,
        ) -> Vec<(Option<Rc<String>>, Option<Type>)> {
            let mut name_type_specs_vec: Vec<(Option<Rc<String>>, Option<Type>)> =
                ::alloc::vec::Vec::new();
            let arg_obj = self.core_ref().arg.get_name_spec_obj(code);
            name_type_specs_vec.push(arg_obj);
            match &self.core_ref().remaining_args {
                Some(remaining_args) => {
                    let mut remaining_args_objs = remaining_args.get_name_type_spec_objs(code);
                    name_type_specs_vec.append(&mut remaining_args_objs);
                }
                None => {}
            }
            name_type_specs_vec
        }
        pub fn core_ref(&self) -> Ref<CoreOkNameTypeSpecsNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreOkNameTypeSpecsNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for OkNameTypeSpecsNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreNameTypeSpecNode {
        colon: TokenNode,
        param_name: TokenNode,
        param_type: TypeExpressionNode,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreNameTypeSpecNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreNameTypeSpecNode",
                "colon",
                &&self.colon,
                "param_name",
                &&self.param_name,
                "param_type",
                &&self.param_type,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreNameTypeSpecNode {
        #[inline]
        fn clone(&self) -> CoreNameTypeSpecNode {
            CoreNameTypeSpecNode {
                colon: ::core::clone::Clone::clone(&self.colon),
                param_name: ::core::clone::Clone::clone(&self.param_name),
                param_type: ::core::clone::Clone::clone(&self.param_type),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct NameTypeSpecNode(Rc<RefCell<CoreNameTypeSpecNode>>);
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
            let node = Rc::new(RefCell::new(CoreNameTypeSpecNode {
                colon: colon.clone(),
                param_name: param_name.clone(),
                param_type: param_type.clone(),
                parent: None,
            }));
            param_name.set_parent(WeakASTNode::NAME_TYPE_SPEC(WeakNameTypeSpecNode(
                Rc::downgrade(&node),
            )));
            param_type.set_parent(WeakASTNode::NAME_TYPE_SPEC(WeakNameTypeSpecNode(
                Rc::downgrade(&node),
            )));
            colon.set_parent(WeakASTNode::NAME_TYPE_SPEC(WeakNameTypeSpecNode(
                Rc::downgrade(&node),
            )));
            NameTypeSpecNode(node)
        }
        pub fn get_name_spec_obj(&self, code: &Code) -> (Option<Rc<String>>, Option<Type>) {
            let name = match self.core_ref().param_name.get_ok() {
                Some(ok_name_node) => Some(Rc::new(ok_name_node.token_value(code))),
                None => None,
            };
            let type_obj = match self.core_ref().param_type.get_type_obj(code) {
                Some(type_obj) => Some(type_obj),
                None => None,
            };
            (name, type_obj)
        }
        pub fn core_ref(&self) -> Ref<CoreNameTypeSpecNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreNameTypeSpecNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for NameTypeSpecNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreTypeExpressionNode {
        kind: TypeExpressionKind,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreTypeExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreTypeExpressionNode",
                "kind",
                &&self.kind,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreTypeExpressionNode {
        #[inline]
        fn clone(&self) -> CoreTypeExpressionNode {
            CoreTypeExpressionNode {
                kind: ::core::clone::Clone::clone(&self.kind),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub enum TypeExpressionKind {
        ATOMIC(AtomicTypeNode),
        USER_DEFINED(UserDefinedTypeNode),
        ARRAY(ArrayTypeNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for TypeExpressionKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                TypeExpressionKind::ATOMIC(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ATOMIC", &__self_0)
                }
                TypeExpressionKind::USER_DEFINED(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "USER_DEFINED", &__self_0)
                }
                TypeExpressionKind::ARRAY(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ARRAY", &__self_0)
                }
                TypeExpressionKind::MISSING_TOKENS(__self_0) => {
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
    impl ::core::clone::Clone for TypeExpressionKind {
        #[inline]
        fn clone(&self) -> TypeExpressionKind {
            match self {
                TypeExpressionKind::ATOMIC(__self_0) => {
                    TypeExpressionKind::ATOMIC(::core::clone::Clone::clone(__self_0))
                }
                TypeExpressionKind::USER_DEFINED(__self_0) => {
                    TypeExpressionKind::USER_DEFINED(::core::clone::Clone::clone(__self_0))
                }
                TypeExpressionKind::ARRAY(__self_0) => {
                    TypeExpressionKind::ARRAY(::core::clone::Clone::clone(__self_0))
                }
                TypeExpressionKind::MISSING_TOKENS(__self_0) => {
                    TypeExpressionKind::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct TypeExpressionNode(Rc<RefCell<CoreTypeExpressionNode>>);
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
            TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode {
                kind: TypeExpressionKind::ATOMIC(AtomicTypeNode::new(atomic_type)),
                parent: None,
            })))
        }
        pub fn new_with_user_defined_type(identifier: &TokenNode) -> Self {
            TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode {
                kind: TypeExpressionKind::USER_DEFINED(UserDefinedTypeNode::new(identifier)),
                parent: None,
            })))
        }
        pub fn new_with_array_type(
            array_size: &TokenNode,
            sub_type: &TypeExpressionNode,
            lsquare: &TokenNode,
            rsquare: &TokenNode,
            semicolon: &TokenNode,
        ) -> Self {
            TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode {
                kind: TypeExpressionKind::ARRAY(ArrayTypeNode::new(
                    array_size, sub_type, lsquare, rsquare, semicolon,
                )),
                parent: None,
            })))
        }
        pub fn get_type_obj(&self, code: &Code) -> Option<Type> {
            match &self.core_ref().kind {
                TypeExpressionKind::ATOMIC(atomic_type) => atomic_type.get_type_obj(code),
                TypeExpressionKind::USER_DEFINED(user_defined_type) => {
                    user_defined_type.get_type_obj(code)
                }
                TypeExpressionKind::ARRAY(array_type) => array_type.get_type_obj(code),
                _ => None,
            }
        }
        pub fn core_ref(&self) -> Ref<CoreTypeExpressionNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreTypeExpressionNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for TypeExpressionNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    impl ErrornousNode for TypeExpressionNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
            lookahead: usize,
        ) -> Self {
            TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode {
                kind: TypeExpressionKind::MISSING_TOKENS(MissingTokenNode::new(
                    expected_symbols,
                    received_token,
                    lookahead,
                )),
                parent: None,
            })))
        }
    }
    pub struct CoreAtomicTypeNode {
        kind: TokenNode,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreAtomicTypeNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreAtomicTypeNode",
                "kind",
                &&self.kind,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreAtomicTypeNode {
        #[inline]
        fn clone(&self) -> CoreAtomicTypeNode {
            CoreAtomicTypeNode {
                kind: ::core::clone::Clone::clone(&self.kind),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct AtomicTypeNode(Rc<RefCell<CoreAtomicTypeNode>>);
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
            let node = Rc::new(RefCell::new(CoreAtomicTypeNode {
                kind: token.clone(),
                parent: None,
            }));
            token.set_parent(WeakASTNode::ATOMIC_TYPE(WeakAtomicTypeNode(Rc::downgrade(
                &node,
            ))));
            AtomicTypeNode(node)
        }
        pub fn get_type_obj(&self, code: &Code) -> Option<Type> {
            match self.core_ref().kind.get_ok() {
                Some(ok_atomic_type) => {
                    let atomic_type_str = ok_atomic_type.token_value(code);
                    return Atomic::new_with_type_str(&atomic_type_str);
                }
                None => return None,
            }
        }
        pub fn core_ref(&self) -> Ref<CoreAtomicTypeNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreAtomicTypeNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for AtomicTypeNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreArrayTypeNode {
        lsquare: TokenNode,
        rsquare: TokenNode,
        semicolon: TokenNode,
        sub_type: TypeExpressionNode,
        size: TokenNode,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreArrayTypeNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            let names: &'static _ = &[
                "lsquare",
                "rsquare",
                "semicolon",
                "sub_type",
                "size",
                "parent",
            ];
            let values: &[&dyn ::core::fmt::Debug] = &[
                &&self.lsquare,
                &&self.rsquare,
                &&self.semicolon,
                &&self.sub_type,
                &&self.size,
                &&self.parent,
            ];
            ::core::fmt::Formatter::debug_struct_fields_finish(
                f,
                "CoreArrayTypeNode",
                names,
                values,
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
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct ArrayTypeNode(Rc<RefCell<CoreArrayTypeNode>>);
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
            let node = Rc::new(RefCell::new(CoreArrayTypeNode {
                lsquare: lsquare.clone(),
                rsquare: rsquare.clone(),
                semicolon: semicolon.clone(),
                sub_type: sub_type.clone(),
                size: size.clone(),
                parent: None,
            }));
            size.set_parent(WeakASTNode::ARRAY_TYPE(WeakArrayTypeNode(Rc::downgrade(
                &node,
            ))));
            sub_type.set_parent(WeakASTNode::ARRAY_TYPE(WeakArrayTypeNode(Rc::downgrade(
                &node,
            ))));
            lsquare.set_parent(WeakASTNode::ARRAY_TYPE(WeakArrayTypeNode(Rc::downgrade(
                &node,
            ))));
            rsquare.set_parent(WeakASTNode::ARRAY_TYPE(WeakArrayTypeNode(Rc::downgrade(
                &node,
            ))));
            semicolon.set_parent(WeakASTNode::ARRAY_TYPE(WeakArrayTypeNode(Rc::downgrade(
                &node,
            ))));
            ArrayTypeNode(node)
        }
        pub fn get_type_obj(&self, code: &Code) -> Option<Type> {
            match self.core_ref().sub_type.get_type_obj(code) {
                Some(sub_type_obj) => match self.core_ref().size.get_ok() {
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
        pub fn core_ref(&self) -> Ref<CoreArrayTypeNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreArrayTypeNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for ArrayTypeNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreUserDefinedTypeNode {
        token: TokenNode,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreUserDefinedTypeNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreUserDefinedTypeNode",
                "token",
                &&self.token,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreUserDefinedTypeNode {
        #[inline]
        fn clone(&self) -> CoreUserDefinedTypeNode {
            CoreUserDefinedTypeNode {
                token: ::core::clone::Clone::clone(&self.token),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct UserDefinedTypeNode(Rc<RefCell<CoreUserDefinedTypeNode>>);
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
            let node = Rc::new(RefCell::new(CoreUserDefinedTypeNode {
                token: identifier.clone(),
                parent: None,
            }));
            identifier.set_parent(WeakASTNode::USER_DEFINED_TYPE(WeakUserDefinedTypeNode(
                Rc::downgrade(&node),
            )));
            UserDefinedTypeNode(node)
        }
        pub fn get_type_obj(&self, code: &Code) -> Option<Type> {
            match self.core_ref().token.get_ok() {
                Some(ok_token_node) => {
                    Some(Type::new_with_user_defined(ok_token_node.token_value(code)))
                }
                None => None,
            }
        }
        pub fn core_ref(&self) -> Ref<CoreUserDefinedTypeNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreUserDefinedTypeNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for UserDefinedTypeNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreTokenNode {
        pub kind: TokenKind,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreTokenNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreTokenNode",
                "kind",
                &&self.kind,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreTokenNode {
        #[inline]
        fn clone(&self) -> CoreTokenNode {
            CoreTokenNode {
                kind: ::core::clone::Clone::clone(&self.kind),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub enum TokenKind {
        OK(OkTokenNode),
        MISSING_TOKENS(MissingTokenNode),
        SKIPPED(SkippedTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for TokenKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                TokenKind::OK(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK", &__self_0)
                }
                TokenKind::MISSING_TOKENS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "MISSING_TOKENS",
                        &__self_0,
                    )
                }
                TokenKind::SKIPPED(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "SKIPPED", &__self_0)
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for TokenKind {
        #[inline]
        fn clone(&self) -> TokenKind {
            match self {
                TokenKind::OK(__self_0) => TokenKind::OK(::core::clone::Clone::clone(__self_0)),
                TokenKind::MISSING_TOKENS(__self_0) => {
                    TokenKind::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
                TokenKind::SKIPPED(__self_0) => {
                    TokenKind::SKIPPED(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct TokenNode(pub Rc<RefCell<CoreTokenNode>>);
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
        pub fn new_with_ok_token(token: &Token, lookahead: usize, kind: OkTokenKind) -> Self {
            TokenNode(Rc::new(RefCell::new(CoreTokenNode {
                kind: TokenKind::OK(OkTokenNode::new(token, lookahead, kind)),
                parent: None,
            })))
        }
        pub fn new_with_skipped_token(skipped_token: &Token, lookahead: usize) -> Self {
            TokenNode(Rc::new(RefCell::new(CoreTokenNode {
                kind: TokenKind::SKIPPED(SkippedTokenNode::new(skipped_token, lookahead)),
                parent: None,
            })))
        }
        pub fn is_ok(&self) -> Option<TokenNode> {
            match &self.core_ref().kind {
                TokenKind::OK(_) => Some(self.clone()),
                _ => None,
            }
        }
        pub fn get_ok(&self) -> Option<OkTokenNode> {
            match &self.core_ref().kind {
                TokenKind::OK(ok_token_node) => Some(ok_token_node.clone()),
                _ => None,
            }
        }
        pub fn is_binary_operator(&self) -> Option<BinaryOperatorKind> {
            match &self.core_ref().kind {
                TokenKind::OK(ok_token) => match ok_token.is_binary_operator() {
                    Some(operator) => return Some(operator),
                    None => None,
                },
                _ => None,
            }
        }
        pub fn core_ref(&self) -> Ref<CoreTokenNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreTokenNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for TokenNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    impl ErrornousNode for TokenNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
            lookahead: usize,
        ) -> Self {
            TokenNode(Rc::new(RefCell::new(CoreTokenNode {
                kind: TokenKind::MISSING_TOKENS(MissingTokenNode::new(
                    expected_symbols,
                    received_token,
                    lookahead,
                )),
                parent: None,
            })))
        }
    }
    pub struct CoreOkTokenNode {
        token: Token,
        kind: OkTokenKind,
        lookahead: usize,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreOkTokenNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreOkTokenNode",
                "token",
                &&self.token,
                "kind",
                &&self.kind,
                "lookahead",
                &&self.lookahead,
                "parent",
                &&self.parent,
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
                lookahead: ::core::clone::Clone::clone(&self.lookahead),
                parent: ::core::clone::Clone::clone(&self.parent),
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
    pub struct OkTokenNode(Rc<RefCell<CoreOkTokenNode>>);
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
        pub fn new(token: &Token, lookahead: usize, kind: OkTokenKind) -> Self {
            OkTokenNode(Rc::new(RefCell::new(CoreOkTokenNode {
                token: token.clone(),
                kind,
                lookahead,
                parent: None,
            })))
        }
        pub fn is_binary_operator(&self) -> Option<BinaryOperatorKind> {
            match &self.core_ref().token.core_token {
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
            self.core_ref().token.token_value(code)
        }
        pub fn is_identifier(&self) -> bool {
            match self.core_ref().kind {
                OkTokenKind::IDENTIFIER(_) => true,
                _ => false,
            }
        }
        pub fn core_ref(&self) -> Ref<CoreOkTokenNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreOkTokenNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for OkTokenNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreMissingTokenNode {
        expected_symbols: Rc<Vec<&'static str>>,
        received_token: Token,
        lookahead: usize,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreMissingTokenNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreMissingTokenNode",
                "expected_symbols",
                &&self.expected_symbols,
                "received_token",
                &&self.received_token,
                "lookahead",
                &&self.lookahead,
                "parent",
                &&self.parent,
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
                lookahead: ::core::clone::Clone::clone(&self.lookahead),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct MissingTokenNode(Rc<RefCell<CoreMissingTokenNode>>);
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
        pub fn new(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
            lookahead: usize,
        ) -> Self {
            MissingTokenNode(Rc::new(RefCell::new(CoreMissingTokenNode {
                expected_symbols: expected_symbols.clone(),
                received_token: received_token.clone(),
                lookahead,
                parent: None,
            })))
        }
        pub fn core_ref(&self) -> Ref<CoreMissingTokenNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreMissingTokenNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for MissingTokenNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreSkippedTokenNode {
        skipped_token: Token,
        lookahead: usize,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreSkippedTokenNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field3_finish(
                f,
                "CoreSkippedTokenNode",
                "skipped_token",
                &&self.skipped_token,
                "lookahead",
                &&self.lookahead,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreSkippedTokenNode {
        #[inline]
        fn clone(&self) -> CoreSkippedTokenNode {
            CoreSkippedTokenNode {
                skipped_token: ::core::clone::Clone::clone(&self.skipped_token),
                lookahead: ::core::clone::Clone::clone(&self.lookahead),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct SkippedTokenNode(Rc<RefCell<CoreSkippedTokenNode>>);
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
        pub fn new(skipped_token: &Token, lookahead: usize) -> Self {
            SkippedTokenNode(Rc::new(RefCell::new(CoreSkippedTokenNode {
                skipped_token: skipped_token.clone(),
                lookahead,
                parent: None,
            })))
        }
        pub fn index(&self) -> usize {
            self.core_ref().skipped_token.index()
        }
        pub fn line_number(&self) -> usize {
            self.core_ref().skipped_token.line_number
        }
        pub fn core_ref(&self) -> Ref<CoreSkippedTokenNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreSkippedTokenNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for SkippedTokenNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreExpressionNode {
        pub kind: ExpressionKind,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreExpressionNode",
                "kind",
                &&self.kind,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreExpressionNode {
        #[inline]
        fn clone(&self) -> CoreExpressionNode {
            CoreExpressionNode {
                kind: ::core::clone::Clone::clone(&self.kind),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub enum ExpressionKind {
        UNARY(UnaryExpressionNode),
        BINARY(BinaryExpressionNode),
        LOGICAL(LogicalExpressionNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ExpressionKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                ExpressionKind::UNARY(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "UNARY", &__self_0)
                }
                ExpressionKind::BINARY(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "BINARY", &__self_0)
                }
                ExpressionKind::LOGICAL(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "LOGICAL", &__self_0)
                }
                ExpressionKind::MISSING_TOKENS(__self_0) => {
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
    impl ::core::clone::Clone for ExpressionKind {
        #[inline]
        fn clone(&self) -> ExpressionKind {
            match self {
                ExpressionKind::UNARY(__self_0) => {
                    ExpressionKind::UNARY(::core::clone::Clone::clone(__self_0))
                }
                ExpressionKind::BINARY(__self_0) => {
                    ExpressionKind::BINARY(::core::clone::Clone::clone(__self_0))
                }
                ExpressionKind::LOGICAL(__self_0) => {
                    ExpressionKind::LOGICAL(::core::clone::Clone::clone(__self_0))
                }
                ExpressionKind::MISSING_TOKENS(__self_0) => {
                    ExpressionKind::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct ExpressionNode(pub Rc<RefCell<CoreExpressionNode>>);
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
            let node = Rc::new(RefCell::new(CoreExpressionNode {
                kind: ExpressionKind::UNARY(unary_expr.clone()),
                parent: None,
            }));
            unary_expr.set_parent(WeakASTNode::EXPRESSION(WeakExpressionNode(Rc::downgrade(
                &node,
            ))));
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
            let node = Rc::new(RefCell::new(CoreExpressionNode {
                kind: ExpressionKind::BINARY(BinaryExpressionNode::new(
                    operator_kind,
                    left_expr,
                    right_expr,
                )),
                parent: None,
            }));
            ExpressionNode(node)
        }
        pub fn new_with_logical(
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
            let node = Rc::new(RefCell::new(CoreExpressionNode {
                kind: ExpressionKind::LOGICAL(LogicalExpressionNode::new(
                    operator_kind,
                    left_expr,
                    right_expr,
                )),
                parent: None,
            }));
            ExpressionNode(node)
        }
        pub fn is_valid_l_value(&self) -> Option<AtomNode> {
            match &self.core_ref().kind {
                ExpressionKind::UNARY(unary_expr_node) => match &unary_expr_node.core_ref().kind {
                    UnaryExpressionKind::ATOMIC(atomic_expr_node) => {
                        match &atomic_expr_node.core_ref().kind {
                            AtomicExpressionKind::ATOM(atom_node) => {
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
        pub fn core_ref(&self) -> Ref<CoreExpressionNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreExpressionNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for ExpressionNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    impl ErrornousNode for ExpressionNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
            lookahead: usize,
        ) -> Self {
            ExpressionNode(Rc::new(RefCell::new(CoreExpressionNode {
                kind: ExpressionKind::MISSING_TOKENS(MissingTokenNode::new(
                    expected_symbols,
                    received_token,
                    lookahead,
                )),
                parent: None,
            })))
        }
    }
    pub struct CoreAtomicExpressionNode {
        kind: AtomicExpressionKind,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreAtomicExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreAtomicExpressionNode",
                "kind",
                &&self.kind,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreAtomicExpressionNode {
        #[inline]
        fn clone(&self) -> CoreAtomicExpressionNode {
            CoreAtomicExpressionNode {
                kind: ::core::clone::Clone::clone(&self.kind),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub enum AtomicExpressionKind {
        BOOL_VALUE(TokenNode),
        INTEGER(TokenNode),
        FLOATING_POINT_NUMBER(TokenNode),
        LITERAL(TokenNode),
        PARENTHESISED_EXPRESSION(ParenthesisedExpressionNode),
        ATOM(AtomNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for AtomicExpressionKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                AtomicExpressionKind::BOOL_VALUE(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "BOOL_VALUE", &__self_0)
                }
                AtomicExpressionKind::INTEGER(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "INTEGER", &__self_0)
                }
                AtomicExpressionKind::FLOATING_POINT_NUMBER(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "FLOATING_POINT_NUMBER",
                        &__self_0,
                    )
                }
                AtomicExpressionKind::LITERAL(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "LITERAL", &__self_0)
                }
                AtomicExpressionKind::PARENTHESISED_EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "PARENTHESISED_EXPRESSION",
                        &__self_0,
                    )
                }
                AtomicExpressionKind::ATOM(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ATOM", &__self_0)
                }
                AtomicExpressionKind::MISSING_TOKENS(__self_0) => {
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
    impl ::core::clone::Clone for AtomicExpressionKind {
        #[inline]
        fn clone(&self) -> AtomicExpressionKind {
            match self {
                AtomicExpressionKind::BOOL_VALUE(__self_0) => {
                    AtomicExpressionKind::BOOL_VALUE(::core::clone::Clone::clone(__self_0))
                }
                AtomicExpressionKind::INTEGER(__self_0) => {
                    AtomicExpressionKind::INTEGER(::core::clone::Clone::clone(__self_0))
                }
                AtomicExpressionKind::FLOATING_POINT_NUMBER(__self_0) => {
                    AtomicExpressionKind::FLOATING_POINT_NUMBER(::core::clone::Clone::clone(
                        __self_0,
                    ))
                }
                AtomicExpressionKind::LITERAL(__self_0) => {
                    AtomicExpressionKind::LITERAL(::core::clone::Clone::clone(__self_0))
                }
                AtomicExpressionKind::PARENTHESISED_EXPRESSION(__self_0) => {
                    AtomicExpressionKind::PARENTHESISED_EXPRESSION(::core::clone::Clone::clone(
                        __self_0,
                    ))
                }
                AtomicExpressionKind::ATOM(__self_0) => {
                    AtomicExpressionKind::ATOM(::core::clone::Clone::clone(__self_0))
                }
                AtomicExpressionKind::MISSING_TOKENS(__self_0) => {
                    AtomicExpressionKind::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct AtomicExpressionNode(Rc<RefCell<CoreAtomicExpressionNode>>);
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
            let node = Rc::new(RefCell::new(CoreAtomicExpressionNode {
                kind: AtomicExpressionKind::BOOL_VALUE(bool_value.clone()),
                parent: None,
            }));
            bool_value.set_parent(WeakASTNode::ATOMIC_EXPRESSION(WeakAtomicExpressionNode(
                Rc::downgrade(&node),
            )));
            AtomicExpressionNode(node)
        }
        pub fn new_with_integer(token: &TokenNode) -> Self {
            let node = Rc::new(RefCell::new(CoreAtomicExpressionNode {
                kind: AtomicExpressionKind::INTEGER(token.clone()),
                parent: None,
            }));
            token.set_parent(WeakASTNode::ATOMIC_EXPRESSION(WeakAtomicExpressionNode(
                Rc::downgrade(&node),
            )));
            AtomicExpressionNode(node)
        }
        pub fn new_with_floating_point_number(token: &TokenNode) -> Self {
            let node = Rc::new(RefCell::new(CoreAtomicExpressionNode {
                kind: AtomicExpressionKind::FLOATING_POINT_NUMBER(token.clone()),
                parent: None,
            }));
            token.set_parent(WeakASTNode::ATOMIC_EXPRESSION(WeakAtomicExpressionNode(
                Rc::downgrade(&node),
            )));
            AtomicExpressionNode(node)
        }
        pub fn new_with_literal(token: &TokenNode) -> Self {
            let node = Rc::new(RefCell::new(CoreAtomicExpressionNode {
                kind: AtomicExpressionKind::LITERAL(token.clone()),
                parent: None,
            }));
            token.set_parent(WeakASTNode::ATOMIC_EXPRESSION(WeakAtomicExpressionNode(
                Rc::downgrade(&node),
            )));
            AtomicExpressionNode(node)
        }
        pub fn new_with_parenthesised_expr(
            expr: &ExpressionNode,
            lparen: &TokenNode,
            rparen: &TokenNode,
        ) -> Self {
            let node = Rc::new(RefCell::new(CoreAtomicExpressionNode {
                kind: AtomicExpressionKind::PARENTHESISED_EXPRESSION(
                    ParenthesisedExpressionNode::new(expr, lparen, rparen),
                ),
                parent: None,
            }));
            AtomicExpressionNode(node)
        }
        pub fn new_with_atom(atom: &AtomNode) -> Self {
            let node = Rc::new(RefCell::new(CoreAtomicExpressionNode {
                kind: AtomicExpressionKind::ATOM(atom.clone()),
                parent: None,
            }));
            atom.set_parent(WeakASTNode::ATOMIC_EXPRESSION(WeakAtomicExpressionNode(
                Rc::downgrade(&node),
            )));
            AtomicExpressionNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreAtomicExpressionNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreAtomicExpressionNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for AtomicExpressionNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    impl ErrornousNode for AtomicExpressionNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
            lookahead: usize,
        ) -> Self {
            AtomicExpressionNode(Rc::new(RefCell::new(CoreAtomicExpressionNode {
                kind: AtomicExpressionKind::MISSING_TOKENS(MissingTokenNode::new(
                    expected_symbols,
                    received_token,
                    lookahead,
                )),
                parent: None,
            })))
        }
    }
    pub struct CoreParenthesisedExpressionNode {
        lparen: TokenNode,
        rparen: TokenNode,
        expr: ExpressionNode,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreParenthesisedExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreParenthesisedExpressionNode",
                "lparen",
                &&self.lparen,
                "rparen",
                &&self.rparen,
                "expr",
                &&self.expr,
                "parent",
                &&self.parent,
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
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct ParenthesisedExpressionNode(Rc<RefCell<CoreParenthesisedExpressionNode>>);
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
            let node = Rc::new(RefCell::new(CoreParenthesisedExpressionNode {
                lparen: lparen.clone(),
                rparen: rparen.clone(),
                expr: expr.clone(),
                parent: None,
            }));
            expr.set_parent(WeakASTNode::PARENTHESISED_EXPRESSION(
                WeakParenthesisedExpressionNode(Rc::downgrade(&node)),
            ));
            lparen.set_parent(WeakASTNode::PARENTHESISED_EXPRESSION(
                WeakParenthesisedExpressionNode(Rc::downgrade(&node)),
            ));
            rparen.set_parent(WeakASTNode::PARENTHESISED_EXPRESSION(
                WeakParenthesisedExpressionNode(Rc::downgrade(&node)),
            ));
            ParenthesisedExpressionNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreParenthesisedExpressionNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreParenthesisedExpressionNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for ParenthesisedExpressionNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreUnaryExpressionNode {
        kind: UnaryExpressionKind,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreUnaryExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreUnaryExpressionNode",
                "kind",
                &&self.kind,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreUnaryExpressionNode {
        #[inline]
        fn clone(&self) -> CoreUnaryExpressionNode {
            CoreUnaryExpressionNode {
                kind: ::core::clone::Clone::clone(&self.kind),
                parent: ::core::clone::Clone::clone(&self.parent),
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
    pub enum UnaryExpressionKind {
        ATOMIC(AtomicExpressionNode),
        UNARY(OnlyUnaryExpressionNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for UnaryExpressionKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                UnaryExpressionKind::ATOMIC(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ATOMIC", &__self_0)
                }
                UnaryExpressionKind::UNARY(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "UNARY", &__self_0)
                }
                UnaryExpressionKind::MISSING_TOKENS(__self_0) => {
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
    impl ::core::clone::Clone for UnaryExpressionKind {
        #[inline]
        fn clone(&self) -> UnaryExpressionKind {
            match self {
                UnaryExpressionKind::ATOMIC(__self_0) => {
                    UnaryExpressionKind::ATOMIC(::core::clone::Clone::clone(__self_0))
                }
                UnaryExpressionKind::UNARY(__self_0) => {
                    UnaryExpressionKind::UNARY(::core::clone::Clone::clone(__self_0))
                }
                UnaryExpressionKind::MISSING_TOKENS(__self_0) => {
                    UnaryExpressionKind::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct UnaryExpressionNode(Rc<RefCell<CoreUnaryExpressionNode>>);
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
            let node = Rc::new(RefCell::new(CoreUnaryExpressionNode {
                kind: UnaryExpressionKind::ATOMIC(atomic_expr.clone()),
                parent: None,
            }));
            atomic_expr.set_parent(WeakASTNode::UNARY_EXPRESSION(WeakUnaryExpressionNode(
                Rc::downgrade(&node),
            )));
            UnaryExpressionNode(node)
        }
        pub fn new_with_unary(
            unary_expr: &UnaryExpressionNode,
            operator: &TokenNode,
            operator_kind: UnaryOperatorKind,
        ) -> Self {
            let node = Rc::new(RefCell::new(CoreUnaryExpressionNode {
                kind: UnaryExpressionKind::UNARY(OnlyUnaryExpressionNode::new(
                    operator,
                    unary_expr,
                    operator_kind,
                )),
                parent: None,
            }));
            UnaryExpressionNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreUnaryExpressionNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreUnaryExpressionNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for UnaryExpressionNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    impl ErrornousNode for UnaryExpressionNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
            lookahead: usize,
        ) -> Self {
            UnaryExpressionNode(Rc::new(RefCell::new(CoreUnaryExpressionNode {
                kind: UnaryExpressionKind::MISSING_TOKENS(MissingTokenNode::new(
                    expected_symbols,
                    received_token,
                    lookahead,
                )),
                parent: None,
            })))
        }
    }
    pub struct CoreOnlyUnaryExpressionNode {
        operator: TokenNode,
        unary_expr: UnaryExpressionNode,
        operator_kind: UnaryOperatorKind,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreOnlyUnaryExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreOnlyUnaryExpressionNode",
                "operator",
                &&self.operator,
                "unary_expr",
                &&self.unary_expr,
                "operator_kind",
                &&self.operator_kind,
                "parent",
                &&self.parent,
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
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct OnlyUnaryExpressionNode(Rc<RefCell<CoreOnlyUnaryExpressionNode>>);
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
            let node = Rc::new(RefCell::new(CoreOnlyUnaryExpressionNode {
                operator: operator.clone(),
                unary_expr: unary_expr.clone(),
                operator_kind,
                parent: None,
            }));
            operator.set_parent(WeakASTNode::ONLY_UNARY_EXPRESSION(
                WeakOnlyUnaryExpressionNode(Rc::downgrade(&node)),
            ));
            unary_expr.set_parent(WeakASTNode::ONLY_UNARY_EXPRESSION(
                WeakOnlyUnaryExpressionNode(Rc::downgrade(&node)),
            ));
            OnlyUnaryExpressionNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreOnlyUnaryExpressionNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreOnlyUnaryExpressionNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for OnlyUnaryExpressionNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreBinaryExpressionNode {
        operator_kind: BinaryOperatorKind,
        left_expr: ExpressionNode,
        right_expr: ExpressionNode,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreBinaryExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreBinaryExpressionNode",
                "operator_kind",
                &&self.operator_kind,
                "left_expr",
                &&self.left_expr,
                "right_expr",
                &&self.right_expr,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreBinaryExpressionNode {
        #[inline]
        fn clone(&self) -> CoreBinaryExpressionNode {
            CoreBinaryExpressionNode {
                operator_kind: ::core::clone::Clone::clone(&self.operator_kind),
                left_expr: ::core::clone::Clone::clone(&self.left_expr),
                right_expr: ::core::clone::Clone::clone(&self.right_expr),
                parent: ::core::clone::Clone::clone(&self.parent),
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
    pub struct BinaryExpressionNode(Rc<RefCell<CoreBinaryExpressionNode>>);
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
            operator: BinaryOperatorKind,
            left_expr: &ExpressionNode,
            right_expr: &ExpressionNode,
        ) -> Self {
            let node = Rc::new(RefCell::new(CoreBinaryExpressionNode {
                operator_kind: operator,
                left_expr: left_expr.clone(),
                right_expr: right_expr.clone(),
                parent: None,
            }));
            left_expr.set_parent(WeakASTNode::BINARY_EXPRESSION(WeakBinaryExpressionNode(
                Rc::downgrade(&node),
            )));
            right_expr.set_parent(WeakASTNode::BINARY_EXPRESSION(WeakBinaryExpressionNode(
                Rc::downgrade(&node),
            )));
            BinaryExpressionNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreBinaryExpressionNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreBinaryExpressionNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for BinaryExpressionNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreLogicalExpressionNode {
        operator_kind: BinaryOperatorKind,
        left_expr: ExpressionNode,
        right_expr: ExpressionNode,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreLogicalExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreLogicalExpressionNode",
                "operator_kind",
                &&self.operator_kind,
                "left_expr",
                &&self.left_expr,
                "right_expr",
                &&self.right_expr,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreLogicalExpressionNode {
        #[inline]
        fn clone(&self) -> CoreLogicalExpressionNode {
            CoreLogicalExpressionNode {
                operator_kind: ::core::clone::Clone::clone(&self.operator_kind),
                left_expr: ::core::clone::Clone::clone(&self.left_expr),
                right_expr: ::core::clone::Clone::clone(&self.right_expr),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct LogicalExpressionNode(Rc<RefCell<CoreLogicalExpressionNode>>);
    #[automatically_derived]
    impl ::core::fmt::Debug for LogicalExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_tuple_field1_finish(f, "LogicalExpressionNode", &&self.0)
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for LogicalExpressionNode {
        #[inline]
        fn clone(&self) -> LogicalExpressionNode {
            LogicalExpressionNode(::core::clone::Clone::clone(&self.0))
        }
    }
    impl LogicalExpressionNode {
        pub fn new(
            operator: BinaryOperatorKind,
            left_expr: &ExpressionNode,
            right_expr: &ExpressionNode,
        ) -> Self {
            let node = Rc::new(RefCell::new(CoreLogicalExpressionNode {
                operator_kind: operator,
                left_expr: left_expr.clone(),
                right_expr: right_expr.clone(),
                parent: None,
            }));
            left_expr.set_parent(WeakASTNode::LOGICAL_EXPRESSION(WeakLogicalExpressionNode(
                Rc::downgrade(&node),
            )));
            right_expr.set_parent(WeakASTNode::LOGICAL_EXPRESSION(WeakLogicalExpressionNode(
                Rc::downgrade(&node),
            )));
            LogicalExpressionNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreLogicalExpressionNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreLogicalExpressionNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for LogicalExpressionNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreParamsNode {
        kind: ParamsKind,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreParamsNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreParamsNode",
                "kind",
                &&self.kind,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreParamsNode {
        #[inline]
        fn clone(&self) -> CoreParamsNode {
            CoreParamsNode {
                kind: ::core::clone::Clone::clone(&self.kind),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub enum ParamsKind {
        OK(OkParamsNode),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for ParamsKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                ParamsKind::OK(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "OK", &__self_0)
                }
                ParamsKind::MISSING_TOKENS(__self_0) => {
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
    impl ::core::clone::Clone for ParamsKind {
        #[inline]
        fn clone(&self) -> ParamsKind {
            match self {
                ParamsKind::OK(__self_0) => ParamsKind::OK(::core::clone::Clone::clone(__self_0)),
                ParamsKind::MISSING_TOKENS(__self_0) => {
                    ParamsKind::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct ParamsNode(Rc<RefCell<CoreParamsNode>>);
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
            let node = Rc::new(RefCell::new(CoreParamsNode {
                kind: ParamsKind::OK(ok_params_node.clone()),
                parent: None,
            }));
            ok_params_node.set_parent(WeakASTNode::PARAMS(WeakParamsNode(Rc::downgrade(&node))));
            ParamsNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreParamsNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreParamsNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for ParamsNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    impl ErrornousNode for ParamsNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
            lookahead: usize,
        ) -> Self {
            ParamsNode(Rc::new(RefCell::new(CoreParamsNode {
                kind: ParamsKind::MISSING_TOKENS(MissingTokenNode::new(
                    expected_symbols,
                    received_token,
                    lookahead,
                )),
                parent: None,
            })))
        }
    }
    pub struct CoreOkParamsNode {
        comma: Option<TokenNode>,
        param: ExpressionNode,
        remaining_params: Option<ParamsNode>,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreOkParamsNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CoreOkParamsNode",
                "comma",
                &&self.comma,
                "param",
                &&self.param,
                "remaining_params",
                &&self.remaining_params,
                "parent",
                &&self.parent,
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
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct OkParamsNode(Rc<RefCell<CoreOkParamsNode>>);
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
            let node = Rc::new(RefCell::new(CoreOkParamsNode {
                comma: None,
                param: param.clone(),
                remaining_params: None,
                parent: None,
            }));
            param.set_parent(WeakASTNode::OK_PARAMS(WeakOkParamsNode(Rc::downgrade(
                &node,
            ))));
            OkParamsNode(node)
        }
        pub fn new_with_params(
            param: &ExpressionNode,
            remaining_params: &ParamsNode,
            comma: &TokenNode,
        ) -> Self {
            let node = Rc::new(RefCell::new(CoreOkParamsNode {
                comma: Some(comma.clone()),
                param: param.clone(),
                remaining_params: Some(remaining_params.clone()),
                parent: None,
            }));
            param.set_parent(WeakASTNode::OK_PARAMS(WeakOkParamsNode(Rc::downgrade(
                &node,
            ))));
            remaining_params.set_parent(WeakASTNode::OK_PARAMS(WeakOkParamsNode(Rc::downgrade(
                &node,
            ))));
            comma.set_parent(WeakASTNode::OK_PARAMS(WeakOkParamsNode(Rc::downgrade(
                &node,
            ))));
            OkParamsNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreOkParamsNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreOkParamsNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for OkParamsNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreCallExpressionNode {
        lparen: TokenNode,
        rparen: TokenNode,
        function_name: TokenNode,
        params: Option<ParamsNode>,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreCallExpressionNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field5_finish(
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
                "parent",
                &&self.parent,
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
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct CallExpressionNode(Rc<RefCell<CoreCallExpressionNode>>);
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
            let node = Rc::new(RefCell::new(CoreCallExpressionNode {
                lparen: lparen.clone(),
                rparen: rparen.clone(),
                function_name: function_name.clone(),
                params: match params {
                    Some(val) => Some(val.clone()),
                    None => None,
                },
                parent: None,
            }));
            function_name.set_parent(WeakASTNode::CALL_EXPRESSION(WeakCallExpressionNode(
                Rc::downgrade(&node),
            )));
            lparen.set_parent(WeakASTNode::CALL_EXPRESSION(WeakCallExpressionNode(
                Rc::downgrade(&node),
            )));
            rparen.set_parent(WeakASTNode::CALL_EXPRESSION(WeakCallExpressionNode(
                Rc::downgrade(&node),
            )));
            match params {
                Some(params) => {
                    params.set_parent(WeakASTNode::CALL_EXPRESSION(WeakCallExpressionNode(
                        Rc::downgrade(&node),
                    )));
                }
                None => {}
            };
            CallExpressionNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreCallExpressionNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreCallExpressionNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for CallExpressionNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreClassMethodCallNode {
        lparen: TokenNode,
        rparen: TokenNode,
        double_colon: TokenNode,
        class_name: TokenNode,
        class_method_name: TokenNode,
        params: Option<ParamsNode>,
        parent: Option<WeakASTNode>,
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
                "parent",
            ];
            let values: &[&dyn ::core::fmt::Debug] = &[
                &&self.lparen,
                &&self.rparen,
                &&self.double_colon,
                &&self.class_name,
                &&self.class_method_name,
                &&self.params,
                &&self.parent,
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
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct ClassMethodCallNode(Rc<RefCell<CoreClassMethodCallNode>>);
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
            let node = Rc::new(RefCell::new(CoreClassMethodCallNode {
                lparen: lparen.clone(),
                rparen: rparen.clone(),
                double_colon: double_colon.clone(),
                class_name: class_name.clone(),
                class_method_name: class_method_name.clone(),
                params: match params {
                    Some(val) => Some(val.clone()),
                    None => None,
                },
                parent: None,
            }));
            class_name.set_parent(WeakASTNode::CLASS_METHOD_CALL(WeakClassMethodCallNode(
                Rc::downgrade(&node),
            )));
            class_method_name.set_parent(WeakASTNode::CLASS_METHOD_CALL(WeakClassMethodCallNode(
                Rc::downgrade(&node),
            )));
            double_colon.set_parent(WeakASTNode::CLASS_METHOD_CALL(WeakClassMethodCallNode(
                Rc::downgrade(&node),
            )));
            lparen.set_parent(WeakASTNode::CLASS_METHOD_CALL(WeakClassMethodCallNode(
                Rc::downgrade(&node),
            )));
            rparen.set_parent(WeakASTNode::CLASS_METHOD_CALL(WeakClassMethodCallNode(
                Rc::downgrade(&node),
            )));
            match params {
                Some(params) => {
                    params.set_parent(WeakASTNode::CLASS_METHOD_CALL(WeakClassMethodCallNode(
                        Rc::downgrade(&node),
                    )));
                }
                None => {}
            };
            ClassMethodCallNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreClassMethodCallNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreClassMethodCallNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for ClassMethodCallNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreAtomNode {
        kind: AtomKind,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreAtomNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreAtomNode",
                "kind",
                &&self.kind,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreAtomNode {
        #[inline]
        fn clone(&self) -> CoreAtomNode {
            CoreAtomNode {
                kind: ::core::clone::Clone::clone(&self.kind),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub enum AtomKind {
        ATOM_START(AtomStartNode),
        CALL(CallNode),
        PROPERTRY_ACCESS(PropertyAccessNode),
        METHOD_ACCESS(MethodAccessNode),
        INDEX_ACCESS(IndexAccessNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for AtomKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                AtomKind::ATOM_START(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "ATOM_START", &__self_0)
                }
                AtomKind::CALL(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "CALL", &__self_0)
                }
                AtomKind::PROPERTRY_ACCESS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "PROPERTRY_ACCESS",
                        &__self_0,
                    )
                }
                AtomKind::METHOD_ACCESS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "METHOD_ACCESS", &__self_0)
                }
                AtomKind::INDEX_ACCESS(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "INDEX_ACCESS", &__self_0)
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for AtomKind {
        #[inline]
        fn clone(&self) -> AtomKind {
            match self {
                AtomKind::ATOM_START(__self_0) => {
                    AtomKind::ATOM_START(::core::clone::Clone::clone(__self_0))
                }
                AtomKind::CALL(__self_0) => AtomKind::CALL(::core::clone::Clone::clone(__self_0)),
                AtomKind::PROPERTRY_ACCESS(__self_0) => {
                    AtomKind::PROPERTRY_ACCESS(::core::clone::Clone::clone(__self_0))
                }
                AtomKind::METHOD_ACCESS(__self_0) => {
                    AtomKind::METHOD_ACCESS(::core::clone::Clone::clone(__self_0))
                }
                AtomKind::INDEX_ACCESS(__self_0) => {
                    AtomKind::INDEX_ACCESS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct AtomNode(Rc<RefCell<CoreAtomNode>>);
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
            let node = Rc::new(RefCell::new(CoreAtomNode {
                kind: AtomKind::ATOM_START(atom_start.clone()),
                parent: None,
            }));
            atom_start.set_parent(WeakASTNode::ATOM(WeakAtomNode(Rc::downgrade(&node))));
            AtomNode(node)
        }
        pub fn new_with_call(
            atom: &AtomNode,
            params: Option<&ParamsNode>,
            lparen: &TokenNode,
            rparen: &TokenNode,
        ) -> Self {
            AtomNode(Rc::new(RefCell::new(CoreAtomNode {
                kind: AtomKind::CALL(CallNode::new(atom, params, lparen, rparen)),
                parent: None,
            })))
        }
        pub fn new_with_propertry_access(
            atom: &AtomNode,
            propertry: &TokenNode,
            dot: &TokenNode,
        ) -> Self {
            let node = Rc::new(RefCell::new(CoreAtomNode {
                kind: AtomKind::PROPERTRY_ACCESS(PropertyAccessNode::new(atom, propertry, dot)),
                parent: None,
            }));
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
            let node = Rc::new(RefCell::new(CoreAtomNode {
                kind: AtomKind::METHOD_ACCESS(MethodAccessNode::new(
                    atom,
                    method_name,
                    params,
                    lparen,
                    rparen,
                    dot,
                )),
                parent: None,
            }));
            AtomNode(node)
        }
        pub fn new_with_index_access(
            atom: &AtomNode,
            index: &ExpressionNode,
            lsquare: &TokenNode,
            rsquare: &TokenNode,
        ) -> Self {
            let node = Rc::new(RefCell::new(CoreAtomNode {
                kind: AtomKind::INDEX_ACCESS(IndexAccessNode::new(atom, index, lsquare, rsquare)),
                parent: None,
            }));
            AtomNode(node)
        }
        pub fn is_valid_l_value(&self) -> bool {
            match &self.core_ref().kind {
                AtomKind::ATOM_START(atom_start_node) => atom_start_node.is_valid_l_value(),
                AtomKind::CALL(_) => false,
                AtomKind::METHOD_ACCESS(_) => false,
                AtomKind::INDEX_ACCESS(atom_index_access_node) => {
                    let atom = &atom_index_access_node.core_ref().atom;
                    return atom.is_valid_l_value();
                }
                AtomKind::PROPERTRY_ACCESS(atom_property_access_node) => {
                    let atom = &atom_property_access_node.core_ref().atom;
                    return atom.is_valid_l_value();
                }
            }
        }
        pub fn core_ref(&self) -> Ref<CoreAtomNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreAtomNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for AtomNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreAtomStartNode {
        kind: AtomStartKind,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreAtomStartNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreAtomStartNode",
                "kind",
                &&self.kind,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreAtomStartNode {
        #[inline]
        fn clone(&self) -> CoreAtomStartNode {
            CoreAtomStartNode {
                kind: ::core::clone::Clone::clone(&self.kind),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub enum AtomStartKind {
        IDENTIFIER(TokenNode),
        FUNCTION_CALL(CallExpressionNode),
        CLASS_METHOD_CALL(ClassMethodCallNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for AtomStartKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                AtomStartKind::IDENTIFIER(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "IDENTIFIER", &__self_0)
                }
                AtomStartKind::FUNCTION_CALL(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "FUNCTION_CALL", &__self_0)
                }
                AtomStartKind::CLASS_METHOD_CALL(__self_0) => {
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
    impl ::core::clone::Clone for AtomStartKind {
        #[inline]
        fn clone(&self) -> AtomStartKind {
            match self {
                AtomStartKind::IDENTIFIER(__self_0) => {
                    AtomStartKind::IDENTIFIER(::core::clone::Clone::clone(__self_0))
                }
                AtomStartKind::FUNCTION_CALL(__self_0) => {
                    AtomStartKind::FUNCTION_CALL(::core::clone::Clone::clone(__self_0))
                }
                AtomStartKind::CLASS_METHOD_CALL(__self_0) => {
                    AtomStartKind::CLASS_METHOD_CALL(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct AtomStartNode(Rc<RefCell<CoreAtomStartNode>>);
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
            let node = Rc::new(RefCell::new(CoreAtomStartNode {
                kind: AtomStartKind::IDENTIFIER(token.clone()),
                parent: None,
            }));
            token.set_parent(WeakASTNode::ATOM_START(WeakAtomStartNode(Rc::downgrade(
                &node,
            ))));
            AtomStartNode(node)
        }
        pub fn new_with_function_call(call_expr: &CallExpressionNode) -> Self {
            let node = Rc::new(RefCell::new(CoreAtomStartNode {
                kind: AtomStartKind::FUNCTION_CALL(call_expr.clone()),
                parent: None,
            }));
            call_expr.set_parent(WeakASTNode::ATOM_START(WeakAtomStartNode(Rc::downgrade(
                &node,
            ))));
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
            let node = Rc::new(RefCell::new(CoreAtomStartNode {
                kind: AtomStartKind::CLASS_METHOD_CALL(ClassMethodCallNode::new(
                    class_name,
                    class_method_name,
                    params,
                    double_colon,
                    lparen,
                    rparen,
                )),
                parent: None,
            }));
            AtomStartNode(node)
        }
        pub fn is_valid_l_value(&self) -> bool {
            match &self.core_ref().kind {
                AtomStartKind::IDENTIFIER(_) => true,
                _ => false,
            }
        }
        pub fn core_ref(&self) -> Ref<CoreAtomStartNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreAtomStartNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for AtomStartNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreCallNode {
        atom: AtomNode,
        lparen: TokenNode,
        rparen: TokenNode,
        params: Option<ParamsNode>,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreCallNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field5_finish(
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
                "parent",
                &&self.parent,
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
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct CallNode(Rc<RefCell<CoreCallNode>>);
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
            let node = Rc::new(RefCell::new(CoreCallNode {
                atom: atom.clone(),
                lparen: lparen.clone(),
                rparen: rparen.clone(),
                params: match params {
                    Some(val) => Some(val.clone()),
                    None => None,
                },
                parent: None,
            }));
            atom.set_parent(WeakASTNode::CALL(WeakCallNode(Rc::downgrade(&node))));
            lparen.set_parent(WeakASTNode::CALL(WeakCallNode(Rc::downgrade(&node))));
            rparen.set_parent(WeakASTNode::CALL(WeakCallNode(Rc::downgrade(&node))));
            match params {
                Some(params) => {
                    params.set_parent(WeakASTNode::CALL(WeakCallNode(Rc::downgrade(&node))));
                }
                None => {}
            };
            CallNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreCallNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreCallNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for CallNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CorePropertyAccessNode {
        dot: TokenNode,
        atom: AtomNode,
        propertry: TokenNode,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CorePropertyAccessNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field4_finish(
                f,
                "CorePropertyAccessNode",
                "dot",
                &&self.dot,
                "atom",
                &&self.atom,
                "propertry",
                &&self.propertry,
                "parent",
                &&self.parent,
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
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct PropertyAccessNode(Rc<RefCell<CorePropertyAccessNode>>);
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
            let node = Rc::new(RefCell::new(CorePropertyAccessNode {
                dot: dot.clone(),
                atom: atom.clone(),
                propertry: propertry.clone(),
                parent: None,
            }));
            atom.set_parent(WeakASTNode::PROPERTY_ACCESS(WeakPropertyAccessNode(
                Rc::downgrade(&node),
            )));
            propertry.set_parent(WeakASTNode::PROPERTY_ACCESS(WeakPropertyAccessNode(
                Rc::downgrade(&node),
            )));
            dot.set_parent(WeakASTNode::PROPERTY_ACCESS(WeakPropertyAccessNode(
                Rc::downgrade(&node),
            )));
            PropertyAccessNode(node)
        }
        pub fn core_ref(&self) -> Ref<CorePropertyAccessNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CorePropertyAccessNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for PropertyAccessNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreMethodAccessNode {
        lparen: TokenNode,
        rparen: TokenNode,
        dot: TokenNode,
        atom: AtomNode,
        method_name: TokenNode,
        params: Option<ParamsNode>,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreMethodAccessNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            let names: &'static _ = &[
                "lparen",
                "rparen",
                "dot",
                "atom",
                "method_name",
                "params",
                "parent",
            ];
            let values: &[&dyn ::core::fmt::Debug] = &[
                &&self.lparen,
                &&self.rparen,
                &&self.dot,
                &&self.atom,
                &&self.method_name,
                &&self.params,
                &&self.parent,
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
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct MethodAccessNode(Rc<RefCell<CoreMethodAccessNode>>);
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
            let node = Rc::new(RefCell::new(CoreMethodAccessNode {
                lparen: lparen.clone(),
                rparen: rparen.clone(),
                dot: dot.clone(),
                atom: atom.clone(),
                method_name: method_name.clone(),
                params: match params {
                    Some(val) => Some(val.clone()),
                    None => None,
                },
                parent: None,
            }));
            atom.set_parent(WeakASTNode::METHOD_ACCESS(WeakMethodAccessNode(
                Rc::downgrade(&node),
            )));
            method_name.set_parent(WeakASTNode::METHOD_ACCESS(WeakMethodAccessNode(
                Rc::downgrade(&node),
            )));
            lparen.set_parent(WeakASTNode::METHOD_ACCESS(WeakMethodAccessNode(
                Rc::downgrade(&node),
            )));
            rparen.set_parent(WeakASTNode::METHOD_ACCESS(WeakMethodAccessNode(
                Rc::downgrade(&node),
            )));
            dot.set_parent(WeakASTNode::METHOD_ACCESS(WeakMethodAccessNode(
                Rc::downgrade(&node),
            )));
            match params {
                Some(params) => {
                    params.set_parent(WeakASTNode::METHOD_ACCESS(WeakMethodAccessNode(
                        Rc::downgrade(&node),
                    )));
                }
                None => {}
            };
            MethodAccessNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreMethodAccessNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreMethodAccessNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for MethodAccessNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreIndexAccessNode {
        lsquare: TokenNode,
        rsquare: TokenNode,
        atom: AtomNode,
        index: ExpressionNode,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreIndexAccessNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field5_finish(
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
                "parent",
                &&self.parent,
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
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub struct IndexAccessNode(Rc<RefCell<CoreIndexAccessNode>>);
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
            let node = Rc::new(RefCell::new(CoreIndexAccessNode {
                lsquare: lsquare.clone(),
                rsquare: rsquare.clone(),
                atom: atom.clone(),
                index: index.clone(),
                parent: None,
            }));
            atom.set_parent(WeakASTNode::INDEX_ACCESS(WeakIndexAccessNode(
                Rc::downgrade(&node),
            )));
            index.set_parent(WeakASTNode::INDEX_ACCESS(WeakIndexAccessNode(
                Rc::downgrade(&node),
            )));
            lsquare.set_parent(WeakASTNode::INDEX_ACCESS(WeakIndexAccessNode(
                Rc::downgrade(&node),
            )));
            rsquare.set_parent(WeakASTNode::INDEX_ACCESS(WeakIndexAccessNode(
                Rc::downgrade(&node),
            )));
            IndexAccessNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreIndexAccessNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreIndexAccessNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for IndexAccessNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    pub struct CoreRAssignmentNode {
        kind: RAssignmentKind,
        parent: Option<WeakASTNode>,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for CoreRAssignmentNode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::debug_struct_field2_finish(
                f,
                "CoreRAssignmentNode",
                "kind",
                &&self.kind,
                "parent",
                &&self.parent,
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for CoreRAssignmentNode {
        #[inline]
        fn clone(&self) -> CoreRAssignmentNode {
            CoreRAssignmentNode {
                kind: ::core::clone::Clone::clone(&self.kind),
                parent: ::core::clone::Clone::clone(&self.parent),
            }
        }
    }
    pub enum RAssignmentKind {
        LAMBDA(FunctionDeclarationNode),
        EXPRESSION((ExpressionNode, TokenNode)),
        MISSING_TOKENS(MissingTokenNode),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for RAssignmentKind {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                RAssignmentKind::LAMBDA(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "LAMBDA", &__self_0)
                }
                RAssignmentKind::EXPRESSION(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "EXPRESSION", &__self_0)
                }
                RAssignmentKind::MISSING_TOKENS(__self_0) => {
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
    impl ::core::clone::Clone for RAssignmentKind {
        #[inline]
        fn clone(&self) -> RAssignmentKind {
            match self {
                RAssignmentKind::LAMBDA(__self_0) => {
                    RAssignmentKind::LAMBDA(::core::clone::Clone::clone(__self_0))
                }
                RAssignmentKind::EXPRESSION(__self_0) => {
                    RAssignmentKind::EXPRESSION(::core::clone::Clone::clone(__self_0))
                }
                RAssignmentKind::MISSING_TOKENS(__self_0) => {
                    RAssignmentKind::MISSING_TOKENS(::core::clone::Clone::clone(__self_0))
                }
            }
        }
    }
    pub struct RAssignmentNode(Rc<RefCell<CoreRAssignmentNode>>);
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
            let node = Rc::new(RefCell::new(CoreRAssignmentNode {
                kind: RAssignmentKind::LAMBDA(lambda_decl.clone()),
                parent: None,
            }));
            lambda_decl.set_parent(WeakASTNode::R_ASSIGNMENT(WeakRAssignmentNode(
                Rc::downgrade(&node),
            )));
            RAssignmentNode(node)
        }
        pub fn new_with_expr(expr: &ExpressionNode, newline: &TokenNode) -> Self {
            let node = Rc::new(RefCell::new(CoreRAssignmentNode {
                kind: RAssignmentKind::EXPRESSION((expr.clone(), newline.clone())),
                parent: None,
            }));
            expr.set_parent(WeakASTNode::R_ASSIGNMENT(WeakRAssignmentNode(
                Rc::downgrade(&node),
            )));
            newline.set_parent(WeakASTNode::R_ASSIGNMENT(WeakRAssignmentNode(
                Rc::downgrade(&node),
            )));
            RAssignmentNode(node)
        }
        pub fn core_ref(&self) -> Ref<CoreRAssignmentNode> {
            self.0.as_ref().borrow()
        }
        pub fn core_ref_mut(&self) -> RefMut<CoreRAssignmentNode> {
            self.0.as_ref().borrow_mut()
        }
    }
    impl Node for RAssignmentNode {
        fn set_parent(&self, parent_node: WeakASTNode) {
            self.core_ref_mut().parent = Some(parent_node);
        }
    }
    impl ErrornousNode for RAssignmentNode {
        fn new_with_missing_tokens(
            expected_symbols: &Rc<Vec<&'static str>>,
            received_token: &Token,
            lookahead: usize,
        ) -> Self {
            RAssignmentNode(Rc::new(RefCell::new(CoreRAssignmentNode {
                kind: RAssignmentKind::MISSING_TOKENS(MissingTokenNode::new(
                    expected_symbols,
                    received_token,
                    lookahead,
                )),
                parent: None,
            })))
        }
    }
}
