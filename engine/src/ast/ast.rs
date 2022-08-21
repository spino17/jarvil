// This module contains green tree nodes. Green Tree is top to down immutable typed structure with no parent information.
// See the following for more information on green and red tree, immutability and cheap mutations.
// 1. `https://github.com/apple/swift/tree/5e2c815edfd758f9b1309ce07bfc01c4bc20ec23/lib/Syntax`
// 2. `https://github.com/rust-analyzer/rowan`

#[macro_use]
use jarvil_macros::Nodify;
#[macro_use]
use jarvil_macros::Node;

use crate::lexer::token::BinaryOperatorKind;
use crate::scope::core::IdentifierKind;
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

#[derive(Debug, Clone, Nodify)]
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
    RETURN(ReturnStatementNode),
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

#[derive(Debug, Clone)]
pub struct CoreBlockNode {
    pub newline: TokenNode,
    pub stmts: Vec<StatemenIndentWrapperNode>,
    pub scope: Option<Namespace>,
}

#[derive(Debug, Clone)]
pub struct BlockNode(pub Rc<RefCell<CoreBlockNode>>);
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
        impl_range!(
            self.0.as_ref().borrow().newline,
            self.0.as_ref().borrow().stmts[stmts_len - 1]
        )
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().borrow().newline.start_line_number()
    }
}

#[derive(Debug, Clone, Node)]
pub enum CoreStatemenIndentWrapperNode {
    CORRECTLY_INDENTED(StatementNode),
    INCORRECTLY_INDENTED(IncorrectlyIndentedStatementNode),
    LEADING_SKIPPED_TOKENS(SkippedTokensNode), // skipped tokens leading to the next stmt in block
    TRAILING_SKIPPED_TOKENS(SkippedTokensNode), // skipped tokens trailing to the previous stmt in block
    EXTRA_NEWLINES(SkippedTokensNode),
}

#[derive(Debug, Clone)]
pub struct StatemenIndentWrapperNode(Rc<CoreStatemenIndentWrapperNode>);
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

    impl_core_ref!(CoreStatemenIndentWrapperNode);
}

#[derive(Debug, Clone)]
pub struct CoreSkippedTokensNode {
    pub skipped_tokens: Vec<SkippedTokenNode>,
}

#[derive(Debug, Clone)]
pub struct SkippedTokensNode(Rc<CoreSkippedTokensNode>);
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

    impl_core_ref!(CoreSkippedTokensNode);
}
impl Node for SkippedTokensNode {
    fn range(&self) -> TextRange {
        let core_skipped_tokens = &self.0.as_ref().skipped_tokens;
        impl_range!(
            core_skipped_tokens[0],
            core_skipped_tokens[core_skipped_tokens.len() - 1]
        )
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().skipped_tokens[0].start_line_number()
    }
}

#[derive(Debug, Clone, Node)]
pub enum CoreStatementNode {
    EXPRESSION(ExpressionStatementNode),
    ASSIGNMENT(AssignmentNode),
    VARIABLE_DECLARATION(VariableDeclarationNode),
    RETURN(ReturnStatementNode),
    FUNCTION_DECLARATION(FunctionDeclarationNode),
    TYPE_DECLARATION(TypeDeclarationNode),
    STRUCT_STATEMENT(StructStatementNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct StatementNode(Rc<CoreStatementNode>);
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

    pub fn new_with_return_statement(return_keyword: &TokenNode, expr: &ExpressionNode, newline: &TokenNode) -> Self {
        let node = Rc::new(CoreStatementNode::RETURN(ReturnStatementNode::new(return_keyword, expr, newline)));
        StatementNode(node)
    }

    impl_core_ref!(CoreStatementNode);
}
default_errornous_node_impl!(StatementNode, CoreStatementNode);

#[derive(Debug, Clone)]
pub struct CoreIncorrectlyIndentedStatementNode {
    pub stmt: StatementNode,
    pub expected_indent: i64,
    pub received_indent: i64,
}

#[derive(Debug, Clone)]
pub struct IncorrectlyIndentedStatementNode(Rc<CoreIncorrectlyIndentedStatementNode>);
impl IncorrectlyIndentedStatementNode {
    fn new(stmt: &StatementNode, expected_indent: i64, received_indent: i64) -> Self {
        let node = Rc::new(CoreIncorrectlyIndentedStatementNode {
            stmt: stmt.clone(),
            expected_indent,
            received_indent,
        });
        IncorrectlyIndentedStatementNode(node)
    }

    impl_core_ref!(CoreIncorrectlyIndentedStatementNode);
}
impl Node for IncorrectlyIndentedStatementNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().stmt, self.0.as_ref().stmt)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().stmt.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreExpressionStatementNode {
    pub expr: ExpressionNode,
    pub newline: TokenNode,
}

#[derive(Debug, Clone)]
pub struct ExpressionStatementNode(Rc<CoreExpressionStatementNode>);
impl ExpressionStatementNode {
    fn new(expr: &ExpressionNode, newline: &TokenNode) -> Self {
        let node = Rc::new(CoreExpressionStatementNode {
            expr: expr.clone(),
            newline: newline.clone(),
        });
        ExpressionStatementNode(node)
    }

    impl_core_ref!(CoreExpressionStatementNode);
}
impl Node for ExpressionStatementNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().expr, self.0.as_ref().newline)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().expr.start_line_number()
    }
}

#[derive(Debug, Clone, Node)]
pub enum CoreAssignmentNode {
    OK(OkAssignmentNode),
    INVALID_L_VALUE(InvalidLValueNode),
}

#[derive(Debug, Clone)]
pub struct AssignmentNode(Rc<CoreAssignmentNode>);
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

    impl_core_ref!(CoreAssignmentNode);
}

#[derive(Debug, Clone)]
pub struct CoreOkAssignmentNode {
    pub equal: TokenNode,
    pub l_atom: AtomNode,
    pub r_assign: RAssignmentNode,
}

#[derive(Debug, Clone)]
pub struct OkAssignmentNode(Rc<CoreOkAssignmentNode>);
impl OkAssignmentNode {
    pub fn new(l_atom: &AtomNode, r_assign: &RAssignmentNode, equal: &TokenNode) -> Self {
        let node = Rc::new(CoreOkAssignmentNode {
            equal: equal.clone(),
            l_atom: l_atom.clone(),
            r_assign: r_assign.clone(),
        });
        OkAssignmentNode(node)
    }

    impl_core_ref!(CoreOkAssignmentNode);
}
impl Node for OkAssignmentNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().l_atom, self.0.as_ref().r_assign)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().l_atom.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreInvalidLValueNode {
    pub l_expr: ExpressionNode,
    pub equal: TokenNode,
    pub r_assign: RAssignmentNode,
}

#[derive(Debug, Clone)]
pub struct InvalidLValueNode(Rc<CoreInvalidLValueNode>);
impl InvalidLValueNode {
    pub fn new(l_expr: &ExpressionNode, r_assign: &RAssignmentNode, equal: &TokenNode) -> Self {
        let node = Rc::new(CoreInvalidLValueNode {
            l_expr: l_expr.clone(),
            equal: equal.clone(),
            r_assign: r_assign.clone(),
        });
        InvalidLValueNode(node)
    }

    impl_core_ref!(CoreInvalidLValueNode);
}
impl Node for InvalidLValueNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().l_expr, self.0.as_ref().r_assign)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().l_expr.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreStructStatementNode {
    pub newline: TokenNode,
    pub name_type_spec: NameTypeSpecNode,
}

#[derive(Debug, Clone)]
pub struct StructStatementNode(Rc<CoreStructStatementNode>);
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

    impl_core_ref!(CoreStructStatementNode);
}
impl Node for StructStatementNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().name_type_spec, self.0.as_ref().newline)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().name_type_spec.start_line_number()
    }
}

#[derive(Debug, Clone, Node)]
pub enum CoreTypeDeclarationNode {
    STRUCT(StructDeclarationNode),
    LAMBDA(LambdaDeclarationNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct TypeDeclarationNode(Rc<CoreTypeDeclarationNode>);
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

    impl_core_ref!(CoreTypeDeclarationNode);
}
default_errornous_node_impl!(TypeDeclarationNode, CoreTypeDeclarationNode);

#[derive(Debug, Clone)]
pub struct CoreStructDeclarationNode {
    pub type_keyword: TokenNode,
    pub colon: TokenNode,
    pub name: TokenNode,
    pub block: BlockNode,
}

#[derive(Debug, Clone)]
pub struct StructDeclarationNode(Rc<CoreStructDeclarationNode>);
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

    impl_core_ref!(CoreStructDeclarationNode);
}
impl Node for StructDeclarationNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().type_keyword, self.0.as_ref().block)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().type_keyword.start_line_number()
    }
}

#[derive(Debug, Clone, Node)]
pub enum CoreLambdaDeclarationNode {
    OK(OkLambdaDeclarationNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct LambdaDeclarationNode(Rc<CoreLambdaDeclarationNode>);
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

    impl_core_ref!(CoreLambdaDeclarationNode);
}
default_errornous_node_impl!(LambdaDeclarationNode, CoreLambdaDeclarationNode);

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct OkLambdaDeclarationNode(Rc<CoreOkLambdaDeclarationNode>);
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
            right_arrow: extract_from_option!(right_arrow),
            newline: newline.clone(),
            type_keyword: type_keyword.clone(),
            colon: colon.clone(),
            name: name.clone(),
            args: extract_from_option!(args),
            return_type: extract_from_option!(return_type),
        });
        OkLambdaDeclarationNode(node)
    }

    impl_core_ref!(CoreOkLambdaDeclarationNode);
}
impl Node for OkLambdaDeclarationNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().type_keyword, self.0.as_ref().newline)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().type_keyword.start_line_number()
    }
}

#[derive(Debug, Clone, Node)]
pub enum CoreFunctionDeclarationNode {
    OK(OkFunctionDeclarationNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct FunctionDeclarationNode(Rc<CoreFunctionDeclarationNode>);
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

    impl_core_ref!(CoreFunctionDeclarationNode);
}
default_errornous_node_impl!(FunctionDeclarationNode, CoreFunctionDeclarationNode);

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum FuncKeywordKind {
    DEF(TokenNode),
    FUNC(TokenNode),
}

#[derive(Debug, Clone)]
pub struct OkFunctionDeclarationNode(Rc<CoreOkFunctionDeclarationNode>);
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
            right_arrow: extract_from_option!(right_arrow),
            colon: colon.clone(),
            name: extract_from_option!(name),
            args: extract_from_option!(args),
            return_type: extract_from_option!(return_type),
            block: block.clone(),
        });
        OkFunctionDeclarationNode(node)
    }

    impl_core_ref!(CoreOkFunctionDeclarationNode);
}
impl Node for OkFunctionDeclarationNode {
    fn range(&self) -> TextRange {
        match &self.0.as_ref().func_keyword {
            FuncKeywordKind::DEF(token) => impl_range!(token, self.0.as_ref().block),
            FuncKeywordKind::FUNC(token) => impl_range!(token, self.0.as_ref().block),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.0.as_ref().func_keyword {
            FuncKeywordKind::DEF(token) => token.start_line_number(),
            FuncKeywordKind::FUNC(token) => token.start_line_number(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CoreVariableDeclarationNode {
    pub let_keyword: TokenNode,
    pub equal: TokenNode,
    pub name: TokenNode,
    pub r_assign: RAssignmentNode,
}

#[derive(Debug, Clone)]
pub struct VariableDeclarationNode(Rc<CoreVariableDeclarationNode>);
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

    impl_core_ref!(CoreVariableDeclarationNode);
}
impl Node for VariableDeclarationNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().let_keyword, self.0.as_ref().r_assign)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().let_keyword.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreReturnStatementNode {
    pub return_keyword: TokenNode,
    pub expr: ExpressionNode,
    pub newline: TokenNode,
}

#[derive(Debug, Clone)]
pub struct ReturnStatementNode(Rc<CoreReturnStatementNode>);
impl ReturnStatementNode {
    fn new(return_keyword: &TokenNode, expr: &ExpressionNode, newline: &TokenNode) -> Self {
        let node = Rc::new(CoreReturnStatementNode{
            return_keyword: return_keyword.clone(),
            expr: expr.clone(),
            newline: newline.clone(),
        });
        ReturnStatementNode(node)
    }

    impl_core_ref!(CoreReturnStatementNode);
}
impl Node for ReturnStatementNode {
    fn range(&self) -> TextRange {
        impl_range!(self.core_ref().return_keyword, self.core_ref().newline)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().return_keyword.start_line_number()
    }
}

#[derive(Debug, Clone, Node)]
pub enum CoreNameTypeSpecsNode {
    OK(OkNameTypeSpecsNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct NameTypeSpecsNode(Rc<CoreNameTypeSpecsNode>);
impl NameTypeSpecsNode {
    pub fn new(ok_name_type_specs: &OkNameTypeSpecsNode) -> Self {
        let node = Rc::new(CoreNameTypeSpecsNode::OK(ok_name_type_specs.clone()));
        NameTypeSpecsNode(node)
    }

    impl_core_ref!(CoreNameTypeSpecsNode);
}
default_errornous_node_impl!(NameTypeSpecsNode, CoreNameTypeSpecsNode);

#[derive(Debug, Clone)]
pub struct CoreOkNameTypeSpecsNode {
    pub comma: Option<TokenNode>,
    pub arg: NameTypeSpecNode,
    pub remaining_args: Option<NameTypeSpecsNode>,
}

#[derive(Debug, Clone)]
pub struct OkNameTypeSpecsNode(Rc<CoreOkNameTypeSpecsNode>);
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

    impl_core_ref!(CoreOkNameTypeSpecsNode);
}
impl Node for OkNameTypeSpecsNode {
    fn range(&self) -> TextRange {
        match &self.0.as_ref().remaining_args {
            Some(remaining_args) => impl_range!(self.0.as_ref().arg, remaining_args),
            None => impl_range!(self.0.as_ref().arg, self.0.as_ref().arg),
        }
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().arg.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreNameTypeSpecNode {
    pub colon: TokenNode,
    pub name: TokenNode,
    pub data_type: TypeExpressionNode,
}

#[derive(Debug, Clone)]
pub struct NameTypeSpecNode(Rc<CoreNameTypeSpecNode>);
impl NameTypeSpecNode {
    pub fn new(param_name: &TokenNode, param_type: &TypeExpressionNode, colon: &TokenNode) -> Self {
        let node = Rc::new(CoreNameTypeSpecNode {
            colon: colon.clone(),
            name: param_name.clone(),
            data_type: param_type.clone(),
        });
        NameTypeSpecNode(node)
    }

    impl_core_ref!(CoreNameTypeSpecNode);
}
impl Node for NameTypeSpecNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().name, self.0.as_ref().data_type)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().name.start_line_number()
    }
}

#[derive(Debug, Clone, Node)]
pub enum CoreTypeExpressionNode {
    ATOMIC(AtomicTypeNode),
    USER_DEFINED(UserDefinedTypeNode),
    ARRAY(ArrayTypeNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct TypeExpressionNode(Rc<CoreTypeExpressionNode>);
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

    impl_core_ref!(CoreTypeExpressionNode);
}
default_errornous_node_impl!(TypeExpressionNode, CoreTypeExpressionNode);

#[derive(Debug, Clone)]
pub struct CoreAtomicTypeNode {
    pub kind: TokenNode,
}

#[derive(Debug, Clone)]
pub struct AtomicTypeNode(Rc<CoreAtomicTypeNode>);
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

    impl_core_ref!(CoreAtomicTypeNode);
}
impl Node for AtomicTypeNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().kind, self.0.as_ref().kind)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().kind.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreArrayTypeNode {
    pub lsquare: TokenNode,
    pub rsquare: TokenNode,
    pub semicolon: TokenNode,
    pub sub_type: TypeExpressionNode,
    pub size: TokenNode,
}

#[derive(Debug, Clone)]
pub struct ArrayTypeNode(Rc<CoreArrayTypeNode>);
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

    impl_core_ref!(CoreArrayTypeNode);
}
impl Node for ArrayTypeNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().lsquare, self.0.as_ref().rsquare)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().lsquare.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreUserDefinedTypeNode {
    pub name: TokenNode,
}

#[derive(Debug, Clone)]
pub struct UserDefinedTypeNode(Rc<CoreUserDefinedTypeNode>);
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

    impl_core_ref!(CoreUserDefinedTypeNode);
}
impl Node for UserDefinedTypeNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().name, self.0.as_ref().name)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().name.start_line_number()
    }
}

#[derive(Debug, Clone, Node)]
pub enum CoreTokenNode {
    OK(OkTokenNode),
    MISSING_TOKENS(MissingTokenNode),
    SKIPPED(SkippedTokenNode),
}

#[derive(Debug, Clone)]
pub struct TokenNode(Rc<CoreTokenNode>);
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

    impl_core_ref!(CoreTokenNode);
}
default_errornous_node_impl!(TokenNode, CoreTokenNode);

#[derive(Debug, Clone)]
pub struct CoreOkTokenNode {
    pub token: Token,
    pub kind: OkTokenKind,
}

#[derive(Debug, Clone)]
pub enum OkTokenKind {
    IDENTIFIER(Option<IdentifierKind>), // This is set when the identifier is resolved
    NON_IDENTIFIER,
}

#[derive(Debug, Clone)]
pub struct OkTokenNode(Rc<CoreOkTokenNode>);
impl OkTokenNode {
    pub fn new(token: &Token, kind: OkTokenKind) -> Self {
        OkTokenNode(Rc::new(CoreOkTokenNode {
            token: token.clone(),
            kind,
        }))
    }

    pub fn is_binary_operator(&self) -> Option<BinaryOperatorKind> {
        self.core_ref().token.is_binary_operator()
    }

    pub fn token_value(&self, code: &Code) -> String {
        self.0.as_ref().token.token_value(code)
    }

    pub fn is_identifier(&self) -> bool {
        self.core_ref().token.is_identifier()
    }

    impl_core_ref!(CoreOkTokenNode);
}
impl Node for OkTokenNode {
    fn range(&self) -> TextRange {
        self.0.as_ref().token.range
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().token.line_number
    }
}

#[derive(Debug, Clone)]
pub struct CoreMissingTokenNode {
    pub expected_symbols: Rc<Vec<&'static str>>,
    pub received_token: Token,
}

#[derive(Debug, Clone)]
pub struct MissingTokenNode(Rc<CoreMissingTokenNode>);
impl MissingTokenNode {
    pub fn new(expected_symbols: &Rc<Vec<&'static str>>, received_token: &Token) -> Self {
        let node = Rc::new(CoreMissingTokenNode {
            expected_symbols: expected_symbols.clone(),
            received_token: received_token.clone(),
        });
        MissingTokenNode(node)
    }

    impl_core_ref!(CoreMissingTokenNode);
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

#[derive(Debug, Clone)]
pub struct CoreSkippedTokenNode {
    pub skipped_token: Token,
}

#[derive(Debug, Clone)]
pub struct SkippedTokenNode(Rc<CoreSkippedTokenNode>);
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

    impl_core_ref!(CoreSkippedTokenNode);
}
impl Node for SkippedTokenNode {
    fn range(&self) -> TextRange {
        self.0.as_ref().skipped_token.range
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().skipped_token.line_number
    }
}

#[derive(Debug, Clone, Node)]
pub enum CoreExpressionNode {
    UNARY(UnaryExpressionNode),
    BINARY(BinaryExpressionNode),
    COMPARISON(ComparisonNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct ExpressionNode(Rc<CoreExpressionNode>);
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
            None => unreachable!(
                "any node passed in this method as operator should be a valid operator"
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

    pub fn new_with_comparison(operands: Vec<ExpressionNode>, operators: Vec<TokenNode>) -> Self {
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

    impl_core_ref!(CoreExpressionNode);
}
default_errornous_node_impl!(ExpressionNode, CoreExpressionNode);

#[derive(Debug, Clone)]
pub struct CoreComparisonNode {
    pub operands: Vec<ExpressionNode>,
    pub operators: Vec<TokenNode>,
}

#[derive(Debug, Clone)]
pub struct ComparisonNode(Rc<CoreComparisonNode>);
impl ComparisonNode {
    pub fn new(operands: Vec<ExpressionNode>, operators: Vec<TokenNode>) -> Self {
        let node = Rc::new(CoreComparisonNode {
            operands,
            operators,
        });
        ComparisonNode(node)
    }

    impl_core_ref!(CoreComparisonNode);
}
impl Node for ComparisonNode {
    fn range(&self) -> TextRange {
        let core_node = self.0.as_ref();
        impl_range!(
            core_node.operands[0],
            core_node.operands[core_node.operands.len() - 1]
        )
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().operands[0].start_line_number()
    }
}

#[derive(Debug, Clone, Node)]
pub enum CoreAtomicExpressionNode {
    BOOL_VALUE(TokenNode),
    INTEGER(TokenNode),
    FLOATING_POINT_NUMBER(TokenNode),
    LITERAL(TokenNode),
    PARENTHESISED_EXPRESSION(ParenthesisedExpressionNode),
    ATOM(AtomNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct AtomicExpressionNode(Rc<CoreAtomicExpressionNode>);
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

    impl_core_ref!(CoreAtomicExpressionNode);
}
default_errornous_node_impl!(AtomicExpressionNode, CoreAtomicExpressionNode);

#[derive(Debug, Clone)]
pub struct CoreParenthesisedExpressionNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub expr: ExpressionNode,
}

#[derive(Debug, Clone)]
pub struct ParenthesisedExpressionNode(Rc<CoreParenthesisedExpressionNode>);
impl ParenthesisedExpressionNode {
    pub fn new(expr: &ExpressionNode, lparen: &TokenNode, rparen: &TokenNode) -> Self {
        let node = Rc::new(CoreParenthesisedExpressionNode {
            lparen: lparen.clone(),
            rparen: rparen.clone(),
            expr: expr.clone(),
        });
        ParenthesisedExpressionNode(node)
    }

    impl_core_ref!(CoreParenthesisedExpressionNode);
}
impl Node for ParenthesisedExpressionNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().lparen, self.0.as_ref().rparen)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().lparen.start_line_number()
    }
}

#[derive(Debug, Clone, Node)]
pub enum CoreUnaryExpressionNode {
    ATOMIC(AtomicExpressionNode),
    UNARY(OnlyUnaryExpressionNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub enum UnaryOperatorKind {
    PLUS,
    MINUS,
    NOT,
}

#[derive(Debug, Clone)]
pub struct UnaryExpressionNode(Rc<CoreUnaryExpressionNode>);
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

    impl_core_ref!(CoreUnaryExpressionNode);
}
default_errornous_node_impl!(UnaryExpressionNode, CoreUnaryExpressionNode);

#[derive(Debug, Clone)]
pub struct CoreOnlyUnaryExpressionNode {
    pub operator: TokenNode,
    pub unary_expr: UnaryExpressionNode,
    pub operator_kind: UnaryOperatorKind,
}

#[derive(Debug, Clone)]
pub struct OnlyUnaryExpressionNode(Rc<CoreOnlyUnaryExpressionNode>);
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

    impl_core_ref!(CoreOnlyUnaryExpressionNode);
}
impl Node for OnlyUnaryExpressionNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().operator, self.0.as_ref().unary_expr)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().operator.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreBinaryExpressionNode {
    pub operator_kind: BinaryOperatorKind,
    pub operator: TokenNode,
    pub left_expr: ExpressionNode,
    pub right_expr: ExpressionNode,
}

#[derive(Debug, Clone)]
pub struct BinaryExpressionNode(Rc<CoreBinaryExpressionNode>);
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

    impl_core_ref!(CoreBinaryExpressionNode);
}
impl Node for BinaryExpressionNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().left_expr, self.0.as_ref().right_expr)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().left_expr.start_line_number()
    }
}

#[derive(Debug, Clone, Node)]
pub enum CoreParamsNode {
    OK(OkParamsNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct ParamsNode(Rc<CoreParamsNode>);
impl ParamsNode {
    pub fn new(ok_params_node: &OkParamsNode) -> Self {
        let node = Rc::new(CoreParamsNode::OK(ok_params_node.clone()));
        ParamsNode(node)
    }

    impl_core_ref!(CoreParamsNode);
}
default_errornous_node_impl!(ParamsNode, CoreParamsNode);

#[derive(Debug, Clone)]
pub struct CoreOkParamsNode {
    pub comma: Option<TokenNode>,
    pub param: ExpressionNode,
    pub remaining_params: Option<ParamsNode>,
}

#[derive(Debug, Clone)]
pub struct OkParamsNode(Rc<CoreOkParamsNode>);
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

    impl_core_ref!(CoreOkParamsNode);
}
impl Node for OkParamsNode {
    fn range(&self) -> TextRange {
        match &self.0.as_ref().remaining_params {
            Some(remaining_params) => impl_range!(self.0.as_ref().param, remaining_params),
            None => impl_range!(self.0.as_ref().param, self.0.as_ref().param),
        }
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().param.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreCallExpressionNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub function_name: TokenNode,
    pub params: Option<ParamsNode>,
}

#[derive(Debug, Clone)]
pub struct CallExpressionNode(Rc<CoreCallExpressionNode>);
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
            params: extract_from_option!(params),
        });
        CallExpressionNode(node)
    }

    impl_core_ref!(CoreCallExpressionNode);
}
impl Node for CallExpressionNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().function_name, self.0.as_ref().rparen)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().function_name.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreClassMethodCallNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub double_colon: TokenNode,
    pub class_name: TokenNode,
    pub class_method_name: TokenNode,
    pub params: Option<ParamsNode>,
}

#[derive(Debug, Clone)]
pub struct ClassMethodCallNode(Rc<CoreClassMethodCallNode>);
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
            params: extract_from_option!(params),
        });
        ClassMethodCallNode(node)
    }

    impl_core_ref!(CoreClassMethodCallNode);
}
impl Node for ClassMethodCallNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().class_name, self.0.as_ref().rparen)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().class_name.start_line_number()
    }
}

#[derive(Debug, Clone, Node)]
pub enum CoreAtomNode {
    ATOM_START(AtomStartNode),            // id, id(...), id::id(...)
    CALL(CallNode),                       // A(...)
    PROPERTRY_ACCESS(PropertyAccessNode), // A.id
    METHOD_ACCESS(MethodAccessNode),      // A.id(...)
    INDEX_ACCESS(IndexAccessNode),        // A[<expr>]
}

#[derive(Debug, Clone)]
pub struct AtomNode(Rc<CoreAtomNode>);
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

    impl_core_ref!(CoreAtomNode);
}

#[derive(Debug, Clone, Node)]
pub enum CoreAtomStartNode {
    IDENTIFIER(TokenNode),                  // id
    FUNCTION_CALL(CallExpressionNode),      // id(...)
    CLASS_METHOD_CALL(ClassMethodCallNode), // id::id(...)
}

#[derive(Debug, Clone)]
pub struct AtomStartNode(Rc<CoreAtomStartNode>);
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

    impl_core_ref!(CoreAtomStartNode);
}

#[derive(Debug, Clone)]
pub struct CoreCallNode {
    pub atom: AtomNode,
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub params: Option<ParamsNode>,
}

#[derive(Debug, Clone)]
pub struct CallNode(Rc<CoreCallNode>);
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
            params: extract_from_option!(params),
        });
        CallNode(node)
    }

    impl_core_ref!(CoreCallNode);
}
impl Node for CallNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().atom, self.0.as_ref().rparen)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().atom.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CorePropertyAccessNode {
    pub dot: TokenNode,
    pub atom: AtomNode,
    pub propertry: TokenNode,
}

#[derive(Debug, Clone)]
pub struct PropertyAccessNode(Rc<CorePropertyAccessNode>);
impl PropertyAccessNode {
    fn new(atom: &AtomNode, propertry: &TokenNode, dot: &TokenNode) -> Self {
        let node = Rc::new(CorePropertyAccessNode {
            dot: dot.clone(),
            atom: atom.clone(),
            propertry: propertry.clone(),
        });
        PropertyAccessNode(node)
    }

    impl_core_ref!(CorePropertyAccessNode);
}
impl Node for PropertyAccessNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().atom, self.0.as_ref().propertry)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().atom.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreMethodAccessNode {
    pub lparen: TokenNode,
    pub rparen: TokenNode,
    pub dot: TokenNode,
    pub atom: AtomNode,
    pub method_name: TokenNode,
    pub params: Option<ParamsNode>,
}

#[derive(Debug, Clone)]
pub struct MethodAccessNode(Rc<CoreMethodAccessNode>);
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
            params: extract_from_option!(params),
        });
        MethodAccessNode(node)
    }

    impl_core_ref!(CoreMethodAccessNode);
}
impl Node for MethodAccessNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().atom, self.0.as_ref().rparen)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().atom.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreIndexAccessNode {
    pub lsquare: TokenNode,
    pub rsquare: TokenNode,
    pub atom: AtomNode,
    pub index: ExpressionNode,
}

#[derive(Debug, Clone)]
pub struct IndexAccessNode(Rc<CoreIndexAccessNode>);
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

    impl_core_ref!(CoreIndexAccessNode);
}
impl Node for IndexAccessNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().atom, self.0.as_ref().rsquare)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().atom.start_line_number()
    }
}

#[derive(Debug, Clone, Node)]
pub enum CoreRAssignmentNode {
    LAMBDA(FunctionDeclarationNode),
    EXPRESSION(ExpressionStatementNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct RAssignmentNode(Rc<CoreRAssignmentNode>);
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

    impl_core_ref!(CoreRAssignmentNode);
}
default_errornous_node_impl!(RAssignmentNode, CoreRAssignmentNode);
