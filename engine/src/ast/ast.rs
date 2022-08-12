// AST Nodes have inner mutability to enable dynamic changes to AST like monomorphism of generics or macro expansion.
// ASTNode has weak reference to core nodes to avoid memory leaks.
// See `https://doc.rust-lang.org/book/ch15-06-reference-cycles.html` for more information

#[macro_use]
use jarvil_macros::set_parent;
#[macro_use]
use jarvil_macros::Nodify;

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
    fn start_index(&self) -> usize;
    fn end_index(&self) -> usize;
    fn start_line_number(&self) -> usize;
}

pub trait ErrornousNode {
    fn new_with_missing_tokens(
        expected_symbols: &Rc<Vec<&'static str>>,
        received_token: &Token,
        lookahead: usize,
    ) -> Self;
}

#[derive(Debug, Clone, Nodify)]
pub enum ASTNode {
    BLOCK(BlockNode),
    STATEMENT_INDENT_WRAPPER(StatemenIndentWrapperNode),
    SKIPPED_TOKENS(SkippedTokens),
    STATEMENT(StatementNode),
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
    SKIPPED_TOKEN(SkippedTokenNode)
}

#[derive(Debug, Clone)]
pub struct CoreBlockNode {
    newline: TokenNode,
    pub stmts: Rc<RefCell<Vec<StatemenIndentWrapperNode>>>,
    scope: Option<Namespace>,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct BlockNode(pub Rc<RefCell<CoreBlockNode>>);
impl BlockNode {
    pub fn new(stmts: &Rc<RefCell<Vec<StatemenIndentWrapperNode>>>, newline: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreBlockNode {
            newline: newline.clone(),
            stmts: stmts.clone(),
            scope: None,
            parent: None,
        }));
        impl_set_parent!(newline, BLOCK, node, WeakBlockNode);
        for stmt in &*stmts.as_ref().borrow() {
            impl_set_parent!(stmt, BLOCK, node, WeakBlockNode);
        }
        BlockNode(node)
    }

    pub fn set_scope(&self, scope: &Namespace) {
        self.core_ref_mut().scope = Some(scope.clone());
    }

    core_node_access!(CoreBlockNode);
}
impl Node for BlockNode {
    default_node_impl!(BlockNode);
    fn start_index(&self) -> usize {
        self.core_ref().newline.start_index()
    }
    fn end_index(&self) -> usize {
        let stmts_len = self.core_ref().stmts.as_ref().borrow().len();
        self.core_ref().stmts.as_ref().borrow()[stmts_len - 1].end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().newline.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreStatemenIndentWrapperNode {
    pub kind: StatementIndentWrapperKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum StatementIndentWrapperKind {
    CORRECTLY_INDENTED(StatementNode),
    INCORRECTLY_INDENTED((StatementNode, (i64, i64))),
    LEADING_SKIPPED_TOKENS(SkippedTokens), // skipped tokens leading to the next stmt in block
    TRAILING_SKIPPED_TOKENS(SkippedTokens), // skipped tokens trailing to the previous stmt in block
    EXTRA_NEWLINES(SkippedTokens),
}

#[derive(Debug, Clone)]
pub struct StatemenIndentWrapperNode(pub Rc<RefCell<CoreStatemenIndentWrapperNode>>);
impl StatemenIndentWrapperNode {
    #[set_parent(STATEMENT_INDENT_WRAPPER, WeakStatemenIndentWrapperNode)]
    pub fn new_with_correctly_indented(stmt: &StatementNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStatemenIndentWrapperNode{
            kind: StatementIndentWrapperKind::CORRECTLY_INDENTED(stmt.clone()),
            parent: None,
        }));
        StatemenIndentWrapperNode(node)
    }

    #[set_parent(STATEMENT_INDENT_WRAPPER, WeakStatemenIndentWrapperNode)]
    pub fn new_with_incorrectly_indented(stmt: &StatementNode, expected_indent: i64, received_indent: i64) -> Self {
        let node = Rc::new(RefCell::new(CoreStatemenIndentWrapperNode{
            kind: StatementIndentWrapperKind::INCORRECTLY_INDENTED((stmt.clone(), (expected_indent, received_indent))),
            parent: None,
        }));
        StatemenIndentWrapperNode(node)
    }

    pub fn new_with_leading_skipped_tokens(skipped_tokens: &SkippedTokens) -> Self {
        let node = Rc::new(RefCell::new(CoreStatemenIndentWrapperNode{
            kind: StatementIndentWrapperKind::LEADING_SKIPPED_TOKENS(skipped_tokens.clone()),
            parent: None,
        }));
        impl_set_parent!(skipped_tokens, STATEMENT_INDENT_WRAPPER, node, WeakStatemenIndentWrapperNode);
        StatemenIndentWrapperNode(node)
    }

    pub fn new_with_trailing_skipped_tokens(skipped_tokens: &SkippedTokens) -> Self {
        let node = Rc::new(RefCell::new(CoreStatemenIndentWrapperNode{
            kind: StatementIndentWrapperKind::TRAILING_SKIPPED_TOKENS(skipped_tokens.clone()),
            parent: None,
        }));
        impl_set_parent!(skipped_tokens, STATEMENT_INDENT_WRAPPER, node, WeakStatemenIndentWrapperNode);
        StatemenIndentWrapperNode(node)
    }

    pub fn new_with_extra_newlines(skipped_tokens: &SkippedTokens) -> Self {
        let node = Rc::new(RefCell::new(CoreStatemenIndentWrapperNode{
            kind: StatementIndentWrapperKind::EXTRA_NEWLINES(skipped_tokens.clone()),
            parent: None,
        }));
        impl_set_parent!(skipped_tokens, STATEMENT_INDENT_WRAPPER, node, WeakStatemenIndentWrapperNode);
        StatemenIndentWrapperNode(node)
    }

    core_node_access!(CoreStatemenIndentWrapperNode);
}
impl Node for StatemenIndentWrapperNode {
    default_node_impl!(StatemenIndentWrapperNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().kind {
            StatementIndentWrapperKind::CORRECTLY_INDENTED(stmt) => stmt.start_index(),
            StatementIndentWrapperKind::INCORRECTLY_INDENTED((stmt, _)) => stmt.start_index(),
            StatementIndentWrapperKind::LEADING_SKIPPED_TOKENS(skipped_tokens) => skipped_tokens.start_index(),
            StatementIndentWrapperKind::TRAILING_SKIPPED_TOKENS(skipped_tokens) => skipped_tokens.start_index(),
            StatementIndentWrapperKind::EXTRA_NEWLINES(skipped_tokens) => skipped_tokens.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().kind {
            StatementIndentWrapperKind::CORRECTLY_INDENTED(stmt) => stmt.end_index(),
            StatementIndentWrapperKind::INCORRECTLY_INDENTED((stmt, _)) => stmt.end_index(),
            StatementIndentWrapperKind::LEADING_SKIPPED_TOKENS(skipped_tokens) => skipped_tokens.end_index(),
            StatementIndentWrapperKind::TRAILING_SKIPPED_TOKENS(skipped_tokens) => skipped_tokens.end_index(),
            StatementIndentWrapperKind::EXTRA_NEWLINES(skipped_tokens) => skipped_tokens.end_index(),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().kind {
            StatementIndentWrapperKind::CORRECTLY_INDENTED(stmt) => stmt.start_line_number(),
            StatementIndentWrapperKind::INCORRECTLY_INDENTED((stmt, _)) => stmt.start_line_number(),
            StatementIndentWrapperKind::LEADING_SKIPPED_TOKENS(skipped_tokens) => skipped_tokens.start_line_number(),
            StatementIndentWrapperKind::TRAILING_SKIPPED_TOKENS(skipped_tokens) => skipped_tokens.start_line_number(),
            StatementIndentWrapperKind::EXTRA_NEWLINES(skipped_tokens) => skipped_tokens.start_line_number(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CoreSkippedTokens {
    pub skipped_tokens: Rc<Vec<SkippedTokenNode>>,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct SkippedTokens(pub Rc<RefCell<CoreSkippedTokens>>);
impl SkippedTokens {
    pub fn new_with_leading_skipped_tokens(skipped_tokens: &Rc<Vec<SkippedTokenNode>>) -> Self {
        let node = Rc::new(RefCell::new(CoreSkippedTokens {
            skipped_tokens: skipped_tokens.clone(),
            parent: None,
        }));
        for skipped_token in skipped_tokens.as_ref() {
            impl_set_parent!(skipped_token, SKIPPED_TOKENS, node, WeakSkippedTokens);
        }
        SkippedTokens(node)
    }

    pub fn new_with_trailing_skipped_tokens(skipped_tokens: &Rc<Vec<SkippedTokenNode>>) -> Self {
        let node = Rc::new(RefCell::new(CoreSkippedTokens {
            skipped_tokens: skipped_tokens.clone(),
            parent: None,
        }));
        for skipped_token in skipped_tokens.as_ref() {
            impl_set_parent!(skipped_token, SKIPPED_TOKENS, node, WeakSkippedTokens);
        }
        SkippedTokens(node)
    }

    pub fn new_with_extra_newlines(extra_newlines: &Rc<Vec<SkippedTokenNode>>) -> Self {
        let node = Rc::new(RefCell::new(CoreSkippedTokens {
            skipped_tokens: extra_newlines.clone(),
            parent: None,
        }));
        for skipped_token in extra_newlines.as_ref() {
            impl_set_parent!(skipped_token, SKIPPED_TOKENS, node, WeakSkippedTokens);
        }
        SkippedTokens(node)
    }

    core_node_access!(CoreSkippedTokens);
}
impl Node for SkippedTokens {
    default_node_impl!(SkippedTokens);
    fn start_index(&self) -> usize {
        self.core_ref().skipped_tokens[0].start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().skipped_tokens[self.core_ref().skipped_tokens.len() - 1].end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().skipped_tokens[0].start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreStatementNode {
    pub kind: StatementKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    // expr, variable declaration, type struct declaration, type lambda declaration, interface declaration,
    // assignment, if, for, while, return, continue, break, implementation of interfaces, implementation of structs
    EXPRESSION((ExpressionNode, TokenNode)),
    ASSIGNMENT(AssignmentNode),
    VARIABLE_DECLARATION(VariableDeclarationNode),
    FUNCTION_DECLARATION(FunctionDeclarationNode),
    TYPE_DECLARATION(TypeDeclarationNode),
    STRUCT_STATEMENT(StructStatementNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct StatementNode(pub Rc<RefCell<CoreStatementNode>>);
impl StatementNode {
    #[set_parent(STATEMENT, WeakStatementNode)]
    pub fn new_with_expression(expr: &ExpressionNode, newline: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStatementNode {
            kind: StatementKind::EXPRESSION((expr.clone(), newline.clone())),
            parent: None,
        }));
        StatementNode(node)
    }

    #[set_parent(STATEMENT, WeakStatementNode)]
    pub fn new_with_assignment(assignment: &AssignmentNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStatementNode {
            kind: StatementKind::ASSIGNMENT(assignment.clone()),
            parent: None,
        }));
        StatementNode(node)
    }

    #[set_parent(STATEMENT, WeakStatementNode)]
    pub fn new_with_variable_declaration(variable_decl: &VariableDeclarationNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStatementNode {
            kind: StatementKind::VARIABLE_DECLARATION(variable_decl.clone()),
            parent: None,
        }));
        StatementNode(node)
    }

    #[set_parent(STATEMENT, WeakStatementNode)]
    pub fn new_with_function_declaration(function_decl: &FunctionDeclarationNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStatementNode {
            kind: StatementKind::FUNCTION_DECLARATION(function_decl.clone()),
            parent: None,
        }));
        StatementNode(node)
    }

    #[set_parent(STATEMENT, WeakStatementNode)]
    pub fn new_with_type_declaration(type_decl: &TypeDeclarationNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStatementNode {
            kind: StatementKind::TYPE_DECLARATION(type_decl.clone()),
            parent: None,
        }));
        StatementNode(node)
    }

    #[set_parent(STATEMENT, WeakStatementNode)]
    pub fn new_with_struct_stmt(struct_stmt: &StructStatementNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStatementNode {
            kind: StatementKind::STRUCT_STATEMENT(struct_stmt.clone()),
            parent: None,
        }));
        StatementNode(node)
    }

    core_node_access!(CoreStatementNode);
}
impl Node for StatementNode {
    default_node_impl!(StatementNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().kind {
            StatementKind::EXPRESSION((expr, _)) => expr.start_index(),
            StatementKind::ASSIGNMENT(assignment) => assignment.start_index(),
            StatementKind::VARIABLE_DECLARATION(variable_decl) => variable_decl.start_index(), 
            StatementKind::FUNCTION_DECLARATION(func_decl) => func_decl.start_index(),
            StatementKind::TYPE_DECLARATION(type_decl) => type_decl.start_index(),
            StatementKind::STRUCT_STATEMENT(struct_stmt) => struct_stmt.start_index(),
            StatementKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().kind {
            StatementKind::EXPRESSION((_, newline)) => newline.end_index(),
            StatementKind::ASSIGNMENT(assignment) => assignment.end_index(),
            StatementKind::VARIABLE_DECLARATION(variable_decl) => variable_decl.end_index(), 
            StatementKind::FUNCTION_DECLARATION(func_decl) => func_decl.end_index(),
            StatementKind::TYPE_DECLARATION(type_decl) => type_decl.end_index(),
            StatementKind::STRUCT_STATEMENT(struct_stmt) => struct_stmt.end_index(),
            StatementKind::MISSING_TOKENS(missing_tokens) => missing_tokens.end_index(),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().kind {
            StatementKind::EXPRESSION((expr, _)) => expr.start_line_number(),
            StatementKind::ASSIGNMENT(assignment) => assignment.start_line_number(),
            StatementKind::VARIABLE_DECLARATION(variable_decl) => variable_decl.start_line_number(), 
            StatementKind::FUNCTION_DECLARATION(func_decl) => func_decl.start_line_number(),
            StatementKind::TYPE_DECLARATION(type_decl) => type_decl.start_line_number(),
            StatementKind::STRUCT_STATEMENT(struct_stmt) => struct_stmt.start_line_number(),
            StatementKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_line_number(),
        }
    }
}
default_errornous_node_impl!(StatementNode, CoreStatementNode, StatementKind);

#[derive(Debug, Clone)]
pub struct CoreAssignmentNode {
    pub kind: AssignmentKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum AssignmentKind {
    OK(OkAssignmentNode),
    INVALID_L_VALUE(InvalidLValueNode),
}

#[derive(Debug, Clone)]
pub struct AssignmentNode(pub Rc<RefCell<CoreAssignmentNode>>);
impl AssignmentNode {
    pub fn new(l_atom: &AtomNode, r_assign: &RAssignmentNode, equal: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAssignmentNode {
            kind: AssignmentKind::OK(OkAssignmentNode::new(l_atom, r_assign, equal)),
            parent: None,
        }));
        AssignmentNode(node)
    }

    pub fn new_with_invalid_l_value(l_expr: &ExpressionNode, r_assign: &RAssignmentNode, equal: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAssignmentNode{
            kind: AssignmentKind::INVALID_L_VALUE(InvalidLValueNode::new(l_expr, r_assign, equal)),
            parent: None,
        }));
        AssignmentNode(node)
    }

    core_node_access!(CoreAssignmentNode);
}
impl Node for AssignmentNode {
    default_node_impl!(AssignmentNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().kind {
            AssignmentKind::OK(ok_assignment) => ok_assignment.start_index(),
            AssignmentKind::INVALID_L_VALUE(invalid_l_value) => invalid_l_value.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().kind {
            AssignmentKind::OK(ok_assignment) => ok_assignment.end_index(),
            AssignmentKind::INVALID_L_VALUE(invalid_l_value) => invalid_l_value.end_index(),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().kind {
            AssignmentKind::OK(ok_assignment) => ok_assignment.start_line_number(),
            AssignmentKind::INVALID_L_VALUE(invalid_l_value) => invalid_l_value.start_line_number(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CoreOkAssignmentNode {
    equal: TokenNode,
    pub l_atom: AtomNode,
    pub r_assign: RAssignmentNode,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct OkAssignmentNode(pub Rc<RefCell<CoreOkAssignmentNode>>);
impl OkAssignmentNode {
    #[set_parent(OK_ASSIGNMENT, WeakOkAssignmentNode)]
    pub fn new(l_atom: &AtomNode, r_assign: &RAssignmentNode, equal: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreOkAssignmentNode{
            equal: equal.clone(),
            l_atom: l_atom.clone(),
            r_assign: r_assign.clone(),
            parent: None,
        }));
        OkAssignmentNode(node)
    }

    core_node_access!(CoreOkAssignmentNode);
}
impl Node for OkAssignmentNode {
    default_node_impl!(OkAssignmentNode);
    fn start_index(&self) -> usize {
        self.core_ref().l_atom.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().r_assign.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().l_atom.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreInvalidLValueNode {
    pub l_expr: ExpressionNode,
    equal: TokenNode,
    pub r_assign: RAssignmentNode,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct InvalidLValueNode(pub Rc<RefCell<CoreInvalidLValueNode>>);
impl InvalidLValueNode {
    #[set_parent(INVALID_L_VALUE, WeakInvalidLValueNode)]
    pub fn new(l_expr: &ExpressionNode, r_assign: &RAssignmentNode, equal: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreInvalidLValueNode{
            l_expr: l_expr.clone(),
            equal: equal.clone(),
            r_assign: r_assign.clone(),
            parent: None,
        }));
        InvalidLValueNode(node)
    }

    core_node_access!(CoreInvalidLValueNode);
}
impl Node for InvalidLValueNode {
    default_node_impl!(InvalidLValueNode);
    fn start_index(&self) -> usize {
        self.core_ref().l_expr.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().r_assign.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().l_expr.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreStructStatementNode {
    newline: TokenNode,
    pub name_type_spec: NameTypeSpecNode,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct StructStatementNode(Rc<RefCell<CoreStructStatementNode>>);
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

    core_node_access!(CoreStructStatementNode);
}
impl Node for StructStatementNode {
    default_node_impl!(StructStatementNode);
    fn start_index(&self) -> usize {
        self.core_ref().name_type_spec.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().newline.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().name_type_spec.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreTypeDeclarationNode {
    pub kind: TypeDeclarationKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum TypeDeclarationKind {
    STRUCT(StructDeclarationNode),
    LAMBDA(LambdaDeclarationNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct TypeDeclarationNode(Rc<RefCell<CoreTypeDeclarationNode>>);
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

    #[set_parent(TYPE_DECLARATION, WeakTypeDeclarationNode)]
    pub fn new_with_lambda(lambda: &LambdaDeclarationNode) -> Self {
        let node = Rc::new(RefCell::new(CoreTypeDeclarationNode {
            kind: TypeDeclarationKind::LAMBDA(lambda.clone()),
            parent: None,
        }));
        TypeDeclarationNode(node)
    }

    core_node_access!(CoreTypeDeclarationNode);
}
impl Node for TypeDeclarationNode {
    default_node_impl!(TypeDeclarationNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().kind {
            TypeDeclarationKind::STRUCT(struct_decl) => struct_decl.start_index(),
            TypeDeclarationKind::LAMBDA(lambda_decl) => lambda_decl.start_index(),
            TypeDeclarationKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().kind {
            TypeDeclarationKind::STRUCT(struct_decl) => struct_decl.end_index(),
            TypeDeclarationKind::LAMBDA(lambda_decl) => lambda_decl.end_index(),
            TypeDeclarationKind::MISSING_TOKENS(missing_tokens) => missing_tokens.end_index(),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().kind {
            TypeDeclarationKind::STRUCT(struct_decl) => struct_decl.start_line_number(),
            TypeDeclarationKind::LAMBDA(lambda_decl) => lambda_decl.start_line_number(),
            TypeDeclarationKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_line_number(),
        }
    }
}
default_errornous_node_impl!(
    TypeDeclarationNode,
    CoreTypeDeclarationNode,
    TypeDeclarationKind
);

#[derive(Debug, Clone)]
pub struct CoreStructDeclarationNode {
    type_keyword: TokenNode,
    colon: TokenNode,
    pub name: TokenNode,
    pub block: BlockNode,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct StructDeclarationNode(Rc<RefCell<CoreStructDeclarationNode>>);
impl StructDeclarationNode {
    #[set_parent(STRUCT_DECLARATION, WeakStructDeclarationNode)]
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
        StructDeclarationNode(node)
    }

    core_node_access!(CoreStructDeclarationNode);
}
impl Node for StructDeclarationNode {
    default_node_impl!(StructDeclarationNode);
    fn start_index(&self) -> usize {
        self.core_ref().type_keyword.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().block.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().type_keyword.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreLambdaDeclarationNode {
    pub kind: LambdaDeclarationKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum LambdaDeclarationKind {
    OK(OkLambdaDeclarationNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct LambdaDeclarationNode(Rc<RefCell<CoreLambdaDeclarationNode>>);
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

    core_node_access!(CoreLambdaDeclarationNode);
}
impl Node for LambdaDeclarationNode {
    default_node_impl!(LambdaDeclarationNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().kind {
            LambdaDeclarationKind::OK(ok_lambda_decl) => ok_lambda_decl.start_index(),
            LambdaDeclarationKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().kind {
            LambdaDeclarationKind::OK(ok_lambda_decl) => ok_lambda_decl.end_index(),
            LambdaDeclarationKind::MISSING_TOKENS(missing_tokens) => missing_tokens.end_index(),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().kind {
            LambdaDeclarationKind::OK(ok_lambda_decl) => ok_lambda_decl.start_line_number(),
            LambdaDeclarationKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_line_number(),
        }
    }
}
default_errornous_node_impl!(
    LambdaDeclarationNode,
    CoreLambdaDeclarationNode,
    LambdaDeclarationKind
);

#[derive(Debug, Clone)]
pub struct CoreOkLambdaDeclarationNode {
    type_keyword: TokenNode,
    colon: TokenNode,
    lparen: TokenNode,
    rparen: TokenNode,
    right_arrow: Option<TokenNode>,
    newline: TokenNode,
    pub name: TokenNode,
    pub args: Option<NameTypeSpecsNode>,
    pub return_type: Option<TypeExpressionNode>,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct OkLambdaDeclarationNode(Rc<RefCell<CoreOkLambdaDeclarationNode>>);
impl OkLambdaDeclarationNode {
    #[set_parent(OK_LAMBDA_DECLARATION, WeakOkLambdaDeclarationNode)]
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
            right_arrow: extract_from_option!(right_arrow),
            newline: newline.clone(),
            type_keyword: type_keyword.clone(),
            colon: colon.clone(),
            name: name.clone(),
            args: extract_from_option!(args),
            return_type: extract_from_option!(return_type),
            parent: None,
        }));
        OkLambdaDeclarationNode(node)
    }

    core_node_access!(CoreOkLambdaDeclarationNode);
}
impl Node for OkLambdaDeclarationNode {
    default_node_impl!(OkLambdaDeclarationNode);
    fn start_index(&self) -> usize {
        self.core_ref().type_keyword.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().newline.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().type_keyword.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreFunctionDeclarationNode {
    pub kind: FunctionDeclarationKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum FunctionDeclarationKind {
    OK(OkFunctionDeclarationNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct FunctionDeclarationNode(pub Rc<RefCell<CoreFunctionDeclarationNode>>);
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

    core_node_access!(CoreFunctionDeclarationNode);
}
impl Node for FunctionDeclarationNode {
    default_node_impl!(FunctionDeclarationNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().kind {
            FunctionDeclarationKind::OK(ok_func_decl) => ok_func_decl.start_index(),
            FunctionDeclarationKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().kind {
            FunctionDeclarationKind::OK(ok_func_decl) => ok_func_decl.end_index(),
            FunctionDeclarationKind::MISSING_TOKENS(missing_tokens) => missing_tokens.end_index(),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().kind {
            FunctionDeclarationKind::OK(ok_func_decl) => ok_func_decl.start_line_number(),
            FunctionDeclarationKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_line_number(),
        }
    }
}
default_errornous_node_impl!(
    FunctionDeclarationNode,
    CoreFunctionDeclarationNode,
    FunctionDeclarationKind
);

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum FuncKeywordKind {
    DEF(TokenNode),
    FUNC(TokenNode),
}

#[derive(Debug, Clone)]
pub struct OkFunctionDeclarationNode(pub Rc<RefCell<CoreOkFunctionDeclarationNode>>);
impl OkFunctionDeclarationNode {
    #[set_parent(OK_FUNCTION_DECLARATION, WeakOkFunctionDeclarationNode)]
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
            right_arrow: extract_from_option!(right_arrow),
            colon: colon.clone(),
            name: extract_from_option!(name),
            args: extract_from_option!(args),
            return_type: extract_from_option!(return_type),
            block: block.clone(),
            parent: None,
        }));
        match func_keyword {
            FuncKeywordKind::DEF(def_node) => {
                impl_set_parent!(def_node, OK_FUNCTION_DECLARATION, node, WeakOkFunctionDeclarationNode);
            }
            FuncKeywordKind::FUNC(func_node) => {
                impl_set_parent!(func_node, OK_FUNCTION_DECLARATION, node, WeakOkFunctionDeclarationNode);
            }
        }
        OkFunctionDeclarationNode(node)
    }

    core_node_access!(CoreOkFunctionDeclarationNode);
}
impl Node for OkFunctionDeclarationNode {
    default_node_impl!(OkFunctionDeclarationNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().func_keyword {
            FuncKeywordKind::DEF(token) => token.start_index(),
            FuncKeywordKind::FUNC(token) => token.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        self.core_ref().block.end_index()
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().func_keyword {
            FuncKeywordKind::DEF(token) => token.start_line_number(),
            FuncKeywordKind::FUNC(token) => token.start_line_number(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CoreVariableDeclarationNode {
    let_keyword: TokenNode,
    equal: TokenNode,
    pub name: TokenNode,
    pub r_assign: RAssignmentNode,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct VariableDeclarationNode(pub Rc<RefCell<CoreVariableDeclarationNode>>);
impl VariableDeclarationNode {
    #[set_parent(VARIABLE_DECLARATION, WeakVariableDeclarationNode)]
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
        VariableDeclarationNode(node)
    }

    core_node_access!(CoreVariableDeclarationNode);
}
impl Node for VariableDeclarationNode {
    default_node_impl!(VariableDeclarationNode);
    fn start_index(&self) -> usize {
        self.core_ref().let_keyword.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().r_assign.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().let_keyword.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreNameTypeSpecsNode {
    pub kind: NameTypeSpecsKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum NameTypeSpecsKind {
    OK(OkNameTypeSpecsNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct NameTypeSpecsNode(Rc<RefCell<CoreNameTypeSpecsNode>>);
impl NameTypeSpecsNode {
    #[set_parent(NAME_TYPE_SPECS, WeakNameTypeSpecsNode)]
    pub fn new(ok_name_type_specs: &OkNameTypeSpecsNode) -> Self {
        let node = Rc::new(RefCell::new(CoreNameTypeSpecsNode {
            kind: NameTypeSpecsKind::OK(ok_name_type_specs.clone()),
            parent: None,
        }));
        NameTypeSpecsNode(node)
    }

    pub fn get_name_type_spec_objs(&self, code: &Code) -> Vec<(Option<Rc<String>>, Option<Type>)> {
        match &self.core_ref().kind {
            NameTypeSpecsKind::OK(ok_name_type_specs) => {
                ok_name_type_specs.get_name_type_spec_objs(code)
            }
            _ => vec![],
        }
    }

    core_node_access!(CoreNameTypeSpecsNode);
}
impl Node for NameTypeSpecsNode {
    default_node_impl!(NameTypeSpecsNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().kind {
            NameTypeSpecsKind::OK(ok_name_type_specs) => ok_name_type_specs.start_index(),
            NameTypeSpecsKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().kind {
            NameTypeSpecsKind::OK(ok_name_type_specs) => ok_name_type_specs.end_index(),
            NameTypeSpecsKind::MISSING_TOKENS(missing_tokens) => missing_tokens.end_index(),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().kind {
            NameTypeSpecsKind::OK(ok_name_type_specs) => ok_name_type_specs.start_line_number(),
            NameTypeSpecsKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_line_number(),
        }
    }
}
default_errornous_node_impl!(NameTypeSpecsNode, CoreNameTypeSpecsNode, NameTypeSpecsKind);

#[derive(Debug, Clone)]
pub struct CoreOkNameTypeSpecsNode {
    comma: Option<TokenNode>,
    pub arg: NameTypeSpecNode,
    pub remaining_args: Option<NameTypeSpecsNode>,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct OkNameTypeSpecsNode(Rc<RefCell<CoreOkNameTypeSpecsNode>>);
impl OkNameTypeSpecsNode {
    #[set_parent(OK_NAME_TYPE_SPECS, WeakOkNameTypeSpecsNode)]
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
        OkNameTypeSpecsNode(node)
    }

    #[set_parent(OK_NAME_TYPE_SPECS, WeakOkNameTypeSpecsNode)]
    pub fn new_with_single_arg(arg: &NameTypeSpecNode) -> Self {
        let node = Rc::new(RefCell::new(CoreOkNameTypeSpecsNode {
            comma: None,
            arg: arg.clone(),
            remaining_args: None,
            parent: None,
        }));
        OkNameTypeSpecsNode(node)
    }

    pub fn get_name_type_spec_objs(&self, code: &Code) -> Vec<(Option<Rc<String>>, Option<Type>)> {
        let mut name_type_specs_vec: Vec<(Option<Rc<String>>, Option<Type>)> = vec![];
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

    core_node_access!(CoreOkNameTypeSpecsNode);
}
impl Node for OkNameTypeSpecsNode {
    default_node_impl!(OkNameTypeSpecsNode);
    fn start_index(&self) -> usize {
        self.core_ref().arg.start_index()
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().remaining_args {
            Some(remaining_args) => remaining_args.end_index(),
            None => self.core_ref().arg.end_index()
        }
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().arg.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreNameTypeSpecNode {
    colon: TokenNode,
    pub name: TokenNode,
    pub data_type: TypeExpressionNode,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct NameTypeSpecNode(Rc<RefCell<CoreNameTypeSpecNode>>);
impl NameTypeSpecNode {
    #[set_parent(NAME_TYPE_SPEC, WeakNameTypeSpecNode)]
    pub fn new(param_name: &TokenNode, param_type: &TypeExpressionNode, colon: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreNameTypeSpecNode {
            colon: colon.clone(),
            name: param_name.clone(),
            data_type: param_type.clone(),
            parent: None,
        }));
        NameTypeSpecNode(node)
    }

    pub fn get_name_spec_obj(&self, code: &Code) -> (Option<Rc<String>>, Option<Type>) {
        let name = match self.core_ref().name.get_ok() {
            Some(ok_name_node) => Some(Rc::new(ok_name_node.token_value(code))),
            None => None,
        };
        let type_obj = match self.core_ref().data_type.get_type_obj(code) {
            Some(type_obj) => Some(type_obj),
            None => None,
        };
        (name, type_obj)
    }

    core_node_access!(CoreNameTypeSpecNode);
}
impl Node for NameTypeSpecNode {
    default_node_impl!(NameTypeSpecNode);
    fn start_index(&self) -> usize {
        self.core_ref().name.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().data_type.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().name.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreTypeExpressionNode {
    pub kind: TypeExpressionKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum TypeExpressionKind {
    ATOMIC(AtomicTypeNode),
    USER_DEFINED(UserDefinedTypeNode),
    ARRAY(ArrayTypeNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct TypeExpressionNode(Rc<RefCell<CoreTypeExpressionNode>>);
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

    core_node_access!(CoreTypeExpressionNode);
}
impl Node for TypeExpressionNode {
    default_node_impl!(TypeExpressionNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().kind {
            TypeExpressionKind::ATOMIC(atomic) => atomic.start_index(),
            TypeExpressionKind::USER_DEFINED(user_defined) => user_defined.start_index(),
            TypeExpressionKind::ARRAY(array) => array.start_index(),
            TypeExpressionKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().kind {
            TypeExpressionKind::ATOMIC(atomic) => atomic.end_index(),
            TypeExpressionKind::USER_DEFINED(user_defined) => user_defined.end_index(),
            TypeExpressionKind::ARRAY(array) => array.end_index(),
            TypeExpressionKind::MISSING_TOKENS(missing_tokens) => missing_tokens.end_index(),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().kind {
            TypeExpressionKind::ATOMIC(atomic) => atomic.start_line_number(),
            TypeExpressionKind::USER_DEFINED(user_defined) => user_defined.start_line_number(),
            TypeExpressionKind::ARRAY(array) => array.start_line_number(),
            TypeExpressionKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_line_number(),
        }
    }
}
default_errornous_node_impl!(
    TypeExpressionNode,
    CoreTypeExpressionNode,
    TypeExpressionKind
);

#[derive(Debug, Clone)]
pub struct CoreAtomicTypeNode {
    pub kind: TokenNode,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct AtomicTypeNode(Rc<RefCell<CoreAtomicTypeNode>>);
impl AtomicTypeNode {
    #[set_parent(ATOMIC_TYPE, WeakAtomicTypeNode)]
    pub fn new(token: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicTypeNode {
            kind: token.clone(),
            parent: None,
        }));
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

    core_node_access!(CoreAtomicTypeNode);
}
impl Node for AtomicTypeNode {
    default_node_impl!(AtomicTypeNode);
    fn start_index(&self) -> usize {
        self.core_ref().kind.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().kind.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().kind.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreArrayTypeNode {
    lsquare: TokenNode,
    rsquare: TokenNode,
    semicolon: TokenNode,
    pub sub_type: TypeExpressionNode,
    pub size: TokenNode,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct ArrayTypeNode(Rc<RefCell<CoreArrayTypeNode>>);
impl ArrayTypeNode {
    #[set_parent(ARRAY_TYPE, WeakArrayTypeNode)]
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

    core_node_access!(CoreArrayTypeNode);
}
impl Node for ArrayTypeNode {
    default_node_impl!(ArrayTypeNode);
    fn start_index(&self) -> usize {
        self.core_ref().lsquare.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().rsquare.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().lsquare.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreUserDefinedTypeNode {
    pub name: TokenNode,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct UserDefinedTypeNode(Rc<RefCell<CoreUserDefinedTypeNode>>);
impl UserDefinedTypeNode {
    #[set_parent(USER_DEFINED_TYPE, WeakUserDefinedTypeNode)]
    pub fn new(identifier: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreUserDefinedTypeNode {
            name: identifier.clone(),
            parent: None,
        }));
        UserDefinedTypeNode(node)
    }

    pub fn get_type_obj(&self, code: &Code) -> Option<Type> {
        match self.core_ref().name.get_ok() {
            Some(ok_token_node) => {
                Some(Type::new_with_user_defined(ok_token_node.token_value(code)))
            }
            None => None,
        }
    }

    core_node_access!(CoreUserDefinedTypeNode);
}
impl Node for UserDefinedTypeNode {
    default_node_impl!(UserDefinedTypeNode);
    fn start_index(&self) -> usize {
        self.core_ref().name.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().name.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().name.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreTokenNode {
    pub kind: TokenKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    OK(OkTokenNode),
    MISSING_TOKENS(MissingTokenNode),
    SKIPPED(SkippedTokenNode),
}

#[derive(Debug, Clone)]
pub struct TokenNode(pub Rc<RefCell<CoreTokenNode>>);
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

    core_node_access!(CoreTokenNode);
}
impl Node for TokenNode {
    default_node_impl!(TokenNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().kind {
            TokenKind::OK(ok_token) => ok_token.start_index(),
            TokenKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_index(),
            TokenKind::SKIPPED(skipped_tokens) => skipped_tokens.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().kind {
            TokenKind::OK(ok_token) => ok_token.end_index(),
            TokenKind::MISSING_TOKENS(missing_tokens) => missing_tokens.end_index(),
            TokenKind::SKIPPED(skipped_tokens) => skipped_tokens.end_index(),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().kind {
            TokenKind::OK(ok_token) => ok_token.start_line_number(),
            TokenKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_line_number(),
            TokenKind::SKIPPED(skipped_tokens) => skipped_tokens.start_line_number(),
        }
    }
}
default_errornous_node_impl!(TokenNode, CoreTokenNode, TokenKind);

#[derive(Debug, Clone)]
pub struct CoreOkTokenNode {
    token: Token,
    kind: OkTokenKind,
    lookahead: usize,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum OkTokenKind {
    IDENTIFIER(Option<SymbolData>), // This is set when the identifier is resolved
    NON_IDENTIFIER,
}
#[derive(Debug, Clone)]
pub struct OkTokenNode(Rc<RefCell<CoreOkTokenNode>>);
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
            CoreToken::NOT_EQUAL        => Some(BinaryOperatorKind::NOT_EQUAL),
            CoreToken::DOUBLE_EQUAL     => Some(BinaryOperatorKind::DOUBLE_EQUAL),
            CoreToken::RBRACKET         => Some(BinaryOperatorKind::GREATER),
            CoreToken::GREATER_EQUAL    => Some(BinaryOperatorKind::GREATER_EQUAL),
            CoreToken::LBRACKET         => Some(BinaryOperatorKind::LESS),
            CoreToken::LESS_EQUAL       => Some(BinaryOperatorKind::LESS_EQUAL),
            CoreToken::DASH             => Some(BinaryOperatorKind::MINUS),
            CoreToken::PLUS             => Some(BinaryOperatorKind::PLUS),
            CoreToken::SLASH            => Some(BinaryOperatorKind::DIVIDE),
            CoreToken::STAR             => Some(BinaryOperatorKind::MULTIPLY),
            CoreToken::AND              => Some(BinaryOperatorKind::AND),
            CoreToken::OR               => Some(BinaryOperatorKind::OR),
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

    core_node_access!(CoreOkTokenNode);
}
impl Node for OkTokenNode {
    default_node_impl!(OkTokenNode);
    fn start_index(&self) -> usize {
        self.core_ref().token.start_index
    }
    fn end_index(&self) -> usize {
        self.core_ref().token.end_index
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().token.line_number
    }
}

#[derive(Debug, Clone)]
pub struct CoreMissingTokenNode {
    expected_symbols: Rc<Vec<&'static str>>,
    received_token: Token,
    lookahead: usize,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct MissingTokenNode(Rc<RefCell<CoreMissingTokenNode>>);
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

    core_node_access!(CoreMissingTokenNode);
}
impl Node for MissingTokenNode {
    default_node_impl!(MissingTokenNode);
    fn start_index(&self) -> usize {
        self.core_ref().received_token.start_index
    }
    fn end_index(&self) -> usize {
        self.core_ref().received_token.start_index
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().received_token.line_number
    }
}

#[derive(Debug, Clone)]
pub struct CoreSkippedTokenNode {
    skipped_token: Token,
    lookahead: usize,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct SkippedTokenNode(Rc<RefCell<CoreSkippedTokenNode>>);
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

    core_node_access!(CoreSkippedTokenNode);
}
impl Node for SkippedTokenNode {
    default_node_impl!(SkippedTokenNode);
    fn start_index(&self) -> usize {
        self.core_ref().skipped_token.start_index
    }
    fn end_index(&self) -> usize {
        self.core_ref().skipped_token.end_index
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().skipped_token.line_number
    }
}

#[derive(Debug, Clone)]
pub struct CoreExpressionNode {
    pub kind: ExpressionKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    UNARY(UnaryExpressionNode),
    BINARY(BinaryExpressionNode),
    COMPARISON(ComparisonNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct ExpressionNode(pub Rc<RefCell<CoreExpressionNode>>);
impl ExpressionNode {
    #[set_parent(EXPRESSION, WeakExpressionNode)]
    pub fn new_with_unary(unary_expr: &UnaryExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreExpressionNode {
            kind: ExpressionKind::UNARY(unary_expr.clone()),
            parent: None,
        }));
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
        let node = Rc::new(RefCell::new(CoreExpressionNode {
            kind: ExpressionKind::BINARY(BinaryExpressionNode::new(
                operator_kind,
                operator,
                left_expr,
                right_expr,
            )),
            parent: None,
        }));
        ExpressionNode(node)
    }

    pub fn new_with_comparison(operands: &Rc<Vec<ExpressionNode>>, operators: &Rc<Vec<TokenNode>>) -> Self {
        let node = Rc::new(RefCell::new(CoreExpressionNode {
            kind: ExpressionKind::COMPARISON(ComparisonNode::new(
                operands,
                operators
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

    core_node_access!(CoreExpressionNode);
}
impl Node for ExpressionNode {
    default_node_impl!(ExpressionNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().kind {
            ExpressionKind::UNARY(unary_expr) => unary_expr.start_index(),
            ExpressionKind::BINARY(binary_expr) => binary_expr.start_index(),
            ExpressionKind::COMPARISON(comp_expr) => comp_expr.start_index(),
            ExpressionKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().kind {
            ExpressionKind::UNARY(unary_expr) => unary_expr.end_index(),
            ExpressionKind::BINARY(binary_expr) => binary_expr.end_index(),
            ExpressionKind::COMPARISON(comp_expr) => comp_expr.end_index(),
            ExpressionKind::MISSING_TOKENS(missing_tokens) => missing_tokens.end_index(),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().kind {
            ExpressionKind::UNARY(unary_expr) => unary_expr.start_line_number(),
            ExpressionKind::BINARY(binary_expr) => binary_expr.start_line_number(),
            ExpressionKind::COMPARISON(comp_expr) => comp_expr.start_line_number(),
            ExpressionKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_line_number(),
        }
    }
}
default_errornous_node_impl!(ExpressionNode, CoreExpressionNode, ExpressionKind);

#[derive(Debug, Clone)]
pub struct CoreComparisonNode {
    pub operands: Rc<Vec<ExpressionNode>>,
    pub operators: Rc<Vec<TokenNode>>,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct ComparisonNode(pub Rc<RefCell<CoreComparisonNode>>);
impl ComparisonNode {
    pub fn new(operands: &Rc<Vec<ExpressionNode>>, operators: &Rc<Vec<TokenNode>>) -> Self {
        let node = Rc::new(RefCell::new(CoreComparisonNode{
            operands: operands.clone(),
            operators: operators.clone(),
            parent: None,
        }));
        for operand in operands.as_ref() {
            impl_set_parent!(operand, COMPARISON, node, WeakComparisonNode);
        }
        for operator in operators.as_ref() {
            impl_set_parent!(operator, COMPARISON, node, WeakComparisonNode);
        }
        ComparisonNode(node)
    }

    core_node_access!(CoreComparisonNode);
}
impl Node for ComparisonNode {
    default_node_impl!(ComparisonNode);
    fn start_index(&self) -> usize {
        self.core_ref().operands[0].start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().operands[self.core_ref().operands.len() - 1].end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().operands[0].start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreAtomicExpressionNode {
    pub kind: AtomicExpressionKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum AtomicExpressionKind {
    BOOL_VALUE(TokenNode),
    INTEGER(TokenNode),
    FLOATING_POINT_NUMBER(TokenNode),
    LITERAL(TokenNode),
    PARENTHESISED_EXPRESSION(ParenthesisedExpressionNode),
    ATOM(AtomNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct AtomicExpressionNode(Rc<RefCell<CoreAtomicExpressionNode>>);
impl AtomicExpressionNode {
    #[set_parent(ATOMIC_EXPRESSION, WeakAtomicExpressionNode)]
    pub fn new_with_bool(bool_value: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode {
            kind: AtomicExpressionKind::BOOL_VALUE(bool_value.clone()),
            parent: None,
        }));
        AtomicExpressionNode(node)
    }

    #[set_parent(ATOMIC_EXPRESSION, WeakAtomicExpressionNode)]
    pub fn new_with_integer(token: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode {
            kind: AtomicExpressionKind::INTEGER(token.clone()),
            parent: None,
        }));
        AtomicExpressionNode(node)
    }

    #[set_parent(ATOMIC_EXPRESSION, WeakAtomicExpressionNode)]
    pub fn new_with_floating_point_number(token: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode {
            kind: AtomicExpressionKind::FLOATING_POINT_NUMBER(token.clone()),
            parent: None,
        }));
        AtomicExpressionNode(node)
    }

    #[set_parent(ATOMIC_EXPRESSION, WeakAtomicExpressionNode)]
    pub fn new_with_literal(token: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode {
            kind: AtomicExpressionKind::LITERAL(token.clone()),
            parent: None,
        }));
        AtomicExpressionNode(node)
    }

    pub fn new_with_parenthesised_expr(
        expr: &ExpressionNode,
        lparen: &TokenNode,
        rparen: &TokenNode,
    ) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode {
            kind: AtomicExpressionKind::PARENTHESISED_EXPRESSION(ParenthesisedExpressionNode::new(
                expr, lparen, rparen,
            )),
            parent: None,
        }));
        AtomicExpressionNode(node)
    }

    #[set_parent(ATOMIC_EXPRESSION, WeakAtomicExpressionNode)]
    pub fn new_with_atom(atom: &AtomNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode {
            kind: AtomicExpressionKind::ATOM(atom.clone()),
            parent: None,
        }));
        AtomicExpressionNode(node)
    }

    core_node_access!(CoreAtomicExpressionNode);
}
impl Node for AtomicExpressionNode {
    default_node_impl!(AtomicExpressionNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().kind {
            AtomicExpressionKind::BOOL_VALUE(token) => token.start_index(),
            AtomicExpressionKind::INTEGER(token) => token.start_index(),
            AtomicExpressionKind::FLOATING_POINT_NUMBER(token) => token.start_index(),
            AtomicExpressionKind::LITERAL(token) => token.start_index(),
            AtomicExpressionKind::PARENTHESISED_EXPRESSION(parenthesised_expr) => parenthesised_expr.start_index(),
            AtomicExpressionKind::ATOM(atom) => atom.start_index(),
            AtomicExpressionKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().kind {
            AtomicExpressionKind::BOOL_VALUE(token) => token.end_index(),
            AtomicExpressionKind::INTEGER(token) => token.end_index(),
            AtomicExpressionKind::FLOATING_POINT_NUMBER(token) => token.end_index(),
            AtomicExpressionKind::LITERAL(token) => token.end_index(),
            AtomicExpressionKind::PARENTHESISED_EXPRESSION(parenthesised_expr) => parenthesised_expr.end_index(),
            AtomicExpressionKind::ATOM(atom) => atom.end_index(),
            AtomicExpressionKind::MISSING_TOKENS(missing_tokens) => missing_tokens.end_index(),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().kind {
            AtomicExpressionKind::BOOL_VALUE(token) => token.start_line_number(),
            AtomicExpressionKind::INTEGER(token) => token.start_line_number(),
            AtomicExpressionKind::FLOATING_POINT_NUMBER(token) => token.start_line_number(),
            AtomicExpressionKind::LITERAL(token) => token.start_line_number(),
            AtomicExpressionKind::PARENTHESISED_EXPRESSION(parenthesised_expr) => parenthesised_expr.start_line_number(),
            AtomicExpressionKind::ATOM(atom) => atom.start_line_number(),
            AtomicExpressionKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_line_number(),
        }
    }
}
default_errornous_node_impl!(
    AtomicExpressionNode,
    CoreAtomicExpressionNode,
    AtomicExpressionKind
);

#[derive(Debug, Clone)]
pub struct CoreParenthesisedExpressionNode {
    lparen: TokenNode,
    rparen: TokenNode,
    pub expr: ExpressionNode,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct ParenthesisedExpressionNode(Rc<RefCell<CoreParenthesisedExpressionNode>>);
impl ParenthesisedExpressionNode {
    #[set_parent(PARENTHESISED_EXPRESSION, WeakParenthesisedExpressionNode)]
    pub fn new(expr: &ExpressionNode, lparen: &TokenNode, rparen: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreParenthesisedExpressionNode {
            lparen: lparen.clone(),
            rparen: rparen.clone(),
            expr: expr.clone(),
            parent: None,
        }));
        ParenthesisedExpressionNode(node)
    }

    core_node_access!(CoreParenthesisedExpressionNode);
}
impl Node for ParenthesisedExpressionNode {
    default_node_impl!(ParenthesisedExpressionNode);
    fn start_index(&self) -> usize {
        self.core_ref().lparen.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().rparen.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().lparen.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreUnaryExpressionNode {
    pub kind: UnaryExpressionKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum UnaryOperatorKind {
    PLUS,
    MINUS,
    NOT,
}

#[derive(Debug, Clone)]
pub enum UnaryExpressionKind {
    ATOMIC(AtomicExpressionNode),
    UNARY(OnlyUnaryExpressionNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct UnaryExpressionNode(Rc<RefCell<CoreUnaryExpressionNode>>);
impl UnaryExpressionNode {
    #[set_parent(UNARY_EXPRESSION, WeakUnaryExpressionNode)]
    pub fn new_with_atomic(atomic_expr: &AtomicExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreUnaryExpressionNode {
            kind: UnaryExpressionKind::ATOMIC(atomic_expr.clone()),
            parent: None,
        }));
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

    core_node_access!(CoreUnaryExpressionNode);
}
impl Node for UnaryExpressionNode {
    default_node_impl!(UnaryExpressionNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().kind {
            UnaryExpressionKind::ATOMIC(atomic) => atomic.start_index(),
            UnaryExpressionKind::UNARY(only_unary) => only_unary.start_index(),
            UnaryExpressionKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().kind {
            UnaryExpressionKind::ATOMIC(atomic) => atomic.end_index(),
            UnaryExpressionKind::UNARY(only_unary) => only_unary.end_index(),
            UnaryExpressionKind::MISSING_TOKENS(missing_tokens) => missing_tokens.end_index(),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().kind {
            UnaryExpressionKind::ATOMIC(atomic) => atomic.start_line_number(),
            UnaryExpressionKind::UNARY(only_unary) => only_unary.start_line_number(),
            UnaryExpressionKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_line_number(),
        }
    }
}
default_errornous_node_impl!(
    UnaryExpressionNode,
    CoreUnaryExpressionNode,
    UnaryExpressionKind
);

#[derive(Debug, Clone)]
pub struct CoreOnlyUnaryExpressionNode {
    pub operator: TokenNode,
    pub unary_expr: UnaryExpressionNode,
    operator_kind: UnaryOperatorKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct OnlyUnaryExpressionNode(Rc<RefCell<CoreOnlyUnaryExpressionNode>>);
impl OnlyUnaryExpressionNode {
    #[set_parent(ONLY_UNARY_EXPRESSION, WeakOnlyUnaryExpressionNode)]
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
        OnlyUnaryExpressionNode(node)
    }

    core_node_access!(CoreOnlyUnaryExpressionNode);
}
impl Node for OnlyUnaryExpressionNode {
    default_node_impl!(OnlyUnaryExpressionNode);
    fn start_index(&self) -> usize {
        self.core_ref().operator.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().unary_expr.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().operator.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreBinaryExpressionNode {
    operator_kind: BinaryOperatorKind,
    pub operator: TokenNode,
    pub left_expr: ExpressionNode,
    pub right_expr: ExpressionNode,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct BinaryExpressionNode(Rc<RefCell<CoreBinaryExpressionNode>>);
impl BinaryExpressionNode {
    #[set_parent(BINARY_EXPRESSION, WeakBinaryExpressionNode)]
    pub fn new(
        operator_kind: BinaryOperatorKind,
        operator: &TokenNode,
        left_expr: &ExpressionNode,
        right_expr: &ExpressionNode,
    ) -> Self {
        let node = Rc::new(RefCell::new(CoreBinaryExpressionNode {
            operator_kind,
            operator: operator.clone(),
            left_expr: left_expr.clone(),
            right_expr: right_expr.clone(),
            parent: None,
        }));
        BinaryExpressionNode(node)
    }

    core_node_access!(CoreBinaryExpressionNode);
}
impl Node for BinaryExpressionNode {
    default_node_impl!(BinaryExpressionNode);
    fn start_index(&self) -> usize {
        self.core_ref().left_expr.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().right_expr.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().left_expr.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreParamsNode {
    pub kind: ParamsKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum ParamsKind {
    OK(OkParamsNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct ParamsNode(Rc<RefCell<CoreParamsNode>>);
impl ParamsNode {
    #[set_parent(PARAMS, WeakParamsNode)]
    pub fn new(ok_params_node: &OkParamsNode) -> Self {
        let node = Rc::new(RefCell::new(CoreParamsNode {
            kind: ParamsKind::OK(ok_params_node.clone()),
            parent: None,
        }));
        ParamsNode(node)
    }

    core_node_access!(CoreParamsNode);
}
impl Node for ParamsNode {
    default_node_impl!(ParamsNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().kind {
            ParamsKind::OK(ok_params) => ok_params.start_index(),
            ParamsKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().kind {
            ParamsKind::OK(ok_params) => ok_params.end_index(),
            ParamsKind::MISSING_TOKENS(missing_tokens) => missing_tokens.end_index(),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().kind {
            ParamsKind::OK(ok_params) => ok_params.start_line_number(),
            ParamsKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_line_number(),
        }
    }
}
default_errornous_node_impl!(ParamsNode, CoreParamsNode, ParamsKind);

#[derive(Debug, Clone)]
pub struct CoreOkParamsNode {
    comma: Option<TokenNode>,
    pub param: ExpressionNode,
    pub remaining_params: Option<ParamsNode>,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct OkParamsNode(Rc<RefCell<CoreOkParamsNode>>);
impl OkParamsNode {
    #[set_parent(OK_PARAMS, WeakOkParamsNode)]
    pub fn new_with_single_param(param: &ExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreOkParamsNode {
            comma: None,
            param: param.clone(),
            remaining_params: None,
            parent: None,
        }));
        OkParamsNode(node)
    }

    #[set_parent(OK_PARAMS, WeakOkParamsNode)]
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
        OkParamsNode(node)
    }

    core_node_access!(CoreOkParamsNode);
}
impl Node for OkParamsNode {
    default_node_impl!(OkParamsNode);
    fn start_index(&self) -> usize {
        self.core_ref().param.start_index()
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().remaining_params {
            Some(remaining_params) => remaining_params.end_index(),
            None => self.core_ref().param.end_index()
        }
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().param.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreCallExpressionNode {
    lparen: TokenNode,
    rparen: TokenNode,
    pub function_name: TokenNode,
    pub params: Option<ParamsNode>,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct CallExpressionNode(Rc<RefCell<CoreCallExpressionNode>>);
impl CallExpressionNode {
    #[set_parent(CALL_EXPRESSION, WeakCallExpressionNode)]
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
            params: extract_from_option!(params),
            parent: None,
        }));
        CallExpressionNode(node)
    }

    core_node_access!(CoreCallExpressionNode);
}
impl Node for CallExpressionNode {
    default_node_impl!(CallExpressionNode);
    fn start_index(&self) -> usize {
        self.core_ref().function_name.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().rparen.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().function_name.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreClassMethodCallNode {
    lparen: TokenNode,
    rparen: TokenNode,
    double_colon: TokenNode,
    pub class_name: TokenNode,
    pub class_method_name: TokenNode,
    pub params: Option<ParamsNode>,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct ClassMethodCallNode(Rc<RefCell<CoreClassMethodCallNode>>);
impl ClassMethodCallNode {
    #[set_parent(CLASS_METHOD_CALL, WeakClassMethodCallNode)]
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
            params: extract_from_option!(params),
            parent: None,
        }));
        ClassMethodCallNode(node)
    }

    core_node_access!(CoreClassMethodCallNode);
}
impl Node for ClassMethodCallNode {
    default_node_impl!(ClassMethodCallNode);
    fn start_index(&self) -> usize {
        self.core_ref().class_name.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().rparen.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().class_name.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreAtomNode {
    pub kind: AtomKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum AtomKind {
    ATOM_START(AtomStartNode),            // id, id(...), id::id(...)
    CALL(CallNode),                       // A(...)
    PROPERTRY_ACCESS(PropertyAccessNode), // A.id
    METHOD_ACCESS(MethodAccessNode),      // A.id(...)
    INDEX_ACCESS(IndexAccessNode),        // A[<expr>]
}

#[derive(Debug, Clone)]
pub struct AtomNode(Rc<RefCell<CoreAtomNode>>);
impl AtomNode {
    #[set_parent(ATOM, WeakAtomNode)]
    pub fn new_with_atom_start(atom_start: &AtomStartNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomNode {
            kind: AtomKind::ATOM_START(atom_start.clone()),
            parent: None,
        }));
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

    core_node_access!(CoreAtomNode);
}
impl Node for AtomNode {
    default_node_impl!(AtomNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().kind {
            AtomKind::ATOM_START(atom_start) => atom_start.start_index(),
            AtomKind::CALL(call) => call.start_index(),
            AtomKind::PROPERTRY_ACCESS(property_access) => property_access.start_index(),
            AtomKind::METHOD_ACCESS(method_access) => method_access.start_index(),
            AtomKind::INDEX_ACCESS(index_access) => index_access.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().kind {
            AtomKind::ATOM_START(atom_start) => atom_start.end_index(),
            AtomKind::CALL(call) => call.end_index(),
            AtomKind::PROPERTRY_ACCESS(property_access) => property_access.end_index(),
            AtomKind::METHOD_ACCESS(method_access) => method_access.end_index(),
            AtomKind::INDEX_ACCESS(index_access) => index_access.end_index(),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().kind {
            AtomKind::ATOM_START(atom_start) => atom_start.start_line_number(),
            AtomKind::CALL(call) => call.start_line_number(),
            AtomKind::PROPERTRY_ACCESS(property_access) => property_access.start_line_number(),
            AtomKind::METHOD_ACCESS(method_access) => method_access.start_line_number(),
            AtomKind::INDEX_ACCESS(index_access) => index_access.start_line_number(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CoreAtomStartNode {
    pub kind: AtomStartKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum AtomStartKind {
    IDENTIFIER(TokenNode),                  // id
    FUNCTION_CALL(CallExpressionNode),      // id(...)
    CLASS_METHOD_CALL(ClassMethodCallNode), // id::id(...)
}

#[derive(Debug, Clone)]
pub struct AtomStartNode(Rc<RefCell<CoreAtomStartNode>>);
impl AtomStartNode {
    #[set_parent(ATOM_START, WeakAtomStartNode)]
    pub fn new_with_identifier(token: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomStartNode {
            kind: AtomStartKind::IDENTIFIER(token.clone()),
            parent: None,
        }));
        AtomStartNode(node)
    }

    #[set_parent(ATOM_START, WeakAtomStartNode)]
    pub fn new_with_function_call(call_expr: &CallExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomStartNode {
            kind: AtomStartKind::FUNCTION_CALL(call_expr.clone()),
            parent: None,
        }));
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

    core_node_access!(CoreAtomStartNode);
}
impl Node for AtomStartNode {
    default_node_impl!(AtomStartNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().kind {
            AtomStartKind::IDENTIFIER(token) => token.start_index(),
            AtomStartKind::FUNCTION_CALL(function_call) => function_call.start_index(),
            AtomStartKind::CLASS_METHOD_CALL(class_method_call) => class_method_call.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().kind {
            AtomStartKind::IDENTIFIER(token) => token.end_index(),
            AtomStartKind::FUNCTION_CALL(function_call) => function_call.end_index(),
            AtomStartKind::CLASS_METHOD_CALL(class_method_call) => class_method_call.end_index(),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().kind {
            AtomStartKind::IDENTIFIER(token) => token.start_line_number(),
            AtomStartKind::FUNCTION_CALL(function_call) => function_call.start_line_number(),
            AtomStartKind::CLASS_METHOD_CALL(class_method_call) => class_method_call.start_line_number(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CoreCallNode {
    pub atom: AtomNode,
    lparen: TokenNode,
    rparen: TokenNode,
    pub params: Option<ParamsNode>,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct CallNode(Rc<RefCell<CoreCallNode>>);
impl CallNode {
    #[set_parent(CALL, WeakCallNode)]
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
            params: extract_from_option!(params),
            parent: None,
        }));
        CallNode(node)
    }

    core_node_access!(CoreCallNode);
}
impl Node for CallNode {
    default_node_impl!(CallNode);
    fn start_index(&self) -> usize {
        self.core_ref().atom.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().rparen.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().atom.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CorePropertyAccessNode {
    dot: TokenNode,
    pub atom: AtomNode,
    pub propertry: TokenNode,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct PropertyAccessNode(Rc<RefCell<CorePropertyAccessNode>>);
impl PropertyAccessNode {
    #[set_parent(PROPERTY_ACCESS, WeakPropertyAccessNode)]
    fn new(atom: &AtomNode, propertry: &TokenNode, dot: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CorePropertyAccessNode {
            dot: dot.clone(),
            atom: atom.clone(),
            propertry: propertry.clone(),
            parent: None,
        }));
        PropertyAccessNode(node)
    }

    core_node_access!(CorePropertyAccessNode);
}
impl Node for PropertyAccessNode {
    default_node_impl!(PropertyAccessNode);
    fn start_index(&self) -> usize {
        self.core_ref().atom.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().propertry.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().atom.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreMethodAccessNode {
    lparen: TokenNode,
    rparen: TokenNode,
    dot: TokenNode,
    pub atom: AtomNode,
    pub method_name: TokenNode,
    pub params: Option<ParamsNode>,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct MethodAccessNode(Rc<RefCell<CoreMethodAccessNode>>);
impl MethodAccessNode {
    #[set_parent(METHOD_ACCESS, WeakMethodAccessNode)]
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
            params: extract_from_option!(params),
            parent: None,
        }));
        MethodAccessNode(node)
    }

    core_node_access!(CoreMethodAccessNode);
}
impl Node for MethodAccessNode {
    default_node_impl!(MethodAccessNode);
    fn start_index(&self) -> usize {
        self.core_ref().atom.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().rparen.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().atom.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreIndexAccessNode {
    lsquare: TokenNode,
    rsquare: TokenNode,
    pub atom: AtomNode,
    pub index: ExpressionNode,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub struct IndexAccessNode(Rc<RefCell<CoreIndexAccessNode>>);
impl IndexAccessNode {
    #[set_parent(INDEX_ACCESS, WeakIndexAccessNode)]
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
        IndexAccessNode(node)
    }

    core_node_access!(CoreIndexAccessNode);
}
impl Node for IndexAccessNode {
    default_node_impl!(IndexAccessNode);
    fn start_index(&self) -> usize {
        self.core_ref().atom.start_index()
    }
    fn end_index(&self) -> usize {
        self.core_ref().rsquare.end_index()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().atom.start_line_number()
    }
}

#[derive(Debug, Clone)]
pub struct CoreRAssignmentNode {
    pub kind: RAssignmentKind,
    parent: Option<WeakASTNode>,
}

#[derive(Debug, Clone)]
pub enum RAssignmentKind {
    LAMBDA(FunctionDeclarationNode),
    EXPRESSION((ExpressionNode, TokenNode)),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct RAssignmentNode(Rc<RefCell<CoreRAssignmentNode>>);
impl RAssignmentNode {
    #[set_parent(R_ASSIGNMENT, WeakRAssignmentNode)]
    pub fn new_with_lambda(lambda_decl: &FunctionDeclarationNode) -> Self {
        let node = Rc::new(RefCell::new(CoreRAssignmentNode {
            kind: RAssignmentKind::LAMBDA(lambda_decl.clone()),
            parent: None,
        }));
        RAssignmentNode(node)
    }

    #[set_parent(R_ASSIGNMENT, WeakRAssignmentNode)]
    pub fn new_with_expr(expr: &ExpressionNode, newline: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreRAssignmentNode {
            kind: RAssignmentKind::EXPRESSION((expr.clone(), newline.clone())),
            parent: None,
        }));
        RAssignmentNode(node)
    }

    core_node_access!(CoreRAssignmentNode);
}
impl Node for RAssignmentNode {
    default_node_impl!(RAssignmentNode);
    fn start_index(&self) -> usize {
        match &self.core_ref().kind {
            RAssignmentKind::LAMBDA(func_decl) => func_decl.start_index(),
            RAssignmentKind::EXPRESSION((expr, _)) => expr.start_index(),
            RAssignmentKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_index(),
        }
    }
    fn end_index(&self) -> usize {
        match &self.core_ref().kind {
            RAssignmentKind::LAMBDA(func_decl) => func_decl.end_index(),
            RAssignmentKind::EXPRESSION((_, newline)) => newline.end_index(),
            RAssignmentKind::MISSING_TOKENS(missing_tokens) => missing_tokens.end_index(),
        }
    }
    fn start_line_number(&self) -> usize {
        match &self.core_ref().kind {
            RAssignmentKind::LAMBDA(func_decl) => func_decl.start_line_number(),
            RAssignmentKind::EXPRESSION((expr, _)) => expr.start_line_number(),
            RAssignmentKind::MISSING_TOKENS(missing_tokens) => missing_tokens.start_line_number(),
        }
    }
}
default_errornous_node_impl!(RAssignmentNode, CoreRAssignmentNode, RAssignmentKind);
