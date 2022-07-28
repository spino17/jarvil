// AST Nodes have inner mutability to enable dynamic changes to AST like monomorphism of generics or macro expansion.
// ASTNode has weak reference to core nodes to avoid memory leaks. 
// See `https://doc.rust-lang.org/book/ch15-06-reference-cycles.html` for more information

use std::{rc::{Rc, Weak}, cell::RefCell};
use crate::{scope::{core::Scope, self}, lexer::token::{Token, CoreToken}, types::{core::Type, array::Array}, code::Code};
use crate::types::atomic::Atomic;

pub trait Node {
    fn set_parent(&self, parent_node: ASTNode);
    // fn width(&self) -> usize;  // TODO - width of the node
}

pub trait ErrornousNode {
    fn new_with_missing_tokens(expected_symbols: &Rc<Vec<&'static str>>, received_token: &Token, lookahead: usize) -> Self;
}

macro_rules! default_node_impl {
    ($t: ident) => {
        impl Node for $t {
            fn set_parent(&self, parent_node: ASTNode) {
                self.0.as_ref().borrow_mut().parent = Some(parent_node);
            }
        }
    };
}

macro_rules! default_errornous_node_impl {
    ($t: ident, $u: ident, $v: ident) => {
        impl ErrornousNode for $t {
            fn new_with_missing_tokens(expected_symbols: &Rc<Vec<&'static str>>, 
            received_token: &Token, lookahead: usize) -> Self {
                $t(Rc::new(RefCell::new($u{
                    kind: $v::MISSING_TOKENS(MissingTokenNode::new(
                        expected_symbols, received_token, lookahead
                    )),
                    parent: None,
                })))
            }
        }
    };
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    BLOCK(Weak<RefCell<CoreBlockNode>>),
    STATEMENT(Weak<RefCell<CoreStatementNode>>),
    ASSIGNMENT(Weak<RefCell<CoreAssignmentNode>>),
    STRUCT_STATEMENT(Weak<RefCell<CoreStructStatementNode>>),
    NAME_TYPE_SPECS(Weak<RefCell<CoreNameTypeSpecsNode>>),
    NAME_TYPE_SPEC(Weak<RefCell<CoreNameTypeSpecNode>>),
    TYPE_EXPRESSION(Weak<RefCell<CoreTypeExpressionNode>>),
    ATOMIC_TYPE(Weak<RefCell<CoreAtomicTypeNode>>),
    ARRAY_TYPE(Weak<RefCell<CoreArrayTypeNode>>),
    USER_DEFINED_TYPE(Weak<RefCell<CoreUserDefinedTypeNode>>),
    SKIPPED_TOKENS(Weak<RefCell<CoreSkippedTokens>>),
    EXPRESSION(Weak<RefCell<CoreExpressionNode>>),
    ATOMIC_EXPRESSION(Weak<RefCell<CoreAtomicExpressionNode>>),
    PARENTHESISED_EXPRESSION(Weak<RefCell<CoreParenthesisedExpressionNode>>),
    UNARY_EXPRESSION(Weak<RefCell<CoreUnaryExpressionNode>>),
    ONLY_UNARY_EXPRESSION(Weak<RefCell<CoreOnlyUnaryExpressionNode>>),
    BINARY_EXPRESSION(Weak<RefCell<CoreBinaryExpressionNode>>),
    PARAMS(Weak<RefCell<CoreParamsNode>>),
    OK_PARAMS(Weak<RefCell<CoreOkParamsNode>>),
    CLASS_METHOD_CALL(Weak<RefCell<CoreClassMethodCallNode>>),
    CALL_EXPRESSION(Weak<RefCell<CoreCallExpressionNode>>),
    ATOM(Weak<RefCell<CoreAtomNode>>),
    PROPERTY_ACCESS(Weak<RefCell<CorePropertyAccessNode>>),
    METHOD_ACCESS(Weak<RefCell<CoreMethodAccessNode>>),
    INDEX_ACCESS(Weak<RefCell<CoreIndexAccessNode>>),
    ATOM_START(Weak<RefCell<CoreAtomStartNode>>),
    VARIABLE_DECLARATION(Weak<RefCell<CoreVariableDeclarationNode>>),
    FUNCTION_DECLARATION(Weak<RefCell<CoreFunctionDeclarationNode>>),
    STRUCT_DECLARATION(Weak<RefCell<CoreStructDeclarationNode>>),
    TYPE_DECLARATION(Weak<RefCell<CoreTypeDeclarationNode>>),
    OK_FUNCTION_DECLARATION(Weak<RefCell<CoreOkFunctionDeclarationNode>>),
    OK_LAMDA_DECLARATION(Weak<RefCell<CoreOkLambdaDeclarationNode>>),
    OK_NAME_TYPE_SPECS(Weak<RefCell<CoreOkNameTypeSpecsNode>>),
    R_ASSIGNMENT(Weak<RefCell<CoreRAssignmentNode>>),
}

#[derive(Debug, Clone)]
pub enum StatemenIndentWrapper {
    CORRECTLY_INDENTED(StatementNode),
    INCORRECTLY_INDENTED((StatementNode, (i64, i64))),
    LEADING_SKIPPED_TOKENS(SkippedTokens),      // skipped tokens leading to the next stmt in block
    TRAILING_SKIPPED_TOKENS(SkippedTokens),     // skipped tokens trailing to the previous stmt in block
    EXTRA_NEWLINES(SkippedTokens),
}

#[derive(Debug, Clone)]
pub struct CoreBlockNode {
    newline: TokenNode,
    pub stmts: Rc<RefCell<Vec<StatemenIndentWrapper>>>,
    scope: Option<Scope>,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct BlockNode(pub Rc<RefCell<CoreBlockNode>>);
impl BlockNode {
    pub fn new(stmts: &Rc<RefCell<Vec<StatemenIndentWrapper>>>, newline: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreBlockNode{
            newline: newline.clone(),
            stmts: stmts.clone(),
            scope: None,
            parent: None,
        }));
        newline.set_parent(ASTNode::BLOCK(Rc::downgrade(&node)));
        for stmt in &*stmts.as_ref().borrow() {
            match stmt {
                StatemenIndentWrapper::CORRECTLY_INDENTED(correct_indented_stmt) => {
                    correct_indented_stmt.set_parent(ASTNode::BLOCK(Rc::downgrade(&node)));
                }
                StatemenIndentWrapper::INCORRECTLY_INDENTED((incorrect_indented_stmt, _)) => {
                    incorrect_indented_stmt.set_parent(ASTNode::BLOCK(Rc::downgrade(&node)));
                }
                StatemenIndentWrapper::LEADING_SKIPPED_TOKENS(leading_skipped_tokens) => {
                    leading_skipped_tokens.set_parent(ASTNode::BLOCK(Rc::downgrade(&node)));
                }
                StatemenIndentWrapper::TRAILING_SKIPPED_TOKENS(trailing_skipped_tokens) => {
                    trailing_skipped_tokens.set_parent(ASTNode::BLOCK(Rc::downgrade(&node)));
                }
                StatemenIndentWrapper::EXTRA_NEWLINES(extra_newlines) => {
                    extra_newlines.set_parent(ASTNode::BLOCK(Rc::downgrade(&node)));
                },
            }
        }
        BlockNode(node)
    }

    pub fn set_scope(&self, scope: &Scope) {
        self.0.as_ref().borrow_mut().scope = Some(scope.clone());
    }
}
default_node_impl!(BlockNode);

#[derive(Debug, Clone)]
pub struct CoreSkippedTokens {
    skipped_tokens: Rc<Vec<SkippedTokenNode>>,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct SkippedTokens(Rc<RefCell<CoreSkippedTokens>>);
impl SkippedTokens {
    pub fn new_with_leading_skipped_tokens(skipped_tokens: &Rc<Vec<SkippedTokenNode>>) -> Self {
        let node = Rc::new(RefCell::new(CoreSkippedTokens{
            skipped_tokens: skipped_tokens.clone(),
            parent: None,
        }));
        for skipped_token in skipped_tokens.as_ref() {
            skipped_token.set_parent(ASTNode::SKIPPED_TOKENS(Rc::downgrade(&node)));
        }
        SkippedTokens(node)
    }

    pub fn new_with_trailing_skipped_tokens(skipped_tokens: &Rc<Vec<SkippedTokenNode>>) -> Self {
        let node = Rc::new(RefCell::new(CoreSkippedTokens{
            skipped_tokens: skipped_tokens.clone(),
            parent: None,
        }));
        for skipped_token in skipped_tokens.as_ref() {
            skipped_token.set_parent(ASTNode::SKIPPED_TOKENS(Rc::downgrade(&node)));
        }
        SkippedTokens(node)
    }

    pub fn new_with_extra_newlines(extra_newlines: &Rc<Vec<SkippedTokenNode>>) -> Self {
        let node = Rc::new(RefCell::new(CoreSkippedTokens{
            skipped_tokens: extra_newlines.clone(),
            parent: None,
        }));
        for skipped_token in extra_newlines.as_ref() {
            skipped_token.set_parent(ASTNode::SKIPPED_TOKENS(Rc::downgrade(&node)));
        }
        SkippedTokens(node)
    }
}
default_node_impl!(SkippedTokens);

#[derive(Debug, Clone)]
pub struct CoreStatementNode {
    pub kind: StatementNodeKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum StatementNodeKind {
    // expr, variable declaration, type struct declaration, type lambda declaration, interface declaration, 
    // assignment, if, for, while, return, continue, break, implementation of interfaces, implementation of structs
    EXPRESSION(ExpressionNode),
    VARIABLE_DECLARATION(VariableDeclarationNode),
    FUNCTION_DECLARATION(FunctionDeclarationNode),
    TYPE_DECLARATION(TypeDeclarationNode),
    STRUCT_STATEMENT(StructStatementNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct StatementNode(pub Rc<RefCell<CoreStatementNode>>);
impl StatementNode {
    pub fn new_with_expression(expr: &ExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStatementNode{
            kind: StatementNodeKind::EXPRESSION(expr.clone()),
            parent: None,
        }));
        expr.set_parent(ASTNode::STATEMENT(Rc::downgrade(&node)));
        StatementNode(node)
    }

    pub fn new_with_variable_declaration(variable_decl: &VariableDeclarationNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStatementNode{
            kind: StatementNodeKind::VARIABLE_DECLARATION(variable_decl.clone()),
            parent: None,
        }));
        variable_decl.set_parent(ASTNode::STATEMENT(Rc::downgrade(&node)));
        StatementNode(node)
    }

    pub fn new_with_function_declaration(function_decl: &FunctionDeclarationNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStatementNode{
            kind: StatementNodeKind::FUNCTION_DECLARATION(function_decl.clone()),
            parent: None,
        }));
        function_decl.set_parent(ASTNode::STATEMENT(Rc::downgrade(&node)));
        StatementNode(node)
    }

    pub fn new_with_type_declaration(type_decl: &TypeDeclarationNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStatementNode{
            kind: StatementNodeKind::TYPE_DECLARATION(type_decl.clone()),
            parent: None,
        }));
        type_decl.set_parent(ASTNode::STATEMENT(Rc::downgrade(&node)));
        StatementNode(node)
    }

    pub fn new_with_struct_stmt(struct_stmt: &StructStatementNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStatementNode{
            kind: StatementNodeKind::STRUCT_STATEMENT(struct_stmt.clone()),
            parent: None,
        }));
        struct_stmt.set_parent(ASTNode::STATEMENT(Rc::downgrade(&node)));
        StatementNode(node)
    }
}
default_node_impl!(StatementNode);
default_errornous_node_impl!(StatementNode, CoreStatementNode, StatementNodeKind);

#[derive(Debug, Clone)]
pub struct CoreAssignmentNode {
    equal: TokenNode,
    l_atom: AtomNode,
    r_assign: RAssignmentNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct AssignmentNode(Rc<RefCell<CoreAssignmentNode>>);
impl AssignmentNode {
    pub fn new(l_atom: &AtomNode, r_assign: &RAssignmentNode, equal: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAssignmentNode{
            equal: equal.clone(),
            l_atom: l_atom.clone(),
            r_assign: r_assign.clone(),
            parent: None,
        }));
        equal.set_parent(ASTNode::ASSIGNMENT(Rc::downgrade(&node)));
        l_atom.set_parent(ASTNode::ASSIGNMENT(Rc::downgrade(&node)));
        r_assign.set_parent(ASTNode::ASSIGNMENT(Rc::downgrade(&node)));
        AssignmentNode(node)
    }
}
default_node_impl!(AssignmentNode);

#[derive(Debug, Clone)]
pub struct CoreStructStatementNode {
    newline: TokenNode,
    name_type_spec: NameTypeSpecNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct StructStatementNode(Rc<RefCell<CoreStructStatementNode>>);
impl StructStatementNode {
    pub fn new(param_name: &TokenNode, param_type: &TypeExpressionNode, colon: &TokenNode, newline: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStructStatementNode{
            newline: newline.clone(),
            name_type_spec: NameTypeSpecNode::new(param_name, param_type, colon),
            parent: None,
        }));
        StructStatementNode(node)
    }
}
default_node_impl!(StructStatementNode);

#[derive(Debug, Clone)]
pub struct CoreTypeDeclarationNode {
    kind: TypeDeclarationKind,
    parent: Option<ASTNode>,
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
    pub fn new_with_struct(name: &TokenNode, block: &BlockNode, type_keyword: &TokenNode, colon: &TokenNode) -> Self {
        TypeDeclarationNode(Rc::new(RefCell::new(CoreTypeDeclarationNode{
            kind: TypeDeclarationKind::STRUCT(StructDeclarationNode::new(name, block, type_keyword, colon)),
            parent: None,
        })))
    }

    pub fn new_with_lambda(lambda: &LambdaDeclarationNode) -> Self {
        let node = Rc::new(RefCell::new(CoreTypeDeclarationNode{
            kind: TypeDeclarationKind::LAMBDA(lambda.clone()),
            parent: None,
        }));
        lambda.set_parent(ASTNode::TYPE_DECLARATION(Rc::downgrade(&node)));
        TypeDeclarationNode(node)
    }
}
default_node_impl!(TypeDeclarationNode);
default_errornous_node_impl!(TypeDeclarationNode, CoreTypeDeclarationNode, TypeDeclarationKind);

#[derive(Debug, Clone)]
pub struct CoreStructDeclarationNode {
    type_keyword: TokenNode,
    colon: TokenNode,
    name: TokenNode,
    block: BlockNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct StructDeclarationNode(Rc<RefCell<CoreStructDeclarationNode>>);
impl StructDeclarationNode {
    pub fn new(name: &TokenNode, block: &BlockNode, type_keyword: &TokenNode, colon: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreStructDeclarationNode{
            type_keyword: type_keyword.clone(),
            colon: colon.clone(),
            name: name.clone(),
            block: block.clone(),
            parent: None,
        }));
        type_keyword.set_parent(ASTNode::STRUCT_DECLARATION(Rc::downgrade(&node)));
        colon.set_parent(ASTNode::STRUCT_DECLARATION(Rc::downgrade(&node)));
        name.set_parent(ASTNode::STRUCT_DECLARATION(Rc::downgrade(&node)));
        block.set_parent(ASTNode::STRUCT_DECLARATION(Rc::downgrade(&node)));
        StructDeclarationNode(node)
    }
}
default_node_impl!(StructDeclarationNode);

#[derive(Debug, Clone)]
pub struct CoreLambdaDeclarationNode {
    kind: LambdaDeclarationKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum LambdaDeclarationKind {
    OK(OkLambdaDeclarationNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct LambdaDeclarationNode(Rc<RefCell<CoreLambdaDeclarationNode>>);
impl LambdaDeclarationNode {
    pub fn new(name: &TokenNode, args: &Option<NameTypeSpecsNode>, 
        return_type: &Option<TypeExpressionNode>, type_keyword: &TokenNode, colon: &TokenNode,
    lparen: &TokenNode, rparen: &TokenNode, right_arrow: &Option<TokenNode>, newline: &TokenNode) -> Self {
        LambdaDeclarationNode(Rc::new(RefCell::new(CoreLambdaDeclarationNode{
            kind: LambdaDeclarationKind::OK(OkLambdaDeclarationNode::new(
                name, args, return_type, type_keyword, colon, lparen, rparen, right_arrow, newline
            )),
            parent: None,
        })))
    }
}
default_node_impl!(LambdaDeclarationNode);
default_errornous_node_impl!(LambdaDeclarationNode, CoreLambdaDeclarationNode, LambdaDeclarationKind);

#[derive(Debug, Clone)]
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
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct OkLambdaDeclarationNode(Rc<RefCell<CoreOkLambdaDeclarationNode>>);
impl OkLambdaDeclarationNode {
    pub fn new(name: &TokenNode, args: &Option<NameTypeSpecsNode>, 
        return_type: &Option<TypeExpressionNode>, type_keyword: &TokenNode, colon: &TokenNode,
        lparen: &TokenNode, rparen: &TokenNode, right_arrow: &Option<TokenNode>, newline: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreOkLambdaDeclarationNode{
            lparen: lparen.clone(),
            rparen: rparen.clone(),
            right_arrow: right_arrow.clone(),
            newline: newline.clone(),
            type_keyword: type_keyword.clone(),
            colon: colon.clone(),
            name: name.clone(),
            args: args.clone(),
            return_type: return_type.clone(),
            parent: None,
        }));
        lparen.set_parent(ASTNode::OK_LAMDA_DECLARATION(Rc::downgrade(&node)));
        rparen.set_parent(ASTNode::OK_LAMDA_DECLARATION(Rc::downgrade(&node)));
        newline.set_parent(ASTNode::OK_LAMDA_DECLARATION(Rc::downgrade(&node)));
        type_keyword.set_parent(ASTNode::OK_LAMDA_DECLARATION(Rc::downgrade(&node)));
        colon.set_parent(ASTNode::OK_LAMDA_DECLARATION(Rc::downgrade(&node)));
        name.set_parent(ASTNode::OK_LAMDA_DECLARATION(Rc::downgrade(&node)));
        match args {
            Some(args) => args.set_parent(
                ASTNode::OK_LAMDA_DECLARATION(Rc::downgrade(&node))
            ),
            None => {},
        }
        match right_arrow {
            Some(right_arrow) => right_arrow.set_parent(
                ASTNode::OK_LAMDA_DECLARATION(Rc::downgrade(&node))
            ),
            None => {},

        }
        match return_type {
            Some(return_type) => return_type.set_parent(
                ASTNode::OK_LAMDA_DECLARATION(Rc::downgrade(&node))
            ),
            None => {},
        }
        OkLambdaDeclarationNode(node)
    }
}
default_node_impl!(OkLambdaDeclarationNode);

#[derive(Debug, Clone)]
pub struct CoreFunctionDeclarationNode {
    pub kind: FunctionDeclarationKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum FunctionDeclarationKind {
    OK(OkFunctionDeclarationNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct FunctionDeclarationNode(pub Rc<RefCell<CoreFunctionDeclarationNode>>);
impl FunctionDeclarationNode {
    pub fn new(name: &Option<TokenNode>, args: &Option<NameTypeSpecsNode>, 
        return_type: &Option<TypeExpressionNode>, block: &BlockNode, func_keyword: &FuncKeywordKind, 
        lparen: &TokenNode, rparen: &TokenNode, right_arrow: &Option<TokenNode>, colon: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreFunctionDeclarationNode{
            kind: FunctionDeclarationKind::OK(OkFunctionDeclarationNode::new(
                name, args, return_type, block, func_keyword, lparen, rparen, right_arrow, colon
            )),
            parent: None,
        }));
        FunctionDeclarationNode(node)
    }
}
default_node_impl!(FunctionDeclarationNode);
default_errornous_node_impl!(FunctionDeclarationNode, CoreFunctionDeclarationNode, FunctionDeclarationKind);

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
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum FuncKeywordKind {
    DEF(TokenNode),
    FUNC(TokenNode),
}

#[derive(Debug, Clone)]
pub struct OkFunctionDeclarationNode(pub Rc<RefCell<CoreOkFunctionDeclarationNode>>);
impl OkFunctionDeclarationNode {
    pub fn new(name: &Option<TokenNode>, args: &Option<NameTypeSpecsNode>, 
        return_type: &Option<TypeExpressionNode>, block: &BlockNode, func_keyword: &FuncKeywordKind, 
        lparen: &TokenNode, rparen: &TokenNode, right_arrow: &Option<TokenNode>, colon: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreOkFunctionDeclarationNode{
            func_keyword: func_keyword.clone(),
            lparen: lparen.clone(),
            rparen: rparen.clone(),
            right_arrow: right_arrow.clone(),
            colon: colon.clone(),
            name: name.clone(),
            args: args.clone(),
            return_type: return_type.clone(),
            block: block.clone(),
            parent: None,
        }));
        lparen.set_parent(ASTNode::OK_FUNCTION_DECLARATION(Rc::downgrade(&node)));
        rparen.set_parent(ASTNode::OK_FUNCTION_DECLARATION(Rc::downgrade(&node)));
        colon.set_parent(ASTNode::OK_FUNCTION_DECLARATION(Rc::downgrade(&node)));
        match func_keyword {
            FuncKeywordKind::DEF(def_node) => def_node.set_parent(ASTNode::OK_FUNCTION_DECLARATION(Rc::downgrade(&node))),
            FuncKeywordKind::FUNC(func_node) => func_node.set_parent(ASTNode::OK_FUNCTION_DECLARATION(Rc::downgrade(&node))),
        }
        match right_arrow {
            Some(right_arrow) => right_arrow.set_parent(ASTNode::OK_FUNCTION_DECLARATION(Rc::downgrade(&node))),
            None => {},
        }
        match name {
            Some(name) => name.set_parent(ASTNode::OK_FUNCTION_DECLARATION(Rc::downgrade(&node))),
            None => {},
        }
        match args {
            Some(args) => args.set_parent(
                ASTNode::OK_FUNCTION_DECLARATION(Rc::downgrade(&node))
            ),
            None => {},
        }
        match return_type {
            Some(return_type) => return_type.set_parent(
                ASTNode::OK_FUNCTION_DECLARATION(Rc::downgrade(&node))
            ),
            None => {},
        }
        block.set_parent(ASTNode::OK_FUNCTION_DECLARATION(Rc::downgrade(&node)));
        OkFunctionDeclarationNode(node)
    }
}
default_node_impl!(OkFunctionDeclarationNode);

#[derive(Debug, Clone)]
pub struct CoreVariableDeclarationNode {
    let_keyword: TokenNode,
    equal: TokenNode,
    newline: TokenNode,
    name: TokenNode,
    r_assign: RAssignmentNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct VariableDeclarationNode(Rc<RefCell<CoreVariableDeclarationNode>>);
impl VariableDeclarationNode {
    pub fn new(name: &TokenNode, 
        r_assign: &RAssignmentNode, let_keyword: &TokenNode, equal: &TokenNode, newline: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreVariableDeclarationNode{
            let_keyword: let_keyword.clone(),
            equal: equal.clone(),
            newline: newline.clone(),
            name: name.clone(),
            r_assign: r_assign.clone(),
            parent: None,
        }));
        let_keyword.set_parent(ASTNode::VARIABLE_DECLARATION(Rc::downgrade(&node)));
        equal.set_parent(ASTNode::VARIABLE_DECLARATION(Rc::downgrade(&node)));
        newline.set_parent(ASTNode::VARIABLE_DECLARATION(Rc::downgrade(&node)));
        name.set_parent(ASTNode::VARIABLE_DECLARATION(Rc::downgrade(&node)));
        r_assign.set_parent(ASTNode::VARIABLE_DECLARATION(Rc::downgrade(&node)));
        VariableDeclarationNode(node)
    }
}
default_node_impl!(VariableDeclarationNode);

#[derive(Debug, Clone)]
pub struct CoreNameTypeSpecsNode {
    kind: NameTypeSpecsKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum NameTypeSpecsKind {
    OK(OkNameTypeSpecsNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct NameTypeSpecsNode(Rc<RefCell<CoreNameTypeSpecsNode>>);
impl NameTypeSpecsNode {
    pub fn new(ok_name_type_specs: &OkNameTypeSpecsNode) -> Self {
        let node = Rc::new(RefCell::new(CoreNameTypeSpecsNode{
            kind: NameTypeSpecsKind::OK(ok_name_type_specs.clone()),
            parent: None,
        }));
        ok_name_type_specs.set_parent(ASTNode::NAME_TYPE_SPECS(Rc::downgrade(&node)));
        NameTypeSpecsNode(node)
    }

    pub fn get_name_type_spec_objs(&self, code: &Code) -> Vec<(Option<Rc<String>>, Option<Type>)> {
        match &self.0.as_ref().borrow().kind {
            NameTypeSpecsKind::OK(ok_name_type_specs) => ok_name_type_specs.get_name_type_spec_objs(code),
            _ => vec![],
        }
    }
}
default_node_impl!(NameTypeSpecsNode);
default_errornous_node_impl!(NameTypeSpecsNode, CoreNameTypeSpecsNode, NameTypeSpecsKind);

#[derive(Debug, Clone)]
pub struct CoreOkNameTypeSpecsNode {
    comma: Option<TokenNode>,
    arg: NameTypeSpecNode,
    remaining_args: Option<NameTypeSpecsNode>,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct OkNameTypeSpecsNode(Rc<RefCell<CoreOkNameTypeSpecsNode>>);
impl OkNameTypeSpecsNode {
    pub fn new_with_args(arg: &NameTypeSpecNode, remaining_args: &NameTypeSpecsNode, comma: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreOkNameTypeSpecsNode{
            comma: Some(comma.clone()),
            arg: arg.clone(),
            remaining_args: Some(remaining_args.clone()),
            parent: None,
        }));
        comma.set_parent(ASTNode::OK_NAME_TYPE_SPECS(Rc::downgrade(&node)));
        arg.set_parent(ASTNode::OK_NAME_TYPE_SPECS(Rc::downgrade(&node)));
        remaining_args.set_parent(ASTNode::OK_NAME_TYPE_SPECS(Rc::downgrade(&node)));
        OkNameTypeSpecsNode(node)
    }

    pub fn new_with_single_arg(arg: &NameTypeSpecNode) -> Self {
        let node = Rc::new(RefCell::new(CoreOkNameTypeSpecsNode{
            comma: None,
            arg: arg.clone(),
            remaining_args: None,
            parent: None,
        }));
        arg.set_parent(ASTNode::OK_NAME_TYPE_SPECS(Rc::downgrade(&node)));
        OkNameTypeSpecsNode(node)
    }

    pub fn get_name_type_spec_objs(&self, code: &Code) -> Vec<(Option<Rc<String>>, Option<Type>)> {
        let mut name_type_specs_vec: Vec<(Option<Rc<String>>, Option<Type>)> = vec![];
        let arg_obj = self.0.as_ref().borrow().arg.get_name_spec_obj(code);
        name_type_specs_vec.push(arg_obj);
        match &self.0.as_ref().borrow().remaining_args {
            Some(remaining_args) => {
                let mut remaining_args_objs = remaining_args.get_name_type_spec_objs(code);
                name_type_specs_vec.append(&mut remaining_args_objs);
            },
            None => {}
        }
        name_type_specs_vec
    }
}
default_node_impl!(OkNameTypeSpecsNode);

#[derive(Debug, Clone)]
pub struct CoreNameTypeSpecNode {
    colon: TokenNode,
    param_name: TokenNode,
    param_type: TypeExpressionNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct NameTypeSpecNode(Rc<RefCell<CoreNameTypeSpecNode>>);
impl NameTypeSpecNode {
    pub fn new(param_name: &TokenNode, param_type: &TypeExpressionNode, colon: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreNameTypeSpecNode{
            colon: colon.clone(),
            param_name: param_name.clone(),
            param_type: param_type.clone(),
            parent: None,
        }));
        colon.set_parent(ASTNode::NAME_TYPE_SPEC(Rc::downgrade(&node)));
        param_name.set_parent(ASTNode::NAME_TYPE_SPEC(Rc::downgrade(&node)));
        param_type.set_parent(ASTNode::NAME_TYPE_SPEC(Rc::downgrade(&node)));
        NameTypeSpecNode(node)
    }

    pub fn get_name_spec_obj(&self, code: &Code) -> (Option<Rc<String>>, Option<Type>) {
        let name = match self.0.as_ref().borrow().param_name.get_ok() {
            Some(ok_name_node) => {
                Some(Rc::new(ok_name_node.token_value(code)))
            },
            None => None,
        };
        let type_obj = match self.0.as_ref().borrow().param_type.get_type_obj(code) {
            Some(type_obj) => Some(type_obj),
            None => None,
        };
        (name, type_obj)
    }
}
default_node_impl!(NameTypeSpecNode);

#[derive(Debug, Clone)]
pub struct CoreTypeExpressionNode {
    kind: TypeExpressionKind,
    parent: Option<ASTNode>,
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
        TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode{
            kind: TypeExpressionKind::ATOMIC(AtomicTypeNode::new(atomic_type)),
            parent: None,
        })))
    }

    pub fn new_with_user_defined_type(identifier: &TokenNode) -> Self {
        TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode{
            kind: TypeExpressionKind::USER_DEFINED(UserDefinedTypeNode::new(identifier)),
            parent: None,
        })))
    }

    pub fn new_with_array_type(array_size: &TokenNode, sub_type: &TypeExpressionNode, 
        lsquare: &TokenNode, rsquare: &TokenNode, semicolon: &TokenNode) -> Self {
        TypeExpressionNode(Rc::new(RefCell::new(CoreTypeExpressionNode{
            kind: TypeExpressionKind::ARRAY(ArrayTypeNode::new(array_size, sub_type, lsquare, rsquare, semicolon)),
            parent: None,
        })))
    }

    pub fn get_type_obj(&self, code: &Code) -> Option<Type> {
        match &self.0.as_ref().borrow().kind {
            TypeExpressionKind::ATOMIC(atomic_type)                  => atomic_type.get_type_obj(code),
            TypeExpressionKind::USER_DEFINED(user_defined_type) => user_defined_type.get_type_obj(code),
            TypeExpressionKind::ARRAY(array_type)                     => array_type.get_type_obj(code),
            _ => None,
        }
    }
}
default_node_impl!(TypeExpressionNode);
default_errornous_node_impl!(TypeExpressionNode, CoreTypeExpressionNode, TypeExpressionKind);

#[derive(Debug, Clone)]
pub struct CoreAtomicTypeNode {
    kind: TokenNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct AtomicTypeNode(Rc<RefCell<CoreAtomicTypeNode>>);
impl AtomicTypeNode {
    pub fn new(token: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicTypeNode{
            kind: token.clone(),
            parent: None,
        }));
        token.set_parent(ASTNode::ATOMIC_TYPE(Rc::downgrade(&node)));
        AtomicTypeNode(node)
    }

    pub fn get_type_obj(&self, code: &Code) -> Option<Type> {
        match self.0.as_ref().borrow().kind.get_ok() {
            Some(ok_atomic_type) => {
                let atomic_type_str = ok_atomic_type.token_value(code);
                return Atomic::new_with_type_str(&atomic_type_str)
            },
            None => return None
        }
    }
}
default_node_impl!(AtomicTypeNode);

#[derive(Debug, Clone)]
pub struct CoreArrayTypeNode {
    lsquare: TokenNode,
    rsquare: TokenNode,
    semicolon: TokenNode,
    sub_type: TypeExpressionNode,
    size: TokenNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct ArrayTypeNode(Rc<RefCell<CoreArrayTypeNode>>);
impl ArrayTypeNode {
    pub fn new(size: &TokenNode, sub_type: &TypeExpressionNode, 
        lsquare: &TokenNode, rsquare: &TokenNode, semicolon: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreArrayTypeNode{
            lsquare: lsquare.clone(),
            rsquare: rsquare.clone(),
            semicolon: semicolon.clone(),
            sub_type: sub_type.clone(),
            size: size.clone(),
            parent: None,
        }));
        lsquare.set_parent(ASTNode::ARRAY_TYPE(Rc::downgrade(&node)));
        rsquare.set_parent(ASTNode::ARRAY_TYPE(Rc::downgrade(&node)));
        semicolon.set_parent(ASTNode::ARRAY_TYPE(Rc::downgrade(&node)));
        size.set_parent(ASTNode::ARRAY_TYPE(Rc::downgrade(&node)));
        sub_type.set_parent(ASTNode::ARRAY_TYPE(Rc::downgrade(&node)));
        ArrayTypeNode(node)
    }

    pub fn get_type_obj(&self, code: &Code) -> Option<Type> {
        match self.0.as_ref().borrow().sub_type.get_type_obj(code) {
            Some(sub_type_obj) => {
                match self.0.as_ref().borrow().size.get_ok() {
                    Some(size) => {
                        let size = match size.token_value(code).parse::<usize>() {
                            Ok(size) => size,
                            Err(_) => return None,
                        };
                        return Some(Array::new(size, sub_type_obj))
                    },
                    None => return None,
                }
            },
            None => return None
        }
    }
}
default_node_impl!(ArrayTypeNode);

#[derive(Debug, Clone)]
pub struct CoreUserDefinedTypeNode {
    token: TokenNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct UserDefinedTypeNode(Rc<RefCell<CoreUserDefinedTypeNode>>);
impl UserDefinedTypeNode {
    pub fn new(identifier: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreUserDefinedTypeNode{
            token: identifier.clone(),
            parent: None,
        }));
        identifier.set_parent(ASTNode::USER_DEFINED_TYPE(Rc::downgrade(&node)));
        UserDefinedTypeNode(node)
    }

    pub fn get_type_obj(&self, code: &Code) -> Option<Type> {
        match self.0.as_ref().borrow().token.get_ok() {
            Some(ok_token_node) => {
                Some(Type::new_with_user_defined(ok_token_node.token_value(code)))
            },
            None => None
        }
    }
}
default_node_impl!(UserDefinedTypeNode);

#[derive(Debug, Clone)]
pub struct CoreTokenNode {
    pub kind: TokenKind,
    parent: Option<ASTNode>,
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
    pub fn new_with_ok_token(token: &Token, lookahead: usize) -> Self {
        TokenNode(Rc::new(RefCell::new(CoreTokenNode{
            kind: TokenKind::OK(OkTokenNode::new(token, lookahead)),
            parent: None,
        })))
    }

    pub fn new_with_skipped_token(skipped_token: &Token, lookahead: usize) -> Self {
        TokenNode(Rc::new(RefCell::new(CoreTokenNode{
            kind: TokenKind::SKIPPED(SkippedTokenNode::new(skipped_token, lookahead)),
            parent: None,
        })))
    }

    pub fn is_ok(&self) -> Option<TokenNode> {
        match &self.0.as_ref().borrow().kind {
            TokenKind::OK(_) => Some(self.clone()),
            _                    => None,
        }
    }

    pub fn get_ok(&self) -> Option<OkTokenNode> {
        match &self.0.as_ref().borrow().kind {
            TokenKind::OK(ok_token_node) => Some(ok_token_node.clone()),
            _ => None,
        }
    }

    pub fn is_binary_operator(&self) -> Option<BinaryOperatorKind> {
        match &self.0.as_ref().borrow().kind {
            TokenKind::OK(ok_token) => {
                match ok_token.is_binary_operator() {
                    Some(operator) => return Some(operator),
                    None => None,
                }
            },
            _ => None,
        }
    }
}
default_node_impl!(TokenNode);
default_errornous_node_impl!(TokenNode, CoreTokenNode, TokenKind);

#[derive(Debug, Clone)]
pub struct CoreOkTokenNode {
    token: Token,
    lookahead: usize,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct OkTokenNode(Rc<RefCell<CoreOkTokenNode>>);
impl OkTokenNode {
    pub fn new(token: &Token, lookahead: usize) -> Self {
        OkTokenNode(Rc::new(RefCell::new(CoreOkTokenNode{
            token: token.clone(),
            lookahead,
            parent: None,
        })))
    }

    pub fn is_binary_operator(&self) -> Option<BinaryOperatorKind> {
        match self.0.as_ref().borrow().token.core_token {
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
        self.0.as_ref().borrow().token.token_value(code)
    }
}
default_node_impl!(OkTokenNode);

#[derive(Debug, Clone)]
pub struct CoreMissingTokenNode {
    expected_symbols: Rc<Vec<&'static str>>,
    received_token: Token,
    lookahead: usize, 
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct MissingTokenNode(Rc<RefCell<CoreMissingTokenNode>>);
impl MissingTokenNode {
    pub fn new(expected_symbols: &Rc<Vec<&'static str>>, received_token: &Token, lookahead: usize) -> Self {
        MissingTokenNode(Rc::new(RefCell::new(CoreMissingTokenNode{
            expected_symbols: expected_symbols.clone(),
            received_token: received_token.clone(),
            lookahead,
            parent: None,
        })))
    }
}
default_node_impl!(MissingTokenNode);

#[derive(Debug, Clone)]
pub struct CoreSkippedTokenNode {
    skipped_token: Token,
    lookahead: usize,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct SkippedTokenNode(Rc<RefCell<CoreSkippedTokenNode>>);
impl SkippedTokenNode {
    pub fn new(skipped_token: &Token, lookahead: usize) -> Self {
        SkippedTokenNode(Rc::new(RefCell::new(CoreSkippedTokenNode{
            skipped_token: skipped_token.clone(),
            lookahead,
            parent: None,
        })))
    }

    pub fn index(&self) -> usize {
        self.0.as_ref().borrow().skipped_token.index()
    }

    pub fn line_number(&self) -> usize {
        self.0.as_ref().borrow().skipped_token.line_number
    }
}
default_node_impl!(SkippedTokenNode);

#[derive(Debug, Clone)]
pub struct CoreExpressionNode {
    kind: ExpressionKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    ATOMIC(AtomicExpressionNode),
    UNARY(UnaryExpressionNode),
    BINARY(BinaryExpressionNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct ExpressionNode(Rc<RefCell<CoreExpressionNode>>);
impl ExpressionNode {
    pub fn new_with_atomic(atomic_expr: &AtomicExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreExpressionNode{
            kind: ExpressionKind::ATOMIC(atomic_expr.clone()),
            parent: None,
        }));
        atomic_expr.set_parent(ASTNode::EXPRESSION(Rc::downgrade(&node)));
        ExpressionNode(node)
    }

    pub fn new_with_unary(unary_expr: &UnaryExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreExpressionNode{
            kind: ExpressionKind::UNARY(unary_expr.clone()),
            parent: None,
        }));
        unary_expr.set_parent(ASTNode::EXPRESSION(Rc::downgrade(&node)));
        ExpressionNode(node)
    }

    pub fn new_with_binary(operator: &TokenNode, left_expr: &ExpressionNode, right_expr: &ExpressionNode) -> Self {
        let operator_kind = match operator.is_binary_operator() {
            Some(operator_kind) => operator_kind,
            None => unreachable!("any node passed in this method as operator should be a valid operator"),
        };
        let node = Rc::new(RefCell::new(CoreExpressionNode{
            kind: ExpressionKind::BINARY(BinaryExpressionNode::new(operator_kind, left_expr, right_expr)),
            parent: None,
        }));
        ExpressionNode(node)
    }
}
default_node_impl!(ExpressionNode);
default_errornous_node_impl!(ExpressionNode, CoreExpressionNode, ExpressionKind);

#[derive(Debug, Clone)]
pub struct  CoreAtomicExpressionNode {
    kind: AtomicExpressionKind,
    parent: Option<ASTNode>,
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
    pub fn new_with_bool(bool_value: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::BOOL_VALUE(bool_value.clone()),
            parent: None,
        }));
        bool_value.set_parent(ASTNode::ATOMIC_EXPRESSION(Rc::downgrade(&node)));
        AtomicExpressionNode(node)
    }

    pub fn new_with_integer(token: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::INTEGER(token.clone()),
            parent: None,
        }));
        token.set_parent(ASTNode::ATOMIC_EXPRESSION(Rc::downgrade(&node)));
        AtomicExpressionNode(node)
    }

    pub fn new_with_floating_point_number(token: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::FLOATING_POINT_NUMBER(token.clone()),
            parent: None,
        }));
        token.set_parent(ASTNode::ATOMIC_EXPRESSION(Rc::downgrade(&node)));
        AtomicExpressionNode(node)
    }

    pub fn new_with_literal(token: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::LITERAL(token.clone()),
            parent: None,
        }));
        token.set_parent(ASTNode::ATOMIC_EXPRESSION(Rc::downgrade(&node)));
        AtomicExpressionNode(node)
    }

    pub fn new_with_parenthesised_expr(expr: &ExpressionNode, lparen: &TokenNode, rparen: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::PARENTHESISED_EXPRESSION(ParenthesisedExpressionNode::new(expr, lparen, rparen)),
            parent: None,
        }));
        AtomicExpressionNode(node)
    }

    pub fn new_with_atom(atom: &AtomNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomicExpressionNode{
            kind: AtomicExpressionKind::ATOM(atom.clone()),
            parent: None,
        }));
        atom.set_parent(ASTNode::ATOMIC_EXPRESSION(Rc::downgrade(&node)));
        AtomicExpressionNode(node)
    }
}
default_node_impl!(AtomicExpressionNode);
default_errornous_node_impl!(AtomicExpressionNode, CoreAtomicExpressionNode, AtomicExpressionKind);

#[derive(Debug, Clone)]
pub struct CoreParenthesisedExpressionNode {
    lparen: TokenNode,
    rparen: TokenNode,
    expr: ExpressionNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct ParenthesisedExpressionNode(Rc<RefCell<CoreParenthesisedExpressionNode>>);
impl ParenthesisedExpressionNode {
    pub fn new(expr: &ExpressionNode, lparen: &TokenNode, rparen: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreParenthesisedExpressionNode{
            lparen: lparen.clone(),
            rparen: rparen.clone(),
            expr: expr.clone(),
            parent: None,
        }));
        lparen.set_parent(ASTNode::PARENTHESISED_EXPRESSION(Rc::downgrade(&node)));
        rparen.set_parent(ASTNode::PARENTHESISED_EXPRESSION(Rc::downgrade(&node)));
        expr.set_parent(ASTNode::PARENTHESISED_EXPRESSION(Rc::downgrade(&node)));
        ParenthesisedExpressionNode(node)
    }
}
default_node_impl!(ParenthesisedExpressionNode);

#[derive(Debug, Clone)]
pub struct  CoreUnaryExpressionNode {
    kind: UnaryExpressionKind,
    parent: Option<ASTNode>,
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
    pub fn new_with_atomic(atomic_expr: &AtomicExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreUnaryExpressionNode{
            kind: UnaryExpressionKind::ATOMIC(atomic_expr.clone()),
            parent: None,
        }));
        atomic_expr.set_parent(ASTNode::UNARY_EXPRESSION(Rc::downgrade(&node)));
        UnaryExpressionNode(node)
    }

    pub fn new_with_unary(unary_expr: &UnaryExpressionNode, operator: &TokenNode, operator_kind: UnaryOperatorKind) -> Self {
        let node = Rc::new(RefCell::new(CoreUnaryExpressionNode{
            kind: UnaryExpressionKind::UNARY(OnlyUnaryExpressionNode::new(operator, unary_expr, operator_kind)),
            parent: None,
        }));
        UnaryExpressionNode(node)
    }
}
default_node_impl!(UnaryExpressionNode);
default_errornous_node_impl!(UnaryExpressionNode, CoreUnaryExpressionNode, UnaryExpressionKind);

#[derive(Debug, Clone)]
pub struct CoreOnlyUnaryExpressionNode {
    operator: TokenNode,
    unary_expr: UnaryExpressionNode,
    operator_kind: UnaryOperatorKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct OnlyUnaryExpressionNode(Rc<RefCell<CoreOnlyUnaryExpressionNode>>);
impl OnlyUnaryExpressionNode {
    pub fn new(operator: &TokenNode, unary_expr: &UnaryExpressionNode, operator_kind: UnaryOperatorKind) -> Self {
        let node = Rc::new(RefCell::new(CoreOnlyUnaryExpressionNode{
            operator: operator.clone(),
            unary_expr: unary_expr.clone(),
            operator_kind,
            parent: None,
        }));
        operator.set_parent(ASTNode::ONLY_UNARY_EXPRESSION(Rc::downgrade(&node)));
        unary_expr.set_parent(ASTNode::ONLY_UNARY_EXPRESSION(Rc::downgrade(&node)));
        OnlyUnaryExpressionNode(node)
    }
}
default_node_impl!(OnlyUnaryExpressionNode);

#[derive(Debug, Clone)]
pub struct CoreBinaryExpressionNode {
    operator_kind: BinaryOperatorKind,
    left_expr: ExpressionNode,
    right_expr: ExpressionNode,
    parent: Option<ASTNode>,
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
    pub fn new(operator: BinaryOperatorKind, left_expr: &ExpressionNode, right_expr: &ExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreBinaryExpressionNode{
            operator_kind: operator,
            left_expr: left_expr.clone(),
            right_expr: right_expr.clone(),
            parent: None,
        }));
        left_expr.set_parent(ASTNode::BINARY_EXPRESSION(Rc::downgrade(&node)));
        right_expr.set_parent(ASTNode::BINARY_EXPRESSION(Rc::downgrade(&node)));
        BinaryExpressionNode(node)
    }
}
default_node_impl!(BinaryExpressionNode);

#[derive(Debug, Clone)]
pub struct CoreParamsNode {
    kind: ParamsKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum ParamsKind {
    OK(OkParamsNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct ParamsNode(Rc<RefCell<CoreParamsNode>>);
impl ParamsNode {
    pub fn new(ok_params_node: &OkParamsNode) -> Self {
        let node = Rc::new(RefCell::new(CoreParamsNode{
            kind: ParamsKind::OK(ok_params_node.clone()),
            parent: None,
        }));
        ok_params_node.set_parent(ASTNode::PARAMS(Rc::downgrade(&node)));
        ParamsNode(node)
    }
}
default_node_impl!(ParamsNode);
default_errornous_node_impl!(ParamsNode, CoreParamsNode, ParamsKind);

#[derive(Debug, Clone)]
pub struct CoreOkParamsNode {
    comma: Option<TokenNode>,
    param: ExpressionNode,
    remaining_params: Option<ParamsNode>,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct OkParamsNode(Rc<RefCell<CoreOkParamsNode>>);
impl OkParamsNode {
    pub fn new_with_single_param(param: &ExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreOkParamsNode{
            comma: None,
            param: param.clone(),
            remaining_params: None,
            parent: None,
        }));
        param.set_parent(ASTNode::OK_PARAMS(Rc::downgrade(&node)));
        OkParamsNode(node)
    }

    pub fn new_with_params(param: &ExpressionNode, remaining_params: &ParamsNode, comma: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreOkParamsNode{
            comma: Some(comma.clone()),
            param: param.clone(),
            remaining_params: Some(remaining_params.clone()),
            parent: None,
        }));
        comma.set_parent(ASTNode::OK_PARAMS(Rc::downgrade(&node)));
        param.set_parent(ASTNode::OK_PARAMS(Rc::downgrade(&node)));
        remaining_params.set_parent(ASTNode::OK_PARAMS(Rc::downgrade(&node)));
        OkParamsNode(node)
    }
}
default_node_impl!(OkParamsNode);

#[derive(Debug, Clone)]
pub struct CoreCallExpressionNode {
    lparen: TokenNode,
    rparen: TokenNode,
    function_name: TokenNode,
    params: Option<ParamsNode>,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct CallExpressionNode(Rc<RefCell<CoreCallExpressionNode>>);
impl CallExpressionNode {
    pub fn new(function_name: &TokenNode, params: &Option<ParamsNode>, lparen: &TokenNode, rparen: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreCallExpressionNode{
            lparen: lparen.clone(),
            rparen: rparen.clone(),
            function_name: function_name.clone(),
            params: params.clone(),
            parent: None,
        }));
        lparen.set_parent(ASTNode::CALL_EXPRESSION(Rc::downgrade(&node)));
        rparen.set_parent(ASTNode::CALL_EXPRESSION(Rc::downgrade(&node)));
        function_name.set_parent(ASTNode::CALL_EXPRESSION(Rc::downgrade(&node)));
        match params {
            Some(params) => params.set_parent(ASTNode::CALL_EXPRESSION(Rc::downgrade(&node))),
            None => {},
        }
        CallExpressionNode(node)
    }
}
default_node_impl!(CallExpressionNode);

#[derive(Debug, Clone)]
pub struct CoreClassMethodCallNode {
    lparen: TokenNode,
    rparen: TokenNode,
    double_colon: TokenNode,
    class_name: TokenNode,
    class_method_name: TokenNode,
    params: Option<ParamsNode>,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct ClassMethodCallNode(Rc<RefCell<CoreClassMethodCallNode>>);
impl ClassMethodCallNode {
    pub fn new(class_name: &TokenNode, class_method_name: &TokenNode, 
        params: &Option<ParamsNode>, double_colon: &TokenNode, lparen: &TokenNode, rparen: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreClassMethodCallNode{
            lparen: lparen.clone(),
            rparen: rparen.clone(),
            double_colon: double_colon.clone(),
            class_name: class_name.clone(),
            class_method_name: class_method_name.clone(),
            params: params.clone(),
            parent: None,
        }));
        lparen.set_parent(ASTNode::CLASS_METHOD_CALL(Rc::downgrade(&node)));
        rparen.set_parent(ASTNode::CLASS_METHOD_CALL(Rc::downgrade(&node)));
        class_name.set_parent(ASTNode::CLASS_METHOD_CALL(Rc::downgrade(&node)));
        class_method_name.set_parent(ASTNode::CLASS_METHOD_CALL(Rc::downgrade(&node)));
        double_colon.set_parent(ASTNode::CLASS_METHOD_CALL(Rc::downgrade(&node)));
        match params {
            Some(params) => params.set_parent(ASTNode::CLASS_METHOD_CALL(Rc::downgrade(&node))),
            None => {},
        }
        ClassMethodCallNode(node)
    }
}
default_node_impl!(ClassMethodCallNode);

#[derive(Debug, Clone)]
pub struct CoreAtomNode {
    kind: AtomKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum AtomKind {
    ATOM_START(AtomStartNode),
    PROPERTRY_ACCESS(PropertyAccessNode),
    METHOD_ACCESS(MethodAccessNode),
    INDEX_ACCESS(IndexAccessNode),
}

#[derive(Debug, Clone)]
pub struct AtomNode(Rc<RefCell<CoreAtomNode>>);
impl AtomNode {
    pub fn new_with_atom_start(atom_start: &AtomStartNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomNode{
            kind: AtomKind::ATOM_START(atom_start.clone()),
            parent: None,
        }));
        atom_start.set_parent(ASTNode::ATOM(Rc::downgrade(&node)));
        AtomNode(node)
    }

    pub fn new_with_propertry_access(atom: &AtomNode, propertry: &TokenNode, dot: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomNode{
            kind: AtomKind::PROPERTRY_ACCESS(PropertyAccessNode::new(atom, propertry, dot)),
            parent: None,
        }));
        AtomNode(node)
    }

    pub fn new_with_method_access(atom: &AtomNode, method_name: &TokenNode, 
        params: &Option<ParamsNode>, lparen: &TokenNode, rparen: &TokenNode, dot: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomNode{
            kind: AtomKind::METHOD_ACCESS(MethodAccessNode::new(atom, method_name, params, lparen, rparen, dot)),
            parent: None,
        }));
        AtomNode(node)
    }

    pub fn new_with_index_access(atom: &AtomNode, index: &ExpressionNode, 
        lsquare: &TokenNode, rsquare: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomNode{
            kind: AtomKind::INDEX_ACCESS(IndexAccessNode::new(atom, index, lsquare, rsquare)),
            parent: None,
        }));
        AtomNode(node)
    }
}
default_node_impl!(AtomNode);

#[derive(Debug, Clone)]
pub struct CoreAtomStartNode {
    kind: AtomStartKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum AtomStartKind {
    IDENTIFIER(TokenNode),                   // id
    FUNCTION_CALL(CallExpressionNode),       // id(...)
    CLASS_METHOD_CALL(ClassMethodCallNode)   // id::id(...)
}

#[derive(Debug, Clone)]
pub struct AtomStartNode(Rc<RefCell<CoreAtomStartNode>>);
impl AtomStartNode {
    pub fn new_with_identifier(token: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomStartNode{
            kind: AtomStartKind::IDENTIFIER(token.clone()),
            parent: None,
        }));
        token.set_parent(ASTNode::ATOM_START(Rc::downgrade(&node)));
        AtomStartNode(node)
    }

    pub fn new_with_function_call(call_expr: &CallExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomStartNode{
            kind: AtomStartKind::FUNCTION_CALL(call_expr.clone()),
            parent: None,
        }));
        call_expr.set_parent(ASTNode::ATOM_START(Rc::downgrade(&node)));
        AtomStartNode(node)
    }

    pub fn new_with_class_method_call(class_name: &TokenNode, class_method_name: &TokenNode, 
        params: &Option<ParamsNode>, double_colon: &TokenNode, lparen: &TokenNode, rparen: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreAtomStartNode{
            kind: AtomStartKind::CLASS_METHOD_CALL(ClassMethodCallNode::new(
                class_name, class_method_name, params, double_colon, lparen, rparen,
            )),
            parent: None,
        }));
        AtomStartNode(node)
    }
}
default_node_impl!(AtomStartNode);

#[derive(Debug, Clone)]
pub struct CorePropertyAccessNode {
    dot: TokenNode,
    atom: AtomNode,
    propertry: TokenNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct PropertyAccessNode(Rc<RefCell<CorePropertyAccessNode>>);
impl PropertyAccessNode {
    fn new(atom: &AtomNode, propertry: &TokenNode, dot: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CorePropertyAccessNode{
            dot: dot.clone(),
            atom: atom.clone(),
            propertry: propertry.clone(),
            parent: None,
        }));
        dot.set_parent(ASTNode::PROPERTY_ACCESS(Rc::downgrade(&node)));
        atom.set_parent(ASTNode::PROPERTY_ACCESS(Rc::downgrade(&node)));
        propertry.set_parent(ASTNode::PROPERTY_ACCESS(Rc::downgrade(&node)));
        PropertyAccessNode(node)
    }
}
default_node_impl!(PropertyAccessNode);

#[derive(Debug, Clone)]
pub struct CoreMethodAccessNode {
    lparen: TokenNode,
    rparen: TokenNode,
    dot: TokenNode,
    atom: AtomNode,
    method_name: TokenNode,
    params: Option<ParamsNode>,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct MethodAccessNode(Rc<RefCell<CoreMethodAccessNode>>);
impl MethodAccessNode {
    pub fn new(atom: &AtomNode, method_name: &TokenNode, 
        params: &Option<ParamsNode>, lparen: &TokenNode, rparen: &TokenNode, dot: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreMethodAccessNode{
            lparen: lparen.clone(),
            rparen: rparen.clone(),
            dot: dot.clone(),
            atom: atom.clone(),
            method_name: method_name.clone(),
            params: params.clone(),
            parent: None,
        }));
        dot.set_parent(ASTNode::METHOD_ACCESS(Rc::downgrade(&node)));
        lparen.set_parent(ASTNode::METHOD_ACCESS(Rc::downgrade(&node)));
        rparen.set_parent(ASTNode::METHOD_ACCESS(Rc::downgrade(&node)));
        atom.set_parent(ASTNode::METHOD_ACCESS(Rc::downgrade(&node)));
        method_name.set_parent(ASTNode::METHOD_ACCESS(Rc::downgrade(&node)));
        match params {
            Some(params) => params.set_parent(ASTNode::METHOD_ACCESS(Rc::downgrade(&node))),
            None => {}
        }
        MethodAccessNode(node)
    }
}
default_node_impl!(MethodAccessNode);

#[derive(Debug, Clone)]
pub struct CoreIndexAccessNode {
    lsquare: TokenNode,
    rsquare: TokenNode,
    atom: AtomNode,
    index: ExpressionNode,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub struct IndexAccessNode(Rc<RefCell<CoreIndexAccessNode>>);
impl IndexAccessNode {
    pub fn new(atom: &AtomNode, index: &ExpressionNode, lsquare: &TokenNode, rsquare: &TokenNode) -> Self {
        let node = Rc::new(RefCell::new(CoreIndexAccessNode{
            lsquare: lsquare.clone(),
            rsquare: rsquare.clone(),
            atom: atom.clone(),
            index: index.clone(),
            parent: None,
        }));
        lsquare.set_parent(ASTNode::INDEX_ACCESS(Rc::downgrade(&node)));
        rsquare.set_parent(ASTNode::INDEX_ACCESS(Rc::downgrade(&node)));
        atom.set_parent(ASTNode::INDEX_ACCESS(Rc::downgrade(&node)));
        index.set_parent(ASTNode::INDEX_ACCESS(Rc::downgrade(&node)));
        IndexAccessNode(node)
    }
}
default_node_impl!(IndexAccessNode);

#[derive(Debug, Clone)]
pub struct CoreRAssignmentNode {
    kind: RAssignmentKind,
    parent: Option<ASTNode>,
}

#[derive(Debug, Clone)]
pub enum RAssignmentKind {
    LAMBDA(FunctionDeclarationNode),
    EXPRESSION(ExpressionNode),
    MISSING_TOKENS(MissingTokenNode),
}

#[derive(Debug, Clone)]
pub struct RAssignmentNode(Rc<RefCell<CoreRAssignmentNode>>);
impl RAssignmentNode {
    pub fn new_with_lambda(lambda_decl: &FunctionDeclarationNode) -> Self {
        let node = Rc::new(RefCell::new(CoreRAssignmentNode{
            kind: RAssignmentKind::LAMBDA(lambda_decl.clone()),
            parent: None,
        }));
        lambda_decl.set_parent(ASTNode::R_ASSIGNMENT(Rc::downgrade(&node)));
        RAssignmentNode(node)
    }

    pub fn new_with_expr(expr: &ExpressionNode) -> Self {
        let node = Rc::new(RefCell::new(CoreRAssignmentNode{
            kind: RAssignmentKind::EXPRESSION(expr.clone()),
            parent: None,
        }));
        expr.set_parent(ASTNode::R_ASSIGNMENT(Rc::downgrade(&node)));
        RAssignmentNode(node)
    }
}
default_node_impl!(RAssignmentNode);
default_errornous_node_impl!(RAssignmentNode, CoreRAssignmentNode, RAssignmentKind);

