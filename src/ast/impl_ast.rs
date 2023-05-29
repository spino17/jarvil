use super::ast::{
    ArrayTypeNode, AssignmentNode, AtomNode, AtomStartNode, AtomicExpressionNode, AtomicTypeNode,
    BinaryExpressionNode, BlockNode, BoundedMethodWrapperNode, CallExpressionNode, CallNode,
    CallableBodyNode, CallablePrototypeNode, ClassMethodCallNode, ComparisonNode,
    CoreArrayTypeNode, CoreAssignmentNode, CoreAtomNode, CoreAtomStartNode,
    CoreAtomicExpressionNode, CoreAtomicTypeNode, CoreBinaryExpressionNode, CoreBlockNode,
    CoreBoundedMethodWrapperNode, CoreCallExpressionNode, CoreCallNode, CoreCallableBodyNode,
    CoreCallablePrototypeNode, CoreClassMethodCallNode, CoreComparisonNode, CoreExpressionNode,
    CoreExpressionStatementNode, CoreFunctionDeclarationNode, CoreFunctionWrapperNode,
    CoreHashMapTypeNode, CoreIdentifierNode, CoreIncorrectlyIndentedStatementNode,
    CoreIndexAccessNode, CoreInvalidLValueNode, CoreLambdaDeclarationNode,
    CoreLambdaTypeDeclarationNode, CoreMethodAccessNode, CoreMissingTokenNode,
    CoreNameTypeSpecNode, CoreNameTypeSpecsNode, CoreOkAssignmentNode, CoreOkCallableBodyNode,
    CoreOkIdentifierNode, CoreOkLambdaTypeDeclarationNode, CoreOkNameTypeSpecsNode,
    CoreOkParamsNode, CoreOkSelfKeywordNode, CoreOkTokenNode, CoreOkTypeTupleNode,
    CoreOnlyUnaryExpressionNode, CoreParamsNode, CoreParenthesisedExpressionNode,
    CorePropertyAccessNode, CoreRAssignmentNode, CoreRVariableDeclarationNode,
    CoreReturnStatementNode, CoreSelfKeywordNode, CoreSkippedTokenNode, CoreSkippedTokensNode,
    CoreStatemenIndentWrapperNode, CoreStatementNode, CoreStructDeclarationNode,
    CoreStructPropertyDeclarationNode, CoreTokenNode, CoreTupleTypeNode, CoreTypeDeclarationNode,
    CoreTypeExpressionNode, CoreTypeTupleNode, CoreUnaryExpressionNode, CoreUserDefinedTypeNode,
    CoreVariableDeclarationNode, ExpressionNode, ExpressionStatementNode, FunctionDeclarationNode,
    FunctionWrapperNode, HashMapTypeNode, IdentifierNode, IncorrectlyIndentedStatementNode,
    IndexAccessNode, InvalidLValueNode, LambdaDeclarationNode, LambdaTypeDeclarationNode,
    MethodAccessNode, NameTypeSpecNode, NameTypeSpecsNode, OkAssignmentNode, OkCallableBodyNode,
    OkIdentifierNode, OkLambdaTypeDeclarationNode, OkNameTypeSpecsNode, OkParamsNode,
    OkSelfKeywordNode, OkTokenNode, OkTypeTupleNode, OnlyUnaryExpressionNode, ParamsNode,
    ParenthesisedExpressionNode, PropertyAccessNode, RAssignmentNode, RVariableDeclarationNode,
    ReturnStatementNode, SelfKeywordNode, SkippedTokenNode, StatemenIndentWrapperNode,
    StatementNode, StructDeclarationNode, StructPropertyDeclarationNode, TokenNode, TupleTypeNode,
    TypeDeclarationNode, TypeExpressionNode, TypeResolveKind, TypeTupleNode, UnaryExpressionNode,
    UserDefinedTypeNode, VariableDeclarationNode,
};
use super::iterators::{NameTypeSpecsIterator, ParamsIterator, TypeTupleIterator};
use crate::ast::ast::ErrornousNode;
use crate::ast::ast::MissingTokenNode;
use crate::ast::ast::Node;
use crate::ast::ast::SkippedTokensNode;
use crate::code::JarvilCode;
use crate::lexer::token::{BinaryOperatorKind, Token, UnaryOperatorKind};
use crate::parser::resolver::Resolver;
use crate::scope::core::NamespaceKind;
use crate::scope::handler::NamespaceHandler;
use crate::scope::user_defined_types::UserDefinedTypeData;
use crate::types::core::Type;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use text_size::TextRange;
use text_size::TextSize;

impl BlockNode {
    pub fn new(stmts: Vec<StatemenIndentWrapperNode>, newline: &TokenNode) -> Self {
        let node = Rc::new(CoreBlockNode {
            newline: newline.clone(),
            stmts: Rc::new(stmts),
        });
        BlockNode(node)
    }
}

impl Node for BlockNode {
    fn range(&self) -> TextRange {
        let core_block = self.0.as_ref();
        let stmts_len = core_block.stmts.len();
        if stmts_len > 0 {
            let mut index = stmts_len - 1;
            let mut is_empty = false;
            loop {
                match core_block.stmts[index].core_ref() {
                    CoreStatemenIndentWrapperNode::ExtraNewlines(_) => {}
                    _ => break,
                }
                if index == 0 {
                    is_empty = true;
                    break;
                }
                index = index - 1;
            }
            if is_empty {
                impl_range!(self.0.as_ref().newline, self.0.as_ref().newline)
            } else {
                impl_range!(self.0.as_ref().newline, self.0.as_ref().stmts[index])
            }
        } else {
            impl_range!(self.0.as_ref().newline, self.0.as_ref().newline)
        }
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().newline.start_line_number()
    }
}

impl PartialEq for BlockNode {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for BlockNode {}

impl Hash for BlockNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr = Rc::as_ptr(&self.0);
        ptr.hash(state);
    }
}

impl StatemenIndentWrapperNode {
    pub fn new_with_correctly_indented(stmt: &StatementNode) -> Self {
        let node = Rc::new(CoreStatemenIndentWrapperNode::CorrectlyIndented(
            stmt.clone(),
        ));
        StatemenIndentWrapperNode(node)
    }

    pub fn new_with_incorrectly_indented(
        stmt: &StatementNode,
        expected_indent: i64,
        received_indent: i64,
    ) -> Self {
        let node = Rc::new(CoreStatemenIndentWrapperNode::IncorrectlyIndented(
            IncorrectlyIndentedStatementNode::new(stmt, expected_indent, received_indent),
        ));
        StatemenIndentWrapperNode(node)
    }

    pub fn new_with_leading_skipped_tokens(skipped_tokens: &SkippedTokensNode) -> Self {
        let node = Rc::new(CoreStatemenIndentWrapperNode::LeadingSkippedTokens(
            skipped_tokens.clone(),
        ));
        StatemenIndentWrapperNode(node)
    }

    pub fn new_with_trailing_skipped_tokens(skipped_tokens: &SkippedTokensNode) -> Self {
        let node = Rc::new(CoreStatemenIndentWrapperNode::TrailingSkippedTokens(
            skipped_tokens.clone(),
        ));
        StatemenIndentWrapperNode(node)
    }

    pub fn new_with_extra_newlines(skipped_tokens: &SkippedTokensNode) -> Self {
        let node = Rc::new(CoreStatemenIndentWrapperNode::ExtraNewlines(
            skipped_tokens.clone(),
        ));
        StatemenIndentWrapperNode(node)
    }

    impl_core_ref!(CoreStatemenIndentWrapperNode);
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

impl StatementNode {
    pub fn new_with_expression(expr: &ExpressionNode, newline: &TokenNode) -> Self {
        let node = Rc::new(CoreStatementNode::Expression(ExpressionStatementNode::new(
            expr, newline,
        )));
        StatementNode(node)
    }

    pub fn new_with_assignment(assignment: &AssignmentNode) -> Self {
        let node = Rc::new(CoreStatementNode::Assignment(assignment.clone()));
        StatementNode(node)
    }

    pub fn new_with_variable_declaration(variable_decl: &VariableDeclarationNode) -> Self {
        let node = Rc::new(CoreStatementNode::VariableDeclaration(
            variable_decl.clone(),
        ));
        StatementNode(node)
    }

    pub fn new_with_function_wrapper(func_wrapper: &FunctionWrapperNode) -> Self {
        let node = Rc::new(CoreStatementNode::FunctionWrapper(func_wrapper.clone()));
        StatementNode(node)
    }

    pub fn new_with_bounded_method_wrapper(
        bounded_method_wrapper: &BoundedMethodWrapperNode,
    ) -> Self {
        let node = Rc::new(CoreStatementNode::BoundedMethodWrapper(
            bounded_method_wrapper.clone(),
        ));
        StatementNode(node)
    }

    pub fn new_with_type_declaration(type_decl: &TypeDeclarationNode) -> Self {
        let node = Rc::new(CoreStatementNode::TypeDeclaration(type_decl.clone()));
        StatementNode(node)
    }

    pub fn new_with_struct_stmt(struct_stmt: &StructPropertyDeclarationNode) -> Self {
        let node = Rc::new(CoreStatementNode::StructPropertyDeclaration(
            struct_stmt.clone(),
        ));
        StatementNode(node)
    }

    pub fn new_with_return_statement(
        return_keyword: &TokenNode,
        expr: Option<&ExpressionNode>,
        newline: &TokenNode,
    ) -> Self {
        let node = Rc::new(CoreStatementNode::Return(ReturnStatementNode::new(
            return_keyword,
            expr,
            newline,
        )));
        StatementNode(node)
    }

    impl_core_ref!(CoreStatementNode);
}
default_errornous_node_impl!(StatementNode, CoreStatementNode);

impl IncorrectlyIndentedStatementNode {
    pub fn new(stmt: &StatementNode, expected_indent: i64, received_indent: i64) -> Self {
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

impl ExpressionStatementNode {
    pub fn new(expr: &ExpressionNode, newline: &TokenNode) -> Self {
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
        impl_range!(self.0.as_ref().expr, self.0.as_ref().expr)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().expr.start_line_number()
    }
}

impl AssignmentNode {
    pub fn new(l_atom: &AtomNode, r_assign: &RAssignmentNode, equal: &TokenNode) -> Self {
        let node = Rc::new(CoreAssignmentNode::Ok(OkAssignmentNode::new(
            l_atom, r_assign, equal,
        )));
        AssignmentNode(node)
    }

    pub fn new_with_invalid_l_value(
        l_expr: &ExpressionNode,
        r_assign: &RAssignmentNode,
        equal: &TokenNode,
    ) -> Self {
        let node = Rc::new(CoreAssignmentNode::InvalidLValue(InvalidLValueNode::new(
            l_expr, r_assign, equal,
        )));
        AssignmentNode(node)
    }

    impl_core_ref!(CoreAssignmentNode);
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

impl StructPropertyDeclarationNode {
    pub fn new(name_type_spec: &NameTypeSpecNode, newline: &TokenNode) -> Self {
        let node = Rc::new(CoreStructPropertyDeclarationNode {
            newline: newline.clone(),
            name_type_spec: name_type_spec.clone(),
        });
        StructPropertyDeclarationNode(node)
    }

    impl_core_ref!(CoreStructPropertyDeclarationNode);
}

impl Node for StructPropertyDeclarationNode {
    fn range(&self) -> TextRange {
        impl_range!(
            self.0.as_ref().name_type_spec,
            self.0.as_ref().name_type_spec
        )
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().name_type_spec.start_line_number()
    }
}

impl TypeDeclarationNode {
    pub fn new_with_struct(
        name: &IdentifierNode,
        block: &BlockNode,
        type_keyword: &TokenNode,
        struct_keyword: &TokenNode,
        colon: &TokenNode,
    ) -> Self {
        let node = Rc::new(CoreTypeDeclarationNode::Struct(StructDeclarationNode::new(
            name,
            block,
            type_keyword,
            struct_keyword,
            colon,
        )));
        TypeDeclarationNode(node)
    }

    pub fn new_with_lambda(lambda: &LambdaTypeDeclarationNode) -> Self {
        let node = Rc::new(CoreTypeDeclarationNode::Lambda(lambda.clone()));
        TypeDeclarationNode(node)
    }

    impl_core_ref!(CoreTypeDeclarationNode);
}
default_errornous_node_impl!(TypeDeclarationNode, CoreTypeDeclarationNode);

impl StructDeclarationNode {
    pub fn new(
        name: &IdentifierNode,
        block: &BlockNode,
        type_keyword: &TokenNode,
        struct_keyword: &TokenNode,
        colon: &TokenNode,
    ) -> Self {
        let node = Rc::new(CoreStructDeclarationNode {
            type_keyword: type_keyword.clone(),
            colon: colon.clone(),
            struct_keyword: struct_keyword.clone(),
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

impl LambdaTypeDeclarationNode {
    pub fn new(
        name: &IdentifierNode,
        type_keyword: &TokenNode,
        lambda_keyword: &TokenNode,
        equal: &TokenNode,
        lparen: &TokenNode,
        rparen: &TokenNode,
        type_tuple: Option<&TypeTupleNode>,
        right_arrow: Option<&TokenNode>,
        return_type: Option<&TypeExpressionNode>,
        newline: &TokenNode,
    ) -> Self {
        let node = Rc::new(CoreLambdaTypeDeclarationNode::Ok(
            OkLambdaTypeDeclarationNode::new(
                name,
                type_keyword,
                lambda_keyword,
                equal,
                lparen,
                rparen,
                type_tuple,
                right_arrow,
                return_type,
                newline,
            ),
        ));
        LambdaTypeDeclarationNode(node)
    }

    impl_core_ref!(CoreLambdaTypeDeclarationNode);
}
default_errornous_node_impl!(LambdaTypeDeclarationNode, CoreLambdaTypeDeclarationNode);

impl OkLambdaTypeDeclarationNode {
    pub fn new(
        name: &IdentifierNode,
        type_keyword: &TokenNode,
        lambda_keyword: &TokenNode,
        equal: &TokenNode,
        lparen: &TokenNode,
        rparen: &TokenNode,
        type_tuple: Option<&TypeTupleNode>,
        right_arrow: Option<&TokenNode>,
        return_type: Option<&TypeExpressionNode>,
        newline: &TokenNode,
    ) -> Self {
        let node = Rc::new(CoreOkLambdaTypeDeclarationNode {
            type_keyword: type_keyword.clone(),
            lambda_keyword: lambda_keyword.clone(),
            equal: equal.clone(),
            name: name.clone(),
            lparen: lparen.clone(),
            rparen: rparen.clone(),
            type_tuple: extract_from_option!(type_tuple),
            right_arrow: extract_from_option!(right_arrow),
            return_type: extract_from_option!(return_type),
            newline: newline.clone(),
        });
        OkLambdaTypeDeclarationNode(node)
    }

    impl_core_ref!(CoreOkLambdaTypeDeclarationNode);
}

impl Node for OkLambdaTypeDeclarationNode {
    fn range(&self) -> TextRange {
        self.0.as_ref().newline.range()
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().type_keyword.start_line_number()
    }
}

impl CallablePrototypeNode {
    pub fn new(
        params: Option<&NameTypeSpecsNode>,
        return_type: Option<&TypeExpressionNode>,
        lparen: &TokenNode,
        rparen: &TokenNode,
        right_arrow: Option<&TokenNode>,
    ) -> Self {
        let node = Rc::new(CoreCallablePrototypeNode {
            lparen: lparen.clone(),
            rparen: rparen.clone(),
            right_arrow: extract_from_option!(right_arrow),
            params: extract_from_option!(params),
            return_type: extract_from_option!(return_type),
        });
        CallablePrototypeNode(node)
    }

    impl_core_ref!(CoreCallablePrototypeNode);
}

impl Node for CallablePrototypeNode {
    fn range(&self) -> TextRange {
        match &self.0.as_ref().return_type {
            Some(return_type) => impl_range!(self.0.as_ref().lparen, return_type),
            None => impl_range!(self.0.as_ref().lparen, self.0.as_ref().rparen),
        }
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().lparen.start_line_number()
    }
}

impl CallableBodyNode {
    pub fn new(block: &BlockNode, colon: &TokenNode, prototype: &CallablePrototypeNode) -> Self {
        let node = Rc::new(CoreCallableBodyNode::Ok(OkCallableBodyNode::new(
            block, colon, prototype,
        )));
        CallableBodyNode(node)
    }

    impl_core_ref!(CoreCallableBodyNode);
}
default_errornous_node_impl!(CallableBodyNode, CoreCallableBodyNode);

impl OkCallableBodyNode {
    pub fn new(block: &BlockNode, colon: &TokenNode, prototype: &CallablePrototypeNode) -> Self {
        let node = Rc::new(CoreOkCallableBodyNode {
            colon: colon.clone(),
            block: block.clone(),
            prototype: prototype.clone(),
        });
        OkCallableBodyNode(node)
    }

    impl_core_ref!(CoreOkCallableBodyNode);
}

impl Node for OkCallableBodyNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().prototype, self.0.as_ref().block)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().prototype.start_line_number()
    }
}

impl FunctionDeclarationNode {
    pub fn new(name: &IdentifierNode, def_keyword: &TokenNode, body: &CallableBodyNode) -> Self {
        let node = Rc::new(CoreFunctionDeclarationNode {
            name: name.clone(),
            def_keyword: def_keyword.clone(),
            body: body.clone(),
        });
        FunctionDeclarationNode(node)
    }

    impl_core_ref!(CoreFunctionDeclarationNode);
}

impl Node for FunctionDeclarationNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().def_keyword, self.0.as_ref().body)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().def_keyword.start_line_number()
    }
}

impl FunctionWrapperNode {
    pub fn new(func_decl: &FunctionDeclarationNode) -> Self {
        let node = Rc::new(CoreFunctionWrapperNode {
            func_decl: func_decl.clone(),
        });
        FunctionWrapperNode(node)
    }

    impl_core_ref!(CoreFunctionWrapperNode);
}

impl Node for FunctionWrapperNode {
    fn range(&self) -> TextRange {
        self.0.as_ref().func_decl.range()
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().func_decl.start_line_number()
    }
}

impl BoundedMethodWrapperNode {
    pub fn new(func_decl: &FunctionDeclarationNode) -> Self {
        let node = Rc::new(CoreBoundedMethodWrapperNode {
            func_decl: func_decl.clone(),
        });
        BoundedMethodWrapperNode(node)
    }
}

impl Node for BoundedMethodWrapperNode {
    fn range(&self) -> TextRange {
        self.0.as_ref().func_decl.range()
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().func_decl.start_line_number()
    }
}

impl PartialEq for BoundedMethodWrapperNode {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for BoundedMethodWrapperNode {}

impl Hash for BoundedMethodWrapperNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr = Rc::as_ptr(&self.0);
        ptr.hash(state);
    }
}

impl LambdaDeclarationNode {
    pub fn new(name: &IdentifierNode, lambda_keyword: &TokenNode, body: &CallableBodyNode) -> Self {
        let node = Rc::new(CoreLambdaDeclarationNode {
            name: name.clone(),
            lambda_keyword: lambda_keyword.clone(),
            body: body.clone(),
        });
        LambdaDeclarationNode(node)
    }

    impl_core_ref!(CoreLambdaDeclarationNode);
}

impl Node for LambdaDeclarationNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().lambda_keyword, self.0.as_ref().body)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().lambda_keyword.start_line_number()
    }
}

impl VariableDeclarationNode {
    pub fn new(
        name: &IdentifierNode,
        r_node: &RVariableDeclarationNode,
        let_keyword: &TokenNode,
        equal: &TokenNode,
    ) -> Self {
        let node = Rc::new(CoreVariableDeclarationNode {
            let_keyword: let_keyword.clone(),
            equal: equal.clone(),
            name: name.clone(),
            r_node: r_node.clone(),
        });
        VariableDeclarationNode(node)
    }

    impl_core_ref!(CoreVariableDeclarationNode);
}

impl Node for VariableDeclarationNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().let_keyword, self.0.as_ref().r_node)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().let_keyword.start_line_number()
    }
}

impl ReturnStatementNode {
    pub fn new(
        return_keyword: &TokenNode,
        expr: Option<&ExpressionNode>,
        newline: &TokenNode,
    ) -> Self {
        let node = Rc::new(CoreReturnStatementNode {
            return_keyword: return_keyword.clone(),
            expr: match expr {
                Some(expr) => Some(expr.clone()),
                None => None,
            },
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

impl TypeTupleNode {
    pub fn new(ok_type_tuple: &OkTypeTupleNode) -> Self {
        let node = Rc::new(CoreTypeTupleNode::Ok(ok_type_tuple.clone()));
        TypeTupleNode(node)
    }

    pub fn iter(&self) -> TypeTupleIterator {
        TypeTupleIterator::new(self)
    }

    impl_core_ref!(CoreTypeTupleNode);
}
default_errornous_node_impl!(TypeTupleNode, CoreTypeTupleNode);

impl OkTypeTupleNode {
    pub fn new_with_args(
        data_type: &TypeExpressionNode,
        remaining_types: &TypeTupleNode,
        comma: &TokenNode,
    ) -> Self {
        let node = Rc::new(CoreOkTypeTupleNode {
            comma: Some(comma.clone()),
            data_type: data_type.clone(),
            remaining_types: Some(remaining_types.clone()),
        });
        OkTypeTupleNode(node)
    }

    pub fn new_with_single_data_type(data_type: &TypeExpressionNode) -> Self {
        let node = Rc::new(CoreOkTypeTupleNode {
            comma: None,
            data_type: data_type.clone(),
            remaining_types: None,
        });
        OkTypeTupleNode(node)
    }

    impl_core_ref!(CoreOkTypeTupleNode);
}

impl Node for OkTypeTupleNode {
    fn range(&self) -> TextRange {
        match &self.0.as_ref().remaining_types {
            Some(remaining_types) => impl_range!(self.0.as_ref().data_type, remaining_types),
            None => impl_range!(self.0.as_ref().data_type, self.0.as_ref().data_type),
        }
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().data_type.start_line_number()
    }
}

impl NameTypeSpecsNode {
    pub fn new(ok_name_type_specs: &OkNameTypeSpecsNode) -> Self {
        let node = Rc::new(CoreNameTypeSpecsNode::Ok(ok_name_type_specs.clone()));
        NameTypeSpecsNode(node)
    }

    pub fn iter(&self) -> NameTypeSpecsIterator {
        NameTypeSpecsIterator::new(self)
    }

    impl_core_ref!(CoreNameTypeSpecsNode);
}
default_errornous_node_impl!(NameTypeSpecsNode, CoreNameTypeSpecsNode);

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

impl NameTypeSpecNode {
    pub fn new(
        param_name: &IdentifierNode,
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

impl TypeExpressionNode {
    pub fn new_with_atomic_type(atomic_type: &TokenNode) -> Self {
        let node = Rc::new(CoreTypeExpressionNode::Atomic(AtomicTypeNode::new(
            atomic_type,
        )));
        TypeExpressionNode(node)
    }

    pub fn new_with_user_defined_type(identifier: &IdentifierNode) -> Self {
        let node = Rc::new(CoreTypeExpressionNode::UserDefined(
            UserDefinedTypeNode::new(identifier),
        ));
        TypeExpressionNode(node)
    }

    pub fn new_with_array_type(
        sub_type: &TypeExpressionNode,
        lsquare: &TokenNode,
        rsquare: &TokenNode,
    ) -> Self {
        let node = Rc::new(CoreTypeExpressionNode::Array(ArrayTypeNode::new(
            sub_type, lsquare, rsquare,
        )));
        TypeExpressionNode(node)
    }

    pub fn new_with_tuple_type(
        lparen: &TokenNode,
        rparen: &TokenNode,
        types: &TypeTupleNode,
    ) -> Self {
        let node = Rc::new(CoreTypeExpressionNode::Tuple(TupleTypeNode::new(
            lparen, rparen, types,
        )));
        TypeExpressionNode(node)
    }

    pub fn new_with_hashmap_type(
        lcurly: &TokenNode,
        rcurly: &TokenNode,
        colon: &TokenNode,
        key_type: &TypeExpressionNode,
        value_type: &TypeExpressionNode,
    ) -> Self {
        let node = Rc::new(CoreTypeExpressionNode::HashMap(HashMapTypeNode::new(
            lcurly, rcurly, colon, key_type, value_type,
        )));
        TypeExpressionNode(node)
    }

    pub fn type_obj_before_resolved(
        &self,
        resolver: &mut Resolver,
        scope_index: usize,
    ) -> TypeResolveKind {
        match self.core_ref() {
            CoreTypeExpressionNode::Atomic(atomic) => {
                atomic.type_obj_before_resolved(&resolver.code)
            }
            CoreTypeExpressionNode::Array(array) => {
                array.type_obj_before_resolved(resolver, scope_index)
            }
            CoreTypeExpressionNode::Tuple(tuple) => {
                tuple.type_obj_before_resolved(resolver, scope_index)
            }
            CoreTypeExpressionNode::HashMap(hashmap) => {
                hashmap.type_obj_before_resolved(resolver, scope_index)
            }
            CoreTypeExpressionNode::UserDefined(user_defined) => {
                user_defined.type_obj_before_resolved(resolver, scope_index)
            }
            CoreTypeExpressionNode::MissingTokens(_) => TypeResolveKind::Invalid,
        }
    }

    pub fn type_obj_after_resolved(
        &self,
        code: &JarvilCode,
        namespace_handler: &NamespaceHandler,
    ) -> TypeResolveKind {
        match self.core_ref() {
            CoreTypeExpressionNode::Atomic(atomic) => atomic.type_obj_after_resolved(code),
            CoreTypeExpressionNode::Array(array) => {
                array.type_obj_after_resolved(code, namespace_handler)
            }
            CoreTypeExpressionNode::Tuple(tuple) => {
                tuple.type_obj_after_resolved(code, namespace_handler)
            }
            CoreTypeExpressionNode::HashMap(hashmap) => {
                hashmap.type_obj_after_resolved(code, namespace_handler)
            }
            CoreTypeExpressionNode::UserDefined(user_defined) => {
                user_defined.type_obj_after_resolved(code, namespace_handler)
            }
            CoreTypeExpressionNode::MissingTokens(_) => TypeResolveKind::Invalid,
        }
    }

    impl_core_ref!(CoreTypeExpressionNode);
}
default_errornous_node_impl!(TypeExpressionNode, CoreTypeExpressionNode);

impl AtomicTypeNode {
    pub fn new(token: &TokenNode) -> Self {
        let node = Rc::new(CoreAtomicTypeNode {
            kind: token.clone(),
        });
        AtomicTypeNode(node)
    }

    pub fn type_obj_before_resolved(&self, code: &JarvilCode) -> TypeResolveKind {
        self.type_obj_after_resolved(code)
    }

    pub fn type_obj_after_resolved(&self, code: &JarvilCode) -> TypeResolveKind {
        match self.core_ref().kind.core_ref() {
            CoreTokenNode::Ok(ok_token) => {
                return TypeResolveKind::Resolved(Type::new_with_atomic(
                    &ok_token.token_value(code),
                ))
            }
            _ => return TypeResolveKind::Invalid,
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

impl ArrayTypeNode {
    pub fn new(sub_type: &TypeExpressionNode, lsquare: &TokenNode, rsquare: &TokenNode) -> Self {
        let node = Rc::new(CoreArrayTypeNode {
            lsquare: lsquare.clone(),
            rsquare: rsquare.clone(),
            sub_type: sub_type.clone(),
        });
        ArrayTypeNode(node)
    }

    pub fn type_obj_before_resolved(
        &self,
        resolver: &mut Resolver,
        scope_index: usize,
    ) -> TypeResolveKind {
        match self
            .core_ref()
            .sub_type
            .type_obj_before_resolved(resolver, scope_index)
        {
            TypeResolveKind::Resolved(element_type) => {
                return TypeResolveKind::Resolved(Type::new_with_array(&element_type))
            }
            TypeResolveKind::Unresolved(identifier_node) => {
                return TypeResolveKind::Unresolved(identifier_node)
            }
            TypeResolveKind::Invalid => return TypeResolveKind::Invalid,
        }
    }

    pub fn type_obj_after_resolved(
        &self,
        code: &JarvilCode,
        namespace_handler: &NamespaceHandler,
    ) -> TypeResolveKind {
        match self
            .core_ref()
            .sub_type
            .type_obj_after_resolved(code, namespace_handler)
        {
            TypeResolveKind::Resolved(element_type) => {
                return TypeResolveKind::Resolved(Type::new_with_array(&element_type))
            }
            TypeResolveKind::Unresolved(identifier_node) => {
                return TypeResolveKind::Unresolved(identifier_node)
            }
            TypeResolveKind::Invalid => return TypeResolveKind::Invalid,
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

impl TupleTypeNode {
    pub fn new(lparen: &TokenNode, rparen: &TokenNode, types: &TypeTupleNode) -> Self {
        let node = Rc::new(CoreTupleTypeNode {
            lparen: lparen.clone(),
            rparen: rparen.clone(),
            types: types.clone(),
        });
        TupleTypeNode(node)
    }

    pub fn type_obj_before_resolved(
        &self,
        resolver: &mut Resolver,
        scope_index: usize,
    ) -> TypeResolveKind {
        let mut unresolved_identifiers: Vec<OkIdentifierNode> = vec![];
        let mut resolved_types: Vec<Type> = vec![];
        for ty in self.core_ref().types.iter() {
            match ty.type_obj_before_resolved(resolver, scope_index) {
                TypeResolveKind::Resolved(type_obj) => resolved_types.push(type_obj),
                TypeResolveKind::Unresolved(mut identifier) => {
                    unresolved_identifiers.append(&mut identifier)
                }
                TypeResolveKind::Invalid => resolved_types.push(Type::new_with_unknown()),
            }
        }
        if unresolved_identifiers.len() > 0 {
            return TypeResolveKind::Unresolved(unresolved_identifiers);
        } else if resolved_types.len() > 0 {
            return TypeResolveKind::Resolved(Type::new_with_tuple(resolved_types));
        } else {
            return TypeResolveKind::Invalid;
        }
    }

    pub fn type_obj_after_resolved(
        &self,
        code: &JarvilCode,
        namespace_handler: &NamespaceHandler,
    ) -> TypeResolveKind {
        let mut unresolved_identifiers: Vec<OkIdentifierNode> = vec![];
        let mut resolved_types: Vec<Type> = vec![];
        for ty in self.core_ref().types.iter() {
            match ty.type_obj_after_resolved(code, namespace_handler) {
                TypeResolveKind::Resolved(type_obj) => resolved_types.push(type_obj),
                TypeResolveKind::Unresolved(mut identifier) => {
                    unresolved_identifiers.append(&mut identifier)
                }
                TypeResolveKind::Invalid => resolved_types.push(Type::new_with_unknown()),
            }
        }
        if unresolved_identifiers.len() > 0 {
            return TypeResolveKind::Unresolved(unresolved_identifiers);
        } else if resolved_types.len() > 0 {
            return TypeResolveKind::Resolved(Type::new_with_tuple(resolved_types));
        } else {
            return TypeResolveKind::Invalid;
        }
    }

    impl_core_ref!(CoreTupleTypeNode);
}

impl Node for TupleTypeNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().lparen, self.0.as_ref().rparen)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().lparen.start_line_number()
    }
}

impl HashMapTypeNode {
    pub fn new(
        lcurly: &TokenNode,
        rcurly: &TokenNode,
        colon: &TokenNode,
        key_type: &TypeExpressionNode,
        value_type: &TypeExpressionNode,
    ) -> Self {
        let node = Rc::new(CoreHashMapTypeNode {
            lcurly: lcurly.clone(),
            rcurly: rcurly.clone(),
            colon: colon.clone(),
            key_type: key_type.clone(),
            value_type: value_type.clone(),
        });
        HashMapTypeNode(node)
    }

    fn aggregate_key_value_result(
        &self,
        key_result: TypeResolveKind,
        value_result: TypeResolveKind,
    ) -> TypeResolveKind {
        match key_result {
            TypeResolveKind::Resolved(key_type) => match value_result {
                TypeResolveKind::Resolved(value_type) => {
                    return TypeResolveKind::Resolved(Type::new_with_hashmap(
                        &key_type,
                        &value_type,
                    ))
                }
                TypeResolveKind::Unresolved(unresolved_vec) => {
                    return TypeResolveKind::Unresolved(unresolved_vec)
                }
                TypeResolveKind::Invalid => {
                    return TypeResolveKind::Resolved(Type::new_with_hashmap(
                        &key_type,
                        &Type::new_with_unknown(),
                    ))
                }
            },
            TypeResolveKind::Unresolved(mut key_unresolved_vec) => match value_result {
                TypeResolveKind::Resolved(_) => {
                    return TypeResolveKind::Unresolved(key_unresolved_vec)
                }
                TypeResolveKind::Unresolved(mut value_unresolved_vec) => {
                    key_unresolved_vec.append(&mut value_unresolved_vec);
                    return TypeResolveKind::Unresolved(key_unresolved_vec);
                }
                TypeResolveKind::Invalid => return TypeResolveKind::Unresolved(key_unresolved_vec),
            },
            TypeResolveKind::Invalid => match value_result {
                TypeResolveKind::Resolved(value_type) => {
                    return TypeResolveKind::Resolved(Type::new_with_hashmap(
                        &Type::new_with_unknown(),
                        &value_type,
                    ))
                }
                TypeResolveKind::Unresolved(unresolved_vec) => {
                    return TypeResolveKind::Unresolved(unresolved_vec)
                }
                TypeResolveKind::Invalid => return TypeResolveKind::Invalid,
            },
        }
    }

    pub fn type_obj_before_resolved(
        &self,
        resolver: &mut Resolver,
        scope_index: usize,
    ) -> TypeResolveKind {
        let key_result = self
            .core_ref()
            .key_type
            .type_obj_before_resolved(resolver, scope_index);
        let value_result = self
            .core_ref()
            .value_type
            .type_obj_before_resolved(resolver, scope_index);
        return self.aggregate_key_value_result(key_result, value_result);
    }

    pub fn type_obj_after_resolved(
        &self,
        code: &JarvilCode,
        namespace_handler: &NamespaceHandler,
    ) -> TypeResolveKind {
        let key_result = self
            .core_ref()
            .key_type
            .type_obj_after_resolved(code, namespace_handler);
        let value_result = self
            .core_ref()
            .value_type
            .type_obj_after_resolved(code, namespace_handler);
        return self.aggregate_key_value_result(key_result, value_result);
    }

    impl_core_ref!(CoreHashMapTypeNode);
}

impl Node for HashMapTypeNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().lcurly, self.0.as_ref().rcurly)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().lcurly.start_line_number()
    }
}

impl UserDefinedTypeNode {
    pub fn new(identifier: &IdentifierNode) -> Self {
        let node = Rc::new(CoreUserDefinedTypeNode {
            name: identifier.clone(),
        });
        UserDefinedTypeNode(node)
    }

    pub fn type_obj_before_resolved(
        &self,
        resolver: &mut Resolver,
        scope_index: usize,
    ) -> TypeResolveKind {
        if let CoreIdentifierNode::Ok(ok_identifier) = self.core_ref().name.core_ref() {
            let name = ok_identifier.token_value(&resolver.code);
            match resolver
                .namespace_handler
                .namespace
                .types
                .lookup_and_get_symbol_data_ref(scope_index, &name)
            {
                Some((symbol_data, resolved_scope_index, _, _)) => {
                    resolver.bind_decl_to_identifier(
                        ok_identifier,
                        resolved_scope_index,
                        NamespaceKind::Type,
                    );
                    match &*symbol_data.0.as_ref().borrow() {
                        UserDefinedTypeData::Struct(_) => {
                            return TypeResolveKind::Resolved(Type::new_with_struct(
                                name,
                                &symbol_data,
                            ));
                        }
                        UserDefinedTypeData::Lambda(lambda_data) => {
                            return TypeResolveKind::Resolved(Type::new_with_lambda(
                                Some(name),
                                &lambda_data.meta_data.params,
                                &lambda_data.meta_data.return_type,
                            ));
                        }
                    }
                }
                None => return TypeResolveKind::Unresolved(vec![ok_identifier.clone()]),
            }
        }
        return TypeResolveKind::Invalid;
    }

    pub fn type_obj_after_resolved(
        &self,
        code: &JarvilCode,
        namespace_handler: &NamespaceHandler,
    ) -> TypeResolveKind {
        if let CoreIdentifierNode::Ok(ok_identifier) = self.core_ref().name.core_ref() {
            let name = ok_identifier.token_value(code);
            match namespace_handler
                .identifier_binding_table
                .get(ok_identifier)
            {
                Some((scope_index, namespace_kind)) => match namespace_kind {
                    NamespaceKind::Type => {
                        let symbol_data = namespace_handler
                            .namespace
                            .get_from_types_namespace(*scope_index, &name);
                        match symbol_data {
                            Some(symbol_data) => match &*symbol_data.0.as_ref().borrow() {
                                UserDefinedTypeData::Struct(_) => {
                                    return TypeResolveKind::Resolved(Type::new_with_struct(
                                        name,
                                        &symbol_data,
                                    ));
                                }
                                UserDefinedTypeData::Lambda(lambda_data) => {
                                    return TypeResolveKind::Resolved(Type::new_with_lambda(
                                        Some(name),
                                        &lambda_data.meta_data.params,
                                        &lambda_data.meta_data.return_type,
                                    ));
                                }
                            },
                            None => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                },
                None => return TypeResolveKind::Unresolved(vec![ok_identifier.clone()]),
            }
            /*
            match ok_identifier.user_defined_type_symbol_data(
                "identifier should be resolved to `SymbolData<UserDefinedTypeData>`",
            ) {
                Some(symbol_data) => match &*symbol_data.0.as_ref().borrow() {
                    UserDefinedTypeData::STRUCT(_) => {
                        return TypeResolveKind::RESOLVED(Type::new_with_struct(
                            name,
                            &symbol_data,
                        ));
                    }
                    UserDefinedTypeData::LAMBDA(_) => {
                        return TypeResolveKind::RESOLVED(Type::new_with_lambda(
                            Some(name),
                            &symbol_data,
                        ));
                    }
                },
                None => return TypeResolveKind::UNRESOLVED(vec![ok_identifier.clone()]),
            }
             */
        }
        return TypeResolveKind::Invalid;
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

impl RAssignmentNode {
    pub fn new_with_expr(expr: &ExpressionNode, newline: &TokenNode) -> Self {
        let node = Rc::new(CoreRAssignmentNode::Expression(
            ExpressionStatementNode::new(expr, newline),
        ));
        RAssignmentNode(node)
    }

    impl_core_ref!(CoreRAssignmentNode);
}
default_errornous_node_impl!(RAssignmentNode, CoreRAssignmentNode);

impl RVariableDeclarationNode {
    pub fn new_with_lambda(lambda_decl: &LambdaDeclarationNode) -> Self {
        let node = Rc::new(CoreRVariableDeclarationNode::Lambda(lambda_decl.clone()));
        RVariableDeclarationNode(node)
    }

    pub fn new_with_expr(expr: &ExpressionNode, newline: &TokenNode) -> Self {
        let node = Rc::new(CoreRVariableDeclarationNode::Expression(
            ExpressionStatementNode::new(expr, newline),
        ));
        RVariableDeclarationNode(node)
    }

    impl_core_ref!(CoreRVariableDeclarationNode);
}
default_errornous_node_impl!(RVariableDeclarationNode, CoreRVariableDeclarationNode);

impl ExpressionNode {
    pub fn new_with_unary(unary_expr: &UnaryExpressionNode) -> Self {
        let node = Rc::new(CoreExpressionNode::Unary(unary_expr.clone()));
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
        let node = Rc::new(CoreExpressionNode::Binary(BinaryExpressionNode::new(
            operator_kind,
            operator,
            left_expr,
            right_expr,
        )));
        ExpressionNode(node)
    }

    pub fn new_with_comparison(operands: Vec<ExpressionNode>, operators: Vec<TokenNode>) -> Self {
        let node = Rc::new(CoreExpressionNode::Comparison(ComparisonNode::new(
            operands, operators,
        )));
        ExpressionNode(node)
    }

    pub fn is_valid_l_value(&self) -> Option<AtomNode> {
        match &self.0.as_ref() {
            CoreExpressionNode::Unary(unary_expr_node) => match &unary_expr_node.0.as_ref() {
                CoreUnaryExpressionNode::Atomic(atomic_expr_node) => {
                    match &atomic_expr_node.0.as_ref() {
                        CoreAtomicExpressionNode::Atom(atom_node) => {
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

impl AtomicExpressionNode {
    pub fn new_with_bool(bool_value: &TokenNode) -> Self {
        let node = Rc::new(CoreAtomicExpressionNode::BoolValue(bool_value.clone()));
        AtomicExpressionNode(node)
    }

    pub fn new_with_integer(integer_value: &TokenNode) -> Self {
        let node = Rc::new(CoreAtomicExpressionNode::Integer(integer_value.clone()));
        AtomicExpressionNode(node)
    }

    pub fn new_with_floating_point_number(floating_point_value: &TokenNode) -> Self {
        let node = Rc::new(CoreAtomicExpressionNode::FloatingPointNumber(
            floating_point_value.clone(),
        ));
        AtomicExpressionNode(node)
    }

    pub fn new_with_literal(literal_value: &TokenNode) -> Self {
        let node = Rc::new(CoreAtomicExpressionNode::Literal(literal_value.clone()));
        AtomicExpressionNode(node)
    }

    pub fn new_with_parenthesised_expr(
        expr: &ExpressionNode,
        lparen: &TokenNode,
        rparen: &TokenNode,
    ) -> Self {
        let node = Rc::new(CoreAtomicExpressionNode::ParenthesisedExpression(
            ParenthesisedExpressionNode::new(expr, lparen, rparen),
        ));
        AtomicExpressionNode(node)
    }

    pub fn new_with_atom(atom: &AtomNode) -> Self {
        let node = Rc::new(CoreAtomicExpressionNode::Atom(atom.clone()));
        AtomicExpressionNode(node)
    }

    impl_core_ref!(CoreAtomicExpressionNode);
}
default_errornous_node_impl!(AtomicExpressionNode, CoreAtomicExpressionNode);

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

impl UnaryExpressionNode {
    pub fn new_with_atomic(atomic_expr: &AtomicExpressionNode) -> Self {
        let node = Rc::new(CoreUnaryExpressionNode::Atomic(atomic_expr.clone()));
        UnaryExpressionNode(node)
    }

    pub fn new_with_unary(
        unary_expr: &UnaryExpressionNode,
        operator: &TokenNode,
        operator_kind: UnaryOperatorKind,
    ) -> Self {
        let node = Rc::new(CoreUnaryExpressionNode::Unary(
            OnlyUnaryExpressionNode::new(operator, unary_expr, operator_kind),
        ));
        UnaryExpressionNode(node)
    }

    impl_core_ref!(CoreUnaryExpressionNode);
}
default_errornous_node_impl!(UnaryExpressionNode, CoreUnaryExpressionNode);

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

impl ParamsNode {
    pub fn new(ok_params_node: &OkParamsNode) -> Self {
        let node = Rc::new(CoreParamsNode::Ok(ok_params_node.clone()));
        ParamsNode(node)
    }

    pub fn iter(&self) -> ParamsIterator {
        ParamsIterator::new(self)
    }

    impl_core_ref!(CoreParamsNode);
}
default_errornous_node_impl!(ParamsNode, CoreParamsNode);

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

impl CallExpressionNode {
    pub fn new(
        function_name: &IdentifierNode,
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

impl ClassMethodCallNode {
    pub fn new(
        class_name: &IdentifierNode,
        class_method_name: &IdentifierNode,
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

impl AtomNode {
    pub fn new_with_atom_start(atom_start: &AtomStartNode) -> Self {
        let node = Rc::new(CoreAtomNode::AtomStart(atom_start.clone()));
        AtomNode(node)
    }

    pub fn new_with_call(
        atom: &AtomNode,
        params: Option<&ParamsNode>,
        lparen: &TokenNode,
        rparen: &TokenNode,
    ) -> Self {
        let node = Rc::new(CoreAtomNode::Call(CallNode::new(
            atom, params, lparen, rparen,
        )));
        AtomNode(node)
    }

    pub fn new_with_propertry_access(
        atom: &AtomNode,
        propertry: &IdentifierNode,
        dot: &TokenNode,
    ) -> Self {
        let node = Rc::new(CoreAtomNode::PropertyAccess(PropertyAccessNode::new(
            atom, propertry, dot,
        )));
        AtomNode(node)
    }

    pub fn new_with_method_access(
        atom: &AtomNode,
        method_name: &IdentifierNode,
        params: Option<&ParamsNode>,
        lparen: &TokenNode,
        rparen: &TokenNode,
        dot: &TokenNode,
    ) -> Self {
        let node = Rc::new(CoreAtomNode::MethodAccess(MethodAccessNode::new(
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
        let node = Rc::new(CoreAtomNode::IndexAccess(IndexAccessNode::new(
            atom, index, lsquare, rsquare,
        )));
        AtomNode(node)
    }

    pub fn is_valid_l_value(&self) -> bool {
        match &self.0.as_ref() {
            CoreAtomNode::AtomStart(atom_start_node) => atom_start_node.is_valid_l_value(),
            CoreAtomNode::Call(_) => false,
            CoreAtomNode::MethodAccess(_) => false,
            CoreAtomNode::IndexAccess(atom_index_access_node) => {
                let atom = &atom_index_access_node.0.as_ref().atom;
                return atom.is_valid_l_value();
            }
            CoreAtomNode::PropertyAccess(atom_property_access_node) => {
                let atom = &atom_property_access_node.0.as_ref().atom;
                return atom.is_valid_l_value();
            }
        }
    }

    impl_core_ref!(CoreAtomNode);
}

impl AtomStartNode {
    pub fn new_with_identifier(token: &IdentifierNode) -> Self {
        let node = Rc::new(CoreAtomStartNode::Identifier(token.clone()));
        AtomStartNode(node)
    }

    pub fn new_with_self_keyword(self_keyword: &SelfKeywordNode) -> Self {
        let node = Rc::new(CoreAtomStartNode::SelfKeyword(self_keyword.clone()));
        AtomStartNode(node)
    }

    pub fn new_with_function_call(call_expr: &CallExpressionNode) -> Self {
        let node = Rc::new(CoreAtomStartNode::Call(call_expr.clone()));
        AtomStartNode(node)
    }

    pub fn new_with_class_method_call(
        class_name: &IdentifierNode,
        class_method_name: &IdentifierNode,
        params: Option<&ParamsNode>,
        double_colon: &TokenNode,
        lparen: &TokenNode,
        rparen: &TokenNode,
    ) -> Self {
        let node = Rc::new(CoreAtomStartNode::ClassMethodCall(
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
            CoreAtomStartNode::Identifier(_) => true,
            CoreAtomStartNode::SelfKeyword(_) => true,
            _ => false,
        }
    }

    impl_core_ref!(CoreAtomStartNode);
}

impl CallNode {
    pub fn new(
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

impl PropertyAccessNode {
    pub fn new(atom: &AtomNode, propertry: &IdentifierNode, dot: &TokenNode) -> Self {
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

impl MethodAccessNode {
    pub fn new(
        atom: &AtomNode,
        method_name: &IdentifierNode,
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

impl IdentifierNode {
    pub fn new_with_ok(token: &OkTokenNode) -> Self {
        let node = Rc::new(CoreIdentifierNode::Ok(OkIdentifierNode::new(token)));
        IdentifierNode(node)
    }

    impl_core_ref!(CoreIdentifierNode);
}
default_errornous_node_impl!(IdentifierNode, CoreIdentifierNode);

impl OkIdentifierNode {
    pub fn new(token: &OkTokenNode) -> Self {
        let node = Rc::new(CoreOkIdentifierNode {
            token: token.clone(),
        });
        OkIdentifierNode(node)
    }

    pub fn token_value(&self, code: &JarvilCode) -> String {
        self.0.as_ref().token.token_value(code)
    }
}

impl Node for OkIdentifierNode {
    fn range(&self) -> TextRange {
        self.0.as_ref().token.range()
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().token.start_line_number()
    }
}

impl PartialEq for OkIdentifierNode {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for OkIdentifierNode {}

impl Hash for OkIdentifierNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr = Rc::as_ptr(&self.0);
        ptr.hash(state);
    }
}

impl SelfKeywordNode {
    pub fn new_with_ok(token: &OkTokenNode) -> Self {
        let node = Rc::new(CoreSelfKeywordNode::Ok(OkSelfKeywordNode::new(token)));
        SelfKeywordNode(node)
    }

    impl_core_ref!(CoreSelfKeywordNode);
}
default_errornous_node_impl!(SelfKeywordNode, CoreSelfKeywordNode);

impl OkSelfKeywordNode {
    pub fn new(token: &OkTokenNode) -> Self {
        let node = Rc::new(CoreOkSelfKeywordNode {
            token: token.clone(),
        });
        OkSelfKeywordNode(node)
    }

    pub fn token_value(&self, code: &JarvilCode) -> String {
        self.0.as_ref().token.token_value(code)
    }
}

impl Node for OkSelfKeywordNode {
    fn range(&self) -> TextRange {
        self.0.as_ref().token.range()
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().token.start_line_number()
    }
}

impl PartialEq for OkSelfKeywordNode {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for OkSelfKeywordNode {}

impl Hash for OkSelfKeywordNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr = Rc::as_ptr(&self.0);
        ptr.hash(state);
    }
}

impl TokenNode {
    pub fn new_with_ok(token: &Token) -> Self {
        let node = Rc::new(CoreTokenNode::Ok(OkTokenNode::new(token)));
        TokenNode(node)
    }

    pub fn is_binary_operator(&self) -> Option<BinaryOperatorKind> {
        match &self.0.as_ref() {
            CoreTokenNode::Ok(ok_token) => match ok_token.is_binary_operator() {
                Some(operator) => return Some(operator),
                None => None,
            },
            _ => None,
        }
    }

    impl_core_ref!(CoreTokenNode);
}
default_errornous_node_impl!(TokenNode, CoreTokenNode);

impl OkTokenNode {
    pub fn new(token: &Token) -> Self {
        OkTokenNode(Rc::new(CoreOkTokenNode {
            token: token.clone(),
        }))
    }

    pub fn is_binary_operator(&self) -> Option<BinaryOperatorKind> {
        self.core_ref().token.try_as_binary_operator()
    }

    pub fn token_value(&self, code: &JarvilCode) -> String {
        self.0.as_ref().token.token_value(code)
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

impl MissingTokenNode {
    pub fn new(expected_symbols: &Vec<&'static str>, received_token: &Token) -> Self {
        let node = Rc::new(CoreMissingTokenNode {
            // NOTE: Below is traditionally an expensive clone but in our case,
            // mostly `expected_symbols.len()` is less so we avoid runtime overhead of using `Rc`
            // which ideally should be used if length is large for example: in `BlockNode`, see `stmts` field.
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

impl SkippedTokenNode {
    pub fn new(skipped_token: &Token) -> Self {
        let node = Rc::new(CoreSkippedTokenNode {
            skipped_token: skipped_token.clone(),
        });
        SkippedTokenNode(node)
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
