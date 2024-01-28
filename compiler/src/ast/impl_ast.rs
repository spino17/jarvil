use super::ast::{
    ArrayExpressionNode, ArrayTypeNode, AssignmentNode, AtomNode, AtomStartNode,
    AtomicExpressionNode, AtomicTypeNode, BinaryExpressionNode, BlockNode,
    BoundedMethodWrapperNode, BreakStatementNode, CallExpressionNode, CallNode, CallableBodyNode,
    CallablePrototypeNode, CaseBranchStatementNode, ComparisonNode, ConditionalBlockNode,
    ConditionalStatementNode, ContinueStatementNode, CoreArrayExpressionNode, CoreArrayTypeNode,
    CoreAssignmentNode, CoreAtomNode, CoreAtomStartNode, CoreAtomicExpressionNode,
    CoreAtomicTypeNode, CoreBinaryExpressionNode, CoreBlockNode, CoreBoundedMethodWrapperNode,
    CoreBreakStatementNode, CoreCallExpressionNode, CoreCallNode, CoreCallableBodyNode,
    CoreCallablePrototypeNode, CoreCaseBranchStatementNode, CoreComparisonNode,
    CoreConditionalBlockNode, CoreConditionalStatementNode, CoreContinueStatementNode,
    CoreEnumDeclarationNode, CoreEnumVariantDeclarationNode,
    CoreEnumVariantExprOrClassMethodCallNode, CoreExpressionNode, CoreExpressionStatementNode,
    CoreForLoopStatementNode, CoreFunctionDeclarationNode, CoreFunctionWrapperNode,
    CoreGenericTypeDeclNode, CoreHashMapExpressionNode, CoreHashMapTypeNode,
    CoreIdentifierInDeclNode, CoreIdentifierInUseNode, CoreIncorrectlyIndentedStatementNode,
    CoreIndexAccessNode, CoreInterfaceDeclarationNode, CoreInterfaceMethodPrototypeWrapperNode,
    CoreInvalidLValueNode, CoreKeyValuePairNode, CoreLambdaDeclarationNode,
    CoreLambdaTypeDeclarationNode, CoreMatchCaseStatementNode, CoreMethodAccessNode,
    CoreMissingTokenNode, CoreNameTypeSpecNode, CoreOkAssignmentNode, CoreOkIdentifierInDeclNode,
    CoreOkIdentifierInUseNode, CoreOkSelfKeywordNode, CoreOkTokenNode, CoreOnlyUnaryExpressionNode,
    CoreParenthesisedExpressionNode, CorePropertyAccessNode, CoreRAssignmentNode,
    CoreRVariableDeclarationNode, CoreReturnStatementNode, CoreSelfKeywordNode,
    CoreSkippedTokenNode, CoreSkippedTokensNode, CoreStatementIndentWrapperNode, CoreStatementNode,
    CoreStructDeclarationNode, CoreStructPropertyDeclarationNode, CoreSymbolSeparatedSequenceNode,
    CoreTokenNode, CoreTupleExpressionNode, CoreTupleTypeNode, CoreTypeDeclarationNode,
    CoreTypeExpressionNode, CoreUnaryExpressionNode, CoreUserDefinedTypeNode,
    CoreVariableDeclarationNode, CoreWhileLoopStatementNode, EnumDeclarationNode,
    EnumVariantDeclarationNode, EnumVariantExprOrClassMethodCallNode, ExpressionNode,
    ExpressionStatementNode, ForLoopStatementNode, FunctionDeclarationNode, FunctionWrapperNode,
    GenericTypeDeclNode, HashMapExpressionNode, HashMapTypeNode, IdentifierInDeclNode,
    IdentifierInUseNode, IncorrectlyIndentedStatementNode, IndexAccessNode,
    InterfaceDeclarationNode, InterfaceMethodPrototypeWrapperNode, InterfaceMethodTerminalNode,
    InvalidLValueNode, KeyValuePairNode, LambdaDeclarationNode, LambdaTypeDeclarationNode,
    MatchCaseStatementNode, MethodAccessNode, NameTypeSpecNode, OkAssignmentNode,
    OkIdentifierInDeclNode, OkIdentifierInUseNode, OkSelfKeywordNode, OkTokenNode,
    OnlyUnaryExpressionNode, ParenthesisedExpressionNode, PropertyAccessNode, RAssignmentNode,
    RVariableDeclarationNode, ReturnStatementNode, SelfKeywordNode, SkippedTokenNode,
    StatementIndentWrapperNode, StatementNode, StructDeclarationNode,
    StructPropertyDeclarationNode, SymbolSeparatedSequenceNode, TokenNode, TupleExpressionNode,
    TupleTypeNode, TypeDeclarationNode, TypeExpressionNode, TypeResolveKind, UnaryExpressionNode,
    UnresolvedIdentifier, UserDefinedTypeNode, VariableDeclarationNode, WhileLoopStatementNode,
};
use super::iterators::SymbolSeparatedSequenceIterator;
use crate::ast::ast::ErrornousNode;
use crate::ast::ast::MissingTokenNode;
use crate::ast::ast::Node;
use crate::ast::ast::SkippedTokensNode;
use crate::code::JarvilCodeHandler;
use crate::core::string_interner::{Interner, StrId};
use crate::lexer::token::{BinaryOperatorKind, Token, UnaryOperatorKind};
use crate::parser::resolver::{BlockKind, Resolver};
use crate::types::core::Type;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use text_size::TextRange;
use text_size::TextSize;

impl BlockNode {
    pub fn new(
        stmts: Vec<StatementIndentWrapperNode>,
        newline: TokenNode,
        kind: BlockKind,
    ) -> Self {
        let node = Rc::new(CoreBlockNode {
            newline,
            stmts,
            kind,
        });
        BlockNode(node)
    }

    impl_core_ref!(CoreBlockNode);
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
                    CoreStatementIndentWrapperNode::ExtraNewlines(_) => {}
                    _ => break,
                }
                if index == 0 {
                    is_empty = true;
                    break;
                }
                index -= 1;
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

impl StatementIndentWrapperNode {
    pub fn new_with_correctly_indented(stmt: StatementNode) -> Self {
        let node = Rc::new(CoreStatementIndentWrapperNode::CorrectlyIndented(stmt));
        StatementIndentWrapperNode(node)
    }

    pub fn new_with_incorrectly_indented(
        stmt: StatementNode,
        expected_indent: i64,
        received_indent: i64,
    ) -> Self {
        let node = Rc::new(CoreStatementIndentWrapperNode::IncorrectlyIndented(
            IncorrectlyIndentedStatementNode::new(stmt, expected_indent, received_indent),
        ));
        StatementIndentWrapperNode(node)
    }

    pub fn new_with_leading_skipped_tokens(skipped_tokens: SkippedTokensNode) -> Self {
        let node = Rc::new(CoreStatementIndentWrapperNode::LeadingSkippedTokens(
            skipped_tokens,
        ));
        StatementIndentWrapperNode(node)
    }

    pub fn new_with_trailing_skipped_tokens(skipped_tokens: SkippedTokensNode) -> Self {
        let node = Rc::new(CoreStatementIndentWrapperNode::TrailingSkippedTokens(
            skipped_tokens,
        ));
        StatementIndentWrapperNode(node)
    }

    pub fn new_with_extra_newlines(skipped_tokens: SkippedTokensNode) -> Self {
        let node = Rc::new(CoreStatementIndentWrapperNode::ExtraNewlines(
            skipped_tokens,
        ));
        StatementIndentWrapperNode(node)
    }

    impl_core_ref!(CoreStatementIndentWrapperNode);
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
    pub fn new_with_expression(expr: ExpressionNode, newline: TokenNode) -> Self {
        let node = Rc::new(CoreStatementNode::Expression(ExpressionStatementNode::new(
            expr, newline,
        )));
        StatementNode(node)
    }

    pub fn new_with_break_statment(break_stmt: BreakStatementNode) -> Self {
        let node = Rc::new(CoreStatementNode::Break(break_stmt));
        StatementNode(node)
    }

    pub fn new_with_continue_statment(continue_stmt: ContinueStatementNode) -> Self {
        let node = Rc::new(CoreStatementNode::Continue(continue_stmt));
        StatementNode(node)
    }

    pub fn new_with_match_case_statement(match_case_stmt: MatchCaseStatementNode) -> Self {
        let node = Rc::new(CoreStatementNode::MatchCase(match_case_stmt));
        StatementNode(node)
    }

    pub fn new_with_case_branch_statement(case_branch_stmt: CaseBranchStatementNode) -> Self {
        let node = Rc::new(CoreStatementNode::CaseBranch(case_branch_stmt));
        StatementNode(node)
    }

    pub fn new_with_assignment(assignment: AssignmentNode) -> Self {
        let node = Rc::new(CoreStatementNode::Assignment(assignment));
        StatementNode(node)
    }

    pub fn new_with_variable_declaration(variable_decl: VariableDeclarationNode) -> Self {
        let node = Rc::new(CoreStatementNode::VariableDeclaration(variable_decl));
        StatementNode(node)
    }

    pub fn new_with_conditional(conditional: ConditionalStatementNode) -> Self {
        let node = Rc::new(CoreStatementNode::Conditional(conditional));
        StatementNode(node)
    }

    pub fn new_with_while_loop(while_loop: WhileLoopStatementNode) -> Self {
        let node = Rc::new(CoreStatementNode::WhileLoop(while_loop));
        StatementNode(node)
    }

    pub fn new_with_for_loop(for_loop: ForLoopStatementNode) -> Self {
        let node = Rc::new(CoreStatementNode::ForLoop(for_loop));
        StatementNode(node)
    }

    pub fn new_with_function_wrapper(func_wrapper: FunctionWrapperNode) -> Self {
        let node = Rc::new(CoreStatementNode::FunctionWrapper(func_wrapper));
        StatementNode(node)
    }

    pub fn new_with_bounded_method_wrapper(
        bounded_method_wrapper: BoundedMethodWrapperNode,
    ) -> Self {
        let node = Rc::new(CoreStatementNode::BoundedMethodWrapper(
            bounded_method_wrapper,
        ));
        StatementNode(node)
    }

    pub fn new_with_type_declaration(type_decl: TypeDeclarationNode) -> Self {
        let node = Rc::new(CoreStatementNode::TypeDeclaration(type_decl));
        StatementNode(node)
    }

    pub fn new_with_struct_stmt(struct_stmt: StructPropertyDeclarationNode) -> Self {
        let node = Rc::new(CoreStatementNode::StructPropertyDeclaration(struct_stmt));
        StatementNode(node)
    }

    pub fn new_with_enum_stmt(enum_stmt: EnumVariantDeclarationNode) -> Self {
        let node = Rc::new(CoreStatementNode::EnumVariantDeclaration(enum_stmt));
        StatementNode(node)
    }

    pub fn new_with_interface_declaration(interface_decl: InterfaceDeclarationNode) -> Self {
        let node = Rc::new(CoreStatementNode::InterfaceDeclaration(interface_decl));
        StatementNode(node)
    }

    pub fn new_with_interface_method_prototype_wrapper(
        interface_method_prototype: InterfaceMethodPrototypeWrapperNode,
    ) -> Self {
        let node = Rc::new(CoreStatementNode::InterfaceMethodPrototypeWrapper(
            interface_method_prototype,
        ));
        StatementNode(node)
    }

    pub fn new_with_return_statement(
        return_keyword: TokenNode,
        expr: Option<ExpressionNode>,
        newline: TokenNode,
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

impl IncorrectlyIndentedStatementNode {
    pub fn new(stmt: StatementNode, expected_indent: i64, received_indent: i64) -> Self {
        let node = Rc::new(CoreIncorrectlyIndentedStatementNode {
            stmt,
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

impl BreakStatementNode {
    pub fn new(break_keyword: TokenNode, newline: TokenNode) -> Self {
        let node = Rc::new(CoreBreakStatementNode {
            break_keyword,
            newline,
        });
        BreakStatementNode(node)
    }

    impl_core_ref!(CoreBreakStatementNode);
}

impl Node for BreakStatementNode {
    fn range(&self) -> TextRange {
        self.core_ref().break_keyword.range()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().break_keyword.start_line_number()
    }
}

impl ContinueStatementNode {
    pub fn new(continue_keyword: TokenNode, newline: TokenNode) -> Self {
        let node = Rc::new(CoreContinueStatementNode {
            continue_keyword,
            newline,
        });
        ContinueStatementNode(node)
    }

    impl_core_ref!(CoreContinueStatementNode);
}

impl MatchCaseStatementNode {
    pub fn new(
        match_keyword: TokenNode,
        expr: ExpressionNode,
        colon: TokenNode,
        block: BlockNode,
    ) -> Self {
        let node = Rc::new(CoreMatchCaseStatementNode {
            match_keyword,
            expr,
            colon,
            block,
        });
        MatchCaseStatementNode(node)
    }

    impl_core_ref!(CoreMatchCaseStatementNode);
}

impl Node for MatchCaseStatementNode {
    fn range(&self) -> TextRange {
        impl_range!(self.core_ref().match_keyword, self.core_ref().block)
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().match_keyword.start_line_number()
    }
}

impl CaseBranchStatementNode {
    pub fn new(
        case_keyword: TokenNode,
        enum_name: IdentifierInDeclNode,
        double_colon_node: TokenNode,
        variant_name: IdentifierInDeclNode,
        variable_name: Option<(TokenNode, IdentifierInDeclNode, TokenNode)>,
        colon: TokenNode,
        block: BlockNode,
    ) -> Self {
        let node = Rc::new(CoreCaseBranchStatementNode {
            case_keyword,
            enum_name,
            double_colon_node,
            variant_name,
            variable_name,
            colon,
            block,
        });
        CaseBranchStatementNode(node)
    }

    impl_core_ref!(CoreCaseBranchStatementNode);
}

impl Node for CaseBranchStatementNode {
    fn range(&self) -> TextRange {
        impl_range!(self.core_ref().case_keyword, self.core_ref().block)
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().case_keyword.start_line_number()
    }
}

impl Node for ContinueStatementNode {
    fn range(&self) -> TextRange {
        self.core_ref().continue_keyword.range()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().continue_keyword.start_line_number()
    }
}

impl WhileLoopStatementNode {
    pub fn new(
        while_keyword: TokenNode,
        condition_expr: ExpressionNode,
        colon: TokenNode,
        block: BlockNode,
    ) -> Self {
        let node = Rc::new(CoreWhileLoopStatementNode {
            while_keyword,
            condition_expr,
            colon,
            block,
        });
        WhileLoopStatementNode(node)
    }

    impl_core_ref!(CoreWhileLoopStatementNode);
}

impl Node for WhileLoopStatementNode {
    fn range(&self) -> TextRange {
        impl_range!(self.core_ref().while_keyword, self.core_ref().block)
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().while_keyword.start_line_number()
    }
}

impl ForLoopStatementNode {
    pub fn new(
        for_keyword: TokenNode,
        loop_variable: IdentifierInDeclNode,
        in_keyword: TokenNode,
        iterable_expr: ExpressionNode,
        colon: TokenNode,
        block: BlockNode,
    ) -> Self {
        let node = Rc::new(CoreForLoopStatementNode {
            for_keyword,
            loop_variable,
            in_keyword,
            iterable_expr,
            colon,
            block,
        });
        ForLoopStatementNode(node)
    }

    impl_core_ref!(CoreForLoopStatementNode);
}

impl Node for ForLoopStatementNode {
    fn range(&self) -> TextRange {
        impl_range!(self.core_ref().for_keyword, self.core_ref().block)
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().for_keyword.start_line_number()
    }
}

impl ExpressionStatementNode {
    pub fn new(expr: ExpressionNode, newline: TokenNode) -> Self {
        let node = Rc::new(CoreExpressionStatementNode { expr, newline });
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
    pub fn new(l_atom: AtomNode, r_assign: RAssignmentNode, equal: TokenNode) -> Self {
        let node = Rc::new(CoreAssignmentNode::Ok(OkAssignmentNode::new(
            l_atom, r_assign, equal,
        )));
        AssignmentNode(node)
    }

    pub fn new_with_invalid_l_value(
        l_expr: ExpressionNode,
        r_assign: RAssignmentNode,
        equal: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreAssignmentNode::InvalidLValue(InvalidLValueNode::new(
            l_expr, r_assign, equal,
        )));
        AssignmentNode(node)
    }

    impl_core_ref!(CoreAssignmentNode);
}

impl OkAssignmentNode {
    pub fn new(l_atom: AtomNode, r_assign: RAssignmentNode, equal: TokenNode) -> Self {
        let node = Rc::new(CoreOkAssignmentNode {
            equal,
            l_atom,
            r_assign,
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
    pub fn new(l_expr: ExpressionNode, r_assign: RAssignmentNode, equal: TokenNode) -> Self {
        let node = Rc::new(CoreInvalidLValueNode {
            l_expr,
            equal,
            r_assign,
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
    pub fn new(name_type_spec: NameTypeSpecNode, newline: TokenNode) -> Self {
        let node = Rc::new(CoreStructPropertyDeclarationNode {
            newline,
            name_type_spec,
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

impl EnumVariantDeclarationNode {
    pub fn new(
        variant: IdentifierInDeclNode,
        ty: Option<(TokenNode, TypeExpressionNode, TokenNode)>,
        newline: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreEnumVariantDeclarationNode {
            variant,
            ty,
            newline,
        });
        EnumVariantDeclarationNode(node)
    }

    impl_core_ref!(CoreEnumVariantDeclarationNode);
}

impl Node for EnumVariantDeclarationNode {
    fn range(&self) -> TextRange {
        match &self.core_ref().ty {
            Some((_, _, rbracket)) => impl_range!(self.core_ref().variant, rbracket),
            None => self.core_ref().variant.range(),
        }
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().variant.start_line_number()
    }
}

impl TypeDeclarationNode {
    pub fn new_with_struct(
        name: IdentifierInDeclNode,
        block: BlockNode,
        type_keyword: TokenNode,
        struct_keyword: TokenNode,
        implementing_interfaces: Option<(
            TokenNode,
            SymbolSeparatedSequenceNode<IdentifierInUseNode>,
        )>,
        colon: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreTypeDeclarationNode::Struct(StructDeclarationNode::new(
            name,
            block,
            type_keyword,
            struct_keyword,
            implementing_interfaces,
            colon,
        )));
        TypeDeclarationNode(node)
    }

    pub fn new_with_enum(
        type_keyword: TokenNode,
        name: IdentifierInDeclNode,
        enum_keyword: TokenNode,
        colon: TokenNode,
        block: BlockNode,
    ) -> Self {
        let node = Rc::new(CoreTypeDeclarationNode::Enum(EnumDeclarationNode::new(
            type_keyword,
            name,
            enum_keyword,
            colon,
            block,
        )));
        TypeDeclarationNode(node)
    }

    pub fn new_with_lambda(lambda: LambdaTypeDeclarationNode) -> Self {
        let node = Rc::new(CoreTypeDeclarationNode::Lambda(lambda));
        TypeDeclarationNode(node)
    }

    impl_core_ref!(CoreTypeDeclarationNode);
}
default_errornous_node_impl!(TypeDeclarationNode, CoreTypeDeclarationNode);

impl StructDeclarationNode {
    pub fn new(
        name: IdentifierInDeclNode,
        block: BlockNode,
        type_keyword: TokenNode,
        struct_keyword: TokenNode,
        implementing_interfaces: Option<(
            TokenNode,
            SymbolSeparatedSequenceNode<IdentifierInUseNode>,
        )>,
        colon: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreStructDeclarationNode {
            type_keyword,
            colon,
            struct_keyword,
            implementing_interfaces,
            name,
            block,
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

impl EnumDeclarationNode {
    pub fn new(
        type_keyword: TokenNode,
        name: IdentifierInDeclNode,
        enum_keyword: TokenNode,
        colon: TokenNode,
        block: BlockNode,
    ) -> Self {
        let node = Rc::new(CoreEnumDeclarationNode {
            type_keyword,
            name,
            enum_keyword,
            colon,
            block,
        });
        EnumDeclarationNode(node)
    }

    impl_core_ref!(CoreEnumDeclarationNode);
}

impl Node for EnumDeclarationNode {
    fn range(&self) -> TextRange {
        impl_range!(self.core_ref().type_keyword, self.core_ref().block)
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().type_keyword.start_line_number()
    }
}

impl LambdaTypeDeclarationNode {
    pub fn new(
        name: IdentifierInDeclNode,
        type_keyword: TokenNode,
        lambda_keyword: TokenNode,
        equal: TokenNode,
        lparen: TokenNode,
        rparen: TokenNode,
        type_tuple: Option<SymbolSeparatedSequenceNode<TypeExpressionNode>>,
        return_type: Option<(TokenNode, TypeExpressionNode)>,
        newline: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreLambdaTypeDeclarationNode {
            name,
            type_keyword,
            lambda_keyword,
            equal,
            lparen,
            rparen,
            type_tuple,
            return_type,
            newline,
        });
        LambdaTypeDeclarationNode(node)
    }

    impl_core_ref!(CoreLambdaTypeDeclarationNode);
}

impl Node for LambdaTypeDeclarationNode {
    fn range(&self) -> TextRange {
        self.0.as_ref().newline.range()
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().type_keyword.start_line_number()
    }
}

impl CallablePrototypeNode {
    pub fn new(
        params: Option<SymbolSeparatedSequenceNode<NameTypeSpecNode>>,
        return_type: Option<(TokenNode, TypeExpressionNode)>,
        lparen: TokenNode,
        rparen: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreCallablePrototypeNode {
            lparen,
            rparen,
            params,
            return_type,
        });
        CallablePrototypeNode(node)
    }

    impl_core_ref!(CoreCallablePrototypeNode);
}

impl Node for CallablePrototypeNode {
    fn range(&self) -> TextRange {
        match &self.0.as_ref().return_type {
            Some((_, return_type)) => impl_range!(self.0.as_ref().lparen, return_type),
            None => impl_range!(self.0.as_ref().lparen, self.0.as_ref().rparen),
        }
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().lparen.start_line_number()
    }
}

impl CallableBodyNode {
    pub fn new(block: BlockNode, colon: TokenNode, prototype: CallablePrototypeNode) -> Self {
        let node = Rc::new(CoreCallableBodyNode {
            block,
            colon,
            prototype,
        });
        CallableBodyNode(node)
    }

    impl_core_ref!(CoreCallableBodyNode);
}

impl Node for CallableBodyNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().prototype, self.0.as_ref().block)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().prototype.start_line_number()
    }
}

impl FunctionDeclarationNode {
    pub fn new(name: IdentifierInDeclNode, def_keyword: TokenNode, body: CallableBodyNode) -> Self {
        let node = Rc::new(CoreFunctionDeclarationNode {
            name,
            def_keyword,
            body,
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
    pub fn new(func_decl: FunctionDeclarationNode) -> Self {
        let node = Rc::new(CoreFunctionWrapperNode { func_decl });
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
    pub fn new(func_decl: FunctionDeclarationNode) -> Self {
        let node = Rc::new(CoreBoundedMethodWrapperNode { func_decl });
        BoundedMethodWrapperNode(node)
    }

    impl_core_ref!(CoreBoundedMethodWrapperNode);
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
    pub fn new(
        name: IdentifierInDeclNode,
        lambda_keyword: TokenNode,
        body: CallableBodyNode,
    ) -> Self {
        let node = Rc::new(CoreLambdaDeclarationNode {
            name,
            lambda_keyword,
            body,
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
        name: IdentifierInDeclNode,
        r_node: RVariableDeclarationNode,
        let_keyword: TokenNode,
        equal: TokenNode,
        optional_ty_annotation: Option<(TokenNode, TypeExpressionNode)>,
    ) -> Self {
        let node = Rc::new(CoreVariableDeclarationNode {
            let_keyword,
            equal,
            name,
            r_node,
            ty_annotation: optional_ty_annotation,
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

impl InterfaceDeclarationNode {
    pub fn new(
        interface_keyword: TokenNode,
        name: IdentifierInDeclNode,
        colon: TokenNode,
        block: BlockNode,
    ) -> Self {
        let node = Rc::new(CoreInterfaceDeclarationNode {
            interface_keyword,
            name,
            colon,
            block,
        });
        InterfaceDeclarationNode(node)
    }

    impl_core_ref!(CoreInterfaceDeclarationNode);
}

impl Node for InterfaceDeclarationNode {
    fn range(&self) -> TextRange {
        impl_range!(self.0.as_ref().interface_keyword, self.0.as_ref().block)
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().interface_keyword.start_line_number()
    }
}

impl InterfaceMethodPrototypeWrapperNode {
    pub fn new(
        def_keyword: TokenNode,
        name: IdentifierInDeclNode,
        prototype: CallablePrototypeNode,
        terminal: InterfaceMethodTerminalNode,
    ) -> Self {
        let node = Rc::new(CoreInterfaceMethodPrototypeWrapperNode {
            def_keyword,
            name,
            prototype,
            terminal,
        });
        InterfaceMethodPrototypeWrapperNode(node)
    }

    impl_core_ref!(CoreInterfaceMethodPrototypeWrapperNode);
}

impl Node for InterfaceMethodPrototypeWrapperNode {
    fn range(&self) -> TextRange {
        match &self.0.as_ref().terminal {
            InterfaceMethodTerminalNode::NoDefaultBody(newline) => {
                impl_range!(self.0.as_ref().def_keyword, newline)
            }
            InterfaceMethodTerminalNode::HasDefaultBody(_, block) => {
                impl_range!(self.0.as_ref().def_keyword, block)
            }
        }
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().def_keyword.start_line_number()
    }
}

impl ConditionalBlockNode {
    pub fn new(
        condition_keyword: TokenNode,
        condition_expr: ExpressionNode,
        colon: TokenNode,
        block: BlockNode,
    ) -> Self {
        let node = Rc::new(CoreConditionalBlockNode {
            condition_keyword,
            condition_expr,
            colon,
            block,
        });
        ConditionalBlockNode(node)
    }

    impl_core_ref!(CoreConditionalBlockNode);
}

impl Node for ConditionalBlockNode {
    fn range(&self) -> TextRange {
        impl_range!(self.core_ref().condition_keyword, self.core_ref().block)
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().condition_keyword.start_line_number()
    }
}

impl ConditionalStatementNode {
    pub fn new(
        if_block: ConditionalBlockNode,
        elifs: Vec<ConditionalBlockNode>,
        else_block: Option<(TokenNode, TokenNode, BlockNode)>,
    ) -> Self {
        let node = Rc::new(CoreConditionalStatementNode {
            if_block,
            elifs,
            else_block,
        });
        ConditionalStatementNode(node)
    }

    impl_core_ref!(CoreConditionalStatementNode);
}

impl Node for ConditionalStatementNode {
    fn range(&self) -> TextRange {
        let core_ref = self.core_ref();
        match &core_ref.else_block {
            Some((_, _, else_block)) => {
                impl_range!(core_ref.if_block, else_block)
            }
            None => {
                if core_ref.elifs.is_empty() {
                    core_ref.if_block.range()
                } else {
                    impl_range!(core_ref.if_block, core_ref.elifs.last().unwrap())
                }
            }
        }
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().if_block.start_line_number()
    }
}

impl ReturnStatementNode {
    pub fn new(
        return_keyword: TokenNode,
        expr: Option<ExpressionNode>,
        newline: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreReturnStatementNode {
            return_keyword,
            expr,
            newline,
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

impl NameTypeSpecNode {
    pub fn new(
        name: IdentifierInDeclNode,
        data_type: TypeExpressionNode,
        colon: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreNameTypeSpecNode {
            colon,
            name,
            data_type,
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
    pub fn new_with_atomic_type(atomic_type: TokenNode) -> Self {
        let node = Rc::new(CoreTypeExpressionNode::Atomic(AtomicTypeNode::new(
            atomic_type,
        )));
        TypeExpressionNode(node)
    }

    pub fn new_with_user_defined_type(identifier: IdentifierInUseNode) -> Self {
        let node = Rc::new(CoreTypeExpressionNode::UserDefined(
            UserDefinedTypeNode::new(identifier),
        ));
        TypeExpressionNode(node)
    }

    pub fn new_with_array_type(
        sub_type: TypeExpressionNode,
        lsquare: TokenNode,
        rsquare: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreTypeExpressionNode::Array(ArrayTypeNode::new(
            sub_type, lsquare, rsquare,
        )));
        TypeExpressionNode(node)
    }

    pub fn new_with_tuple_type(
        lparen: TokenNode,
        rparen: TokenNode,
        types: SymbolSeparatedSequenceNode<TypeExpressionNode>,
    ) -> Self {
        let node = Rc::new(CoreTypeExpressionNode::Tuple(TupleTypeNode::new(
            lparen, rparen, types,
        )));
        TypeExpressionNode(node)
    }

    pub fn new_with_hashmap_type(
        lcurly: TokenNode,
        rcurly: TokenNode,
        colon: TokenNode,
        key_type: TypeExpressionNode,
        value_type: TypeExpressionNode,
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
        has_generics: &mut bool,
    ) -> TypeResolveKind {
        match self.core_ref() {
            CoreTypeExpressionNode::Atomic(atomic) => atomic.type_obj_before_resolved(
                &resolver.code_handler,
                &mut resolver.semantic_state_db.interner,
            ),
            CoreTypeExpressionNode::Array(array) => {
                array.type_obj_before_resolved(resolver, scope_index, has_generics)
            }
            CoreTypeExpressionNode::Tuple(tuple) => {
                tuple.type_obj_before_resolved(resolver, scope_index, has_generics)
            }
            CoreTypeExpressionNode::HashMap(hashmap) => {
                hashmap.type_obj_before_resolved(resolver, scope_index, has_generics)
            }
            CoreTypeExpressionNode::UserDefined(user_defined) => {
                user_defined.type_obj_before_resolved(resolver, scope_index, has_generics)
            }
            CoreTypeExpressionNode::MissingTokens(_) => TypeResolveKind::Invalid,
        }
    }

    impl_core_ref!(CoreTypeExpressionNode);
}
default_errornous_node_impl!(TypeExpressionNode, CoreTypeExpressionNode);

impl PartialEq for TypeExpressionNode {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for TypeExpressionNode {}

impl Hash for TypeExpressionNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr = Rc::as_ptr(&self.0);
        ptr.hash(state);
    }
}

impl AtomicTypeNode {
    pub fn new(token: TokenNode) -> Self {
        let node = Rc::new(CoreAtomicTypeNode { kind: token });
        AtomicTypeNode(node)
    }

    pub fn type_obj_before_resolved(
        &self,
        code: &JarvilCodeHandler,
        interner: &mut Interner,
    ) -> TypeResolveKind {
        self.type_obj_after_resolved(code, interner)
    }

    pub fn type_obj_after_resolved(
        &self,
        code: &JarvilCodeHandler,
        interner: &mut Interner,
    ) -> TypeResolveKind {
        let CoreTokenNode::Ok(ok_token) = self.core_ref().kind.core_ref() else {
            return TypeResolveKind::Invalid;
        };
        let idx = ok_token.token_value(code, interner);
        TypeResolveKind::Resolved(Type::new_with_atomic(interner.lookup(idx)))
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
    pub fn new(sub_type: TypeExpressionNode, lsquare: TokenNode, rsquare: TokenNode) -> Self {
        let node = Rc::new(CoreArrayTypeNode {
            lsquare,
            rsquare,
            sub_type,
        });
        ArrayTypeNode(node)
    }

    pub fn type_obj_before_resolved(
        &self,
        resolver: &mut Resolver,
        scope_index: usize,
        has_generics: &mut bool,
    ) -> TypeResolveKind {
        match self
            .core_ref()
            .sub_type
            .type_obj_before_resolved(resolver, scope_index, has_generics)
        {
            TypeResolveKind::Resolved(element_type) => {
                return TypeResolveKind::Resolved(Type::new_with_array(element_type))
            }
            TypeResolveKind::Unresolved(identifier_node) => {
                return TypeResolveKind::Unresolved(identifier_node)
            }
            TypeResolveKind::Invalid => TypeResolveKind::Invalid,
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
    pub fn new(
        lparen: TokenNode,
        rparen: TokenNode,
        types: SymbolSeparatedSequenceNode<TypeExpressionNode>,
    ) -> Self {
        let node = Rc::new(CoreTupleTypeNode {
            lparen,
            rparen,
            types,
        });
        TupleTypeNode(node)
    }

    pub fn type_obj_before_resolved(
        &self,
        resolver: &mut Resolver,
        scope_index: usize,
        has_generics: &mut bool,
    ) -> TypeResolveKind {
        let mut unresolved_identifiers: Vec<UnresolvedIdentifier> = vec![];
        let mut resolved_types: Vec<Type> = vec![];
        for ty in self.core_ref().types.iter() {
            match ty.type_obj_before_resolved(resolver, scope_index, has_generics) {
                TypeResolveKind::Resolved(type_obj) => resolved_types.push(type_obj),
                TypeResolveKind::Unresolved(mut unresolved) => {
                    unresolved_identifiers.append(&mut unresolved);
                }
                TypeResolveKind::Invalid => resolved_types.push(Type::new_with_unknown()),
            }
        }
        if !unresolved_identifiers.is_empty() {
            return TypeResolveKind::Unresolved(unresolved_identifiers);
        } else if !resolved_types.is_empty() {
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
        lcurly: TokenNode,
        rcurly: TokenNode,
        colon: TokenNode,
        key_type: TypeExpressionNode,
        value_type: TypeExpressionNode,
    ) -> Self {
        let node = Rc::new(CoreHashMapTypeNode {
            lcurly,
            rcurly,
            colon,
            key_type,
            value_type,
        });
        HashMapTypeNode(node)
    }

    fn aggregate_key_value_result<'a>(
        &'a self,
        key_result: TypeResolveKind<'a>,
        value_result: TypeResolveKind<'a>,
    ) -> TypeResolveKind<'a> {
        match key_result {
            TypeResolveKind::Resolved(key_type) => match value_result {
                TypeResolveKind::Resolved(value_type) => {
                    return TypeResolveKind::Resolved(Type::new_with_hashmap(key_type, value_type))
                }
                TypeResolveKind::Unresolved(unresolved_vec) => {
                    return TypeResolveKind::Unresolved(unresolved_vec)
                }
                TypeResolveKind::Invalid => {
                    return TypeResolveKind::Resolved(Type::new_with_hashmap(
                        key_type,
                        Type::new_with_unknown(),
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
                        Type::new_with_unknown(),
                        value_type,
                    ))
                }
                TypeResolveKind::Unresolved(unresolved_vec) => {
                    return TypeResolveKind::Unresolved(unresolved_vec)
                }
                TypeResolveKind::Invalid => TypeResolveKind::Invalid,
            },
        }
    }

    pub fn type_obj_before_resolved(
        &self,
        resolver: &mut Resolver,
        scope_index: usize,
        has_generics: &mut bool,
    ) -> TypeResolveKind {
        let key_result =
            self.core_ref()
                .key_type
                .type_obj_before_resolved(resolver, scope_index, has_generics);
        let value_result = self.core_ref().value_type.type_obj_before_resolved(
            resolver,
            scope_index,
            has_generics,
        );
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
    pub fn new(identifier: IdentifierInUseNode) -> Self {
        let node = Rc::new(CoreUserDefinedTypeNode { name: identifier });
        UserDefinedTypeNode(node)
    }

    pub fn type_obj_before_resolved(
        &self,
        resolver: &mut Resolver,
        scope_index: usize,
        has_generics: &mut bool,
    ) -> TypeResolveKind {
        resolver.type_obj_from_user_defined_type_expr(self, scope_index, has_generics)
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
    pub fn new_with_expr(expr: ExpressionNode, newline: TokenNode) -> Self {
        let node = Rc::new(CoreRAssignmentNode {
            expr: ExpressionStatementNode::new(expr, newline),
        });
        RAssignmentNode(node)
    }

    impl_core_ref!(CoreRAssignmentNode);
}

impl Node for RAssignmentNode {
    fn range(&self) -> TextRange {
        self.core_ref().expr.range()
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().expr.start_line_number()
    }
}

impl RVariableDeclarationNode {
    pub fn new_with_lambda(lambda_decl: LambdaDeclarationNode) -> Self {
        let node = Rc::new(CoreRVariableDeclarationNode::Lambda(lambda_decl));
        RVariableDeclarationNode(node)
    }

    pub fn new_with_expr(expr: ExpressionNode, newline: TokenNode) -> Self {
        let node = Rc::new(CoreRVariableDeclarationNode::Expression(
            ExpressionStatementNode::new(expr, newline),
        ));
        RVariableDeclarationNode(node)
    }

    impl_core_ref!(CoreRVariableDeclarationNode);
}

impl ExpressionNode {
    pub fn new_with_unary(unary_expr: UnaryExpressionNode) -> Self {
        let node = Rc::new(CoreExpressionNode::Unary(unary_expr));
        ExpressionNode(node)
    }

    pub fn new_with_binary(
        operator: TokenNode,
        left_expr: ExpressionNode,
        right_expr: ExpressionNode,
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
        let CoreExpressionNode::Unary(unary_expr_node) = &self.0.as_ref() else {
            return None;
        };
        let CoreUnaryExpressionNode::Atomic(atomic_expr_node) = &unary_expr_node.0.as_ref() else {
            return None;
        };
        let CoreAtomicExpressionNode::Atom(atom_node) = &atomic_expr_node.0.as_ref() else {
            return None;
        };
        if atom_node.is_valid_l_value() {
            Some(atom_node.clone())
        } else {
            None
        }
    }

    impl_core_ref!(CoreExpressionNode);
}

impl AtomicExpressionNode {
    pub fn new_with_bool(bool_value: TokenNode) -> Self {
        let node = Rc::new(CoreAtomicExpressionNode::Bool(bool_value));
        AtomicExpressionNode(node)
    }

    pub fn new_with_integer(integer_value: TokenNode) -> Self {
        let node = Rc::new(CoreAtomicExpressionNode::Integer(integer_value));
        AtomicExpressionNode(node)
    }

    pub fn new_with_floating_point_number(floating_point_value: TokenNode) -> Self {
        let node = Rc::new(CoreAtomicExpressionNode::FloatingPointNumber(
            floating_point_value,
        ));
        AtomicExpressionNode(node)
    }

    pub fn new_with_literal(literal_value: TokenNode) -> Self {
        let node = Rc::new(CoreAtomicExpressionNode::Literal(literal_value));
        AtomicExpressionNode(node)
    }

    pub fn new_with_parenthesised_expr(
        expr: ExpressionNode,
        lparen: TokenNode,
        rparen: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreAtomicExpressionNode::ParenthesisedExpression(
            ParenthesisedExpressionNode::new(expr, lparen, rparen),
        ));
        AtomicExpressionNode(node)
    }

    pub fn new_with_atom(atom: AtomNode) -> Self {
        let node = Rc::new(CoreAtomicExpressionNode::Atom(atom));
        AtomicExpressionNode(node)
    }

    pub fn new_with_array_expr(
        lsquare: TokenNode,
        rsquare: TokenNode,
        initials: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> AtomicExpressionNode {
        let node = Rc::new(CoreAtomicExpressionNode::ArrayExpression(
            ArrayExpressionNode::new(lsquare, rsquare, initials),
        ));
        AtomicExpressionNode(node)
    }

    pub fn new_with_hashmap_expr(
        lcurly: TokenNode,
        rcurly: TokenNode,
        initials: Option<SymbolSeparatedSequenceNode<KeyValuePairNode>>,
    ) -> AtomicExpressionNode {
        let node = Rc::new(CoreAtomicExpressionNode::HashMapExpression(
            HashMapExpressionNode::new(lcurly, rcurly, initials),
        ));
        AtomicExpressionNode(node)
    }

    pub fn new_with_tuple_expr(
        lround: TokenNode,
        rround: TokenNode,
        initials: SymbolSeparatedSequenceNode<ExpressionNode>,
    ) -> AtomicExpressionNode {
        let node = Rc::new(CoreAtomicExpressionNode::TupleExpression(
            TupleExpressionNode::new(lround, rround, initials),
        ));
        AtomicExpressionNode(node)
    }

    impl_core_ref!(CoreAtomicExpressionNode);
}
default_errornous_node_impl!(AtomicExpressionNode, CoreAtomicExpressionNode);

impl ParenthesisedExpressionNode {
    pub fn new(expr: ExpressionNode, lparen: TokenNode, rparen: TokenNode) -> Self {
        let node = Rc::new(CoreParenthesisedExpressionNode {
            lparen,
            rparen,
            expr,
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
    pub fn new_with_atomic(atomic_expr: AtomicExpressionNode) -> Self {
        let node = Rc::new(CoreUnaryExpressionNode::Atomic(atomic_expr));
        UnaryExpressionNode(node)
    }

    pub fn new_with_unary(
        unary_expr: UnaryExpressionNode,
        operator: TokenNode,
        operator_kind: UnaryOperatorKind,
    ) -> Self {
        let node = Rc::new(CoreUnaryExpressionNode::Unary(
            OnlyUnaryExpressionNode::new(operator, unary_expr, operator_kind),
        ));
        UnaryExpressionNode(node)
    }

    impl_core_ref!(CoreUnaryExpressionNode);
}

impl OnlyUnaryExpressionNode {
    pub fn new(
        operator: TokenNode,
        unary_expr: UnaryExpressionNode,
        operator_kind: UnaryOperatorKind,
    ) -> Self {
        let node = Rc::new(CoreOnlyUnaryExpressionNode {
            operator,
            unary_expr,
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
        operator: TokenNode,
        left_expr: ExpressionNode,
        right_expr: ExpressionNode,
    ) -> Self {
        let node = Rc::new(CoreBinaryExpressionNode {
            operator_kind,
            operator,
            left_expr,
            right_expr,
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

impl<T: Node> SymbolSeparatedSequenceNode<T> {
    pub fn new_with_single_entity(entity: T) -> Self {
        let node = Rc::new(CoreSymbolSeparatedSequenceNode {
            entity,
            remaining_entities: None,
        });
        SymbolSeparatedSequenceNode(node)
    }

    pub fn new_with_entities(
        entity: T,
        remaining_entities: SymbolSeparatedSequenceNode<T>,
        comma: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreSymbolSeparatedSequenceNode {
            entity,
            remaining_entities: Some((comma, remaining_entities)),
        });
        SymbolSeparatedSequenceNode(node)
    }

    pub fn iter(&self) -> SymbolSeparatedSequenceIterator<T> {
        SymbolSeparatedSequenceIterator::new(self)
    }

    pub fn core_ref(&self) -> &CoreSymbolSeparatedSequenceNode<T> {
        self.0.as_ref()
    }
}

impl<T: Clone + Node> Node for SymbolSeparatedSequenceNode<T> {
    fn range(&self) -> TextRange {
        match &self.0.as_ref().remaining_entities {
            Some((_, remaining_entities)) => {
                impl_range!(self.0.as_ref().entity, remaining_entities)
            }
            None => impl_range!(self.0.as_ref().entity, self.0.as_ref().entity),
        }
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().entity.start_line_number()
    }
}

impl CallExpressionNode {
    pub fn new(
        function_name: IdentifierInUseNode,
        params: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
        lparen: TokenNode,
        rparen: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreCallExpressionNode {
            lparen,
            rparen,
            function_name,
            params,
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

impl EnumVariantExprOrClassMethodCallNode {
    pub fn new(
        ty_name: IdentifierInUseNode,
        property_name: IdentifierInUseNode,
        params: Option<(
            TokenNode,
            Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
            TokenNode,
        )>,
        double_colon: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreEnumVariantExprOrClassMethodCallNode {
            double_colon,
            ty_name,
            property_name,
            params,
        });
        EnumVariantExprOrClassMethodCallNode(node)
    }

    impl_core_ref!(CoreEnumVariantExprOrClassMethodCallNode);
}

impl Node for EnumVariantExprOrClassMethodCallNode {
    fn range(&self) -> TextRange {
        match &self.core_ref().params {
            Some((_, _, rparen)) => impl_range!(self.core_ref().ty_name, rparen),
            None => impl_range!(self.core_ref().ty_name, self.core_ref().property_name),
        }
    }
    fn start_line_number(&self) -> usize {
        self.0.as_ref().ty_name.start_line_number()
    }
}

impl ArrayExpressionNode {
    pub fn new(
        lsquare: TokenNode,
        rsquare: TokenNode,
        initials: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
    ) -> Self {
        let node = Rc::new(CoreArrayExpressionNode {
            lsquare,
            rsquare,
            initials,
        });
        ArrayExpressionNode(node)
    }

    impl_core_ref!(CoreArrayExpressionNode);
}

impl Node for ArrayExpressionNode {
    fn range(&self) -> TextRange {
        impl_range!(self.core_ref().lsquare, self.core_ref().rsquare)
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().lsquare.start_line_number()
    }
}

impl KeyValuePairNode {
    pub fn new(key_expr: ExpressionNode, value_expr: ExpressionNode, colon: TokenNode) -> Self {
        let node = Rc::new(CoreKeyValuePairNode {
            key_expr,
            value_expr,
            colon,
        });
        KeyValuePairNode(node)
    }

    impl_core_ref!(CoreKeyValuePairNode);
}

impl Node for KeyValuePairNode {
    fn range(&self) -> TextRange {
        impl_range!(self.core_ref().key_expr, self.core_ref().value_expr)
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().key_expr.start_line_number()
    }
}

impl HashMapExpressionNode {
    pub fn new(
        lcurly: TokenNode,
        rcurly: TokenNode,
        initials: Option<SymbolSeparatedSequenceNode<KeyValuePairNode>>,
    ) -> Self {
        let node = Rc::new(CoreHashMapExpressionNode {
            lcurly,
            rcurly,
            initials,
        });
        HashMapExpressionNode(node)
    }

    impl_core_ref!(CoreHashMapExpressionNode);
}

impl Node for HashMapExpressionNode {
    fn range(&self) -> TextRange {
        impl_range!(self.core_ref().lcurly, self.core_ref().rcurly)
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().lcurly.start_line_number()
    }
}

impl TupleExpressionNode {
    pub fn new(
        lround: TokenNode,
        rround: TokenNode,
        initials: SymbolSeparatedSequenceNode<ExpressionNode>,
    ) -> Self {
        let node = Rc::new(CoreTupleExpressionNode {
            lround,
            rround,
            initials,
        });
        TupleExpressionNode(node)
    }

    impl_core_ref!(CoreTupleExpressionNode);
}

impl Node for TupleExpressionNode {
    fn range(&self) -> TextRange {
        impl_range!(self.core_ref().lround, self.core_ref().rround)
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().lround.start_line_number()
    }
}

impl AtomNode {
    pub fn new_with_atom_start(atom_start: AtomStartNode) -> Self {
        let node = Rc::new(CoreAtomNode::AtomStart(atom_start));
        AtomNode(node)
    }

    pub fn new_with_call(
        atom: AtomNode,
        params: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
        lparen: TokenNode,
        rparen: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreAtomNode::Call(CallNode::new(
            atom, params, lparen, rparen,
        )));
        AtomNode(node)
    }

    pub fn new_with_propertry_access(
        atom: AtomNode,
        propertry: IdentifierInUseNode,
        dot: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreAtomNode::PropertyAccess(PropertyAccessNode::new(
            atom, propertry, dot,
        )));
        AtomNode(node)
    }

    pub fn new_with_method_access(
        atom: AtomNode,
        method_name: IdentifierInUseNode,
        params: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
        lparen: TokenNode,
        rparen: TokenNode,
        dot: TokenNode,
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
        atom: AtomNode,
        index: ExpressionNode,
        lsquare: TokenNode,
        rsquare: TokenNode,
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
                atom.is_valid_l_value()
            }
            CoreAtomNode::PropertyAccess(atom_property_access_node) => {
                let atom = &atom_property_access_node.0.as_ref().atom;
                atom.is_valid_l_value()
            }
        }
    }

    impl_core_ref!(CoreAtomNode);
}

impl AtomStartNode {
    pub fn new_with_identifier(token: IdentifierInUseNode) -> Self {
        let node = Rc::new(CoreAtomStartNode::Identifier(token));
        AtomStartNode(node)
    }

    pub fn new_with_self_keyword(self_keyword: SelfKeywordNode) -> Self {
        let node = Rc::new(CoreAtomStartNode::SelfKeyword(self_keyword));
        AtomStartNode(node)
    }

    pub fn new_with_function_call(call_expr: CallExpressionNode) -> Self {
        let node = Rc::new(CoreAtomStartNode::Call(call_expr));
        AtomStartNode(node)
    }

    pub fn new_with_enum_variant_expr_or_class_method_call(
        ty_name: IdentifierInUseNode,
        property_name: IdentifierInUseNode,
        params: Option<(
            TokenNode,
            Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
            TokenNode,
        )>,
        double_colon: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreAtomStartNode::EnumVariantExprOrClassMethodCall(
            EnumVariantExprOrClassMethodCallNode::new(ty_name, property_name, params, double_colon),
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
        atom: AtomNode,
        params: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
        lparen: TokenNode,
        rparen: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreCallNode {
            atom,
            lparen,
            rparen,
            params,
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
    pub fn new(atom: AtomNode, propertry: IdentifierInUseNode, dot: TokenNode) -> Self {
        let node = Rc::new(CorePropertyAccessNode {
            dot,
            atom,
            propertry,
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
        atom: AtomNode,
        method_name: IdentifierInUseNode,
        params: Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
        lparen: TokenNode,
        rparen: TokenNode,
        dot: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreMethodAccessNode {
            lparen,
            rparen,
            dot,
            atom,
            method_name,
            params,
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
        atom: AtomNode,
        index: ExpressionNode,
        lsquare: TokenNode,
        rsquare: TokenNode,
    ) -> Self {
        let node = Rc::new(CoreIndexAccessNode {
            lsquare,
            rsquare,
            atom,
            index,
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

impl SelfKeywordNode {
    pub fn new_with_ok(token: OkTokenNode) -> Self {
        let node = Rc::new(CoreSelfKeywordNode::Ok(OkSelfKeywordNode::new(token)));
        SelfKeywordNode(node)
    }

    impl_core_ref!(CoreSelfKeywordNode);
}
default_errornous_node_impl!(SelfKeywordNode, CoreSelfKeywordNode);

impl OkSelfKeywordNode {
    pub fn new(token: OkTokenNode) -> Self {
        let node = Rc::new(CoreOkSelfKeywordNode { token });
        OkSelfKeywordNode(node)
    }

    pub fn token_value(&self, code: &JarvilCodeHandler, interner: &mut Interner) -> StrId {
        self.0.as_ref().token.token_value(code, interner)
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
    pub fn new_with_ok(token: Token) -> Self {
        let node = Rc::new(CoreTokenNode::Ok(OkTokenNode::new(token)));
        TokenNode(node)
    }

    pub fn is_binary_operator(&self) -> Option<BinaryOperatorKind> {
        match &self.0.as_ref() {
            CoreTokenNode::Ok(ok_token) => ok_token.is_binary_operator(),
            _ => None,
        }
    }

    impl_core_ref!(CoreTokenNode);
}
default_errornous_node_impl!(TokenNode, CoreTokenNode);

impl OkTokenNode {
    pub fn new(token: Token) -> Self {
        OkTokenNode(Rc::new(CoreOkTokenNode { token }))
    }

    pub fn is_binary_operator(&self) -> Option<BinaryOperatorKind> {
        self.core_ref().token.try_as_binary_operator()
    }

    pub fn token_value(&self, code: &JarvilCodeHandler, interner: &mut Interner) -> StrId {
        self.0.as_ref().token.token_value(code, interner)
    }

    pub fn token_value_str(&self, code: &JarvilCodeHandler) -> String {
        self.0.as_ref().token.token_value_str(code)
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

impl PartialEq for OkTokenNode {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for OkTokenNode {}

impl Hash for OkTokenNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr = Rc::as_ptr(&self.0);
        ptr.hash(state);
    }
}

impl MissingTokenNode {
    pub fn new(expected_symbols: Vec<&'static str>, received_token: Token) -> Self {
        let node = Rc::new(CoreMissingTokenNode {
            // NOTE: Below is traditionally an expensive clone but in our case,
            // mostly `expected_symbols.len()` is less so we avoid runtime overhead of using `Rc`
            // which ideally should be used if length is large for example: in `BlockNode`, see `stmts` field.
            expected_symbols,
            received_token,
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
    pub fn new(skipped_token: Token) -> Self {
        let node = Rc::new(CoreSkippedTokenNode { skipped_token });
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

impl IdentifierInUseNode {
    pub fn new_with_ok(
        token: OkTokenNode,
        generic_type_args: Option<(
            TokenNode,
            SymbolSeparatedSequenceNode<TypeExpressionNode>,
            TokenNode,
        )>,
    ) -> Self {
        let node = Rc::new(CoreIdentifierInUseNode::Ok(OkIdentifierInUseNode::new(
            token,
            generic_type_args,
        )));
        IdentifierInUseNode(node)
    }

    impl_core_ref!(CoreIdentifierInUseNode);
}
default_errornous_node_impl!(IdentifierInUseNode, CoreIdentifierInUseNode);

impl IdentifierInDeclNode {
    pub fn new_with_ok(
        token: OkTokenNode,
        generic_type_decls: Option<(
            TokenNode,
            SymbolSeparatedSequenceNode<GenericTypeDeclNode>,
            TokenNode,
        )>,
    ) -> Self {
        let node = Rc::new(CoreIdentifierInDeclNode::Ok(OkIdentifierInDeclNode::new(
            token,
            generic_type_decls,
        )));
        IdentifierInDeclNode(node)
    }

    impl_core_ref!(CoreIdentifierInDeclNode);
}
default_errornous_node_impl!(IdentifierInDeclNode, CoreIdentifierInDeclNode);

impl OkIdentifierInUseNode {
    fn new(
        token: OkTokenNode,
        generic_type_args: Option<(
            TokenNode,
            SymbolSeparatedSequenceNode<TypeExpressionNode>,
            TokenNode,
        )>,
    ) -> Self {
        let node = Rc::new(CoreOkIdentifierInUseNode {
            name: token,
            generic_type_args,
        });
        OkIdentifierInUseNode(node)
    }

    pub fn token_value(&self, code: &JarvilCodeHandler, interner: &mut Interner) -> StrId {
        self.0.as_ref().name.token_value(code, interner)
    }

    pub fn token_value_str(&self, code: &JarvilCodeHandler) -> String {
        self.0.as_ref().name.token_value_str(code)
    }

    impl_core_ref!(CoreOkIdentifierInUseNode);
}

impl Node for OkIdentifierInUseNode {
    fn range(&self) -> TextRange {
        match &self.core_ref().generic_type_args {
            Some((_, _, rangle)) => impl_range!(self.core_ref().name, rangle),
            None => self.core_ref().name.range(),
        }
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().name.start_line_number()
    }
}

impl PartialEq for OkIdentifierInUseNode {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for OkIdentifierInUseNode {}

impl Hash for OkIdentifierInUseNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr = Rc::as_ptr(&self.0);
        ptr.hash(state);
    }
}

impl OkIdentifierInDeclNode {
    fn new(
        token: OkTokenNode,
        generic_type_decls: Option<(
            TokenNode,
            SymbolSeparatedSequenceNode<GenericTypeDeclNode>,
            TokenNode,
        )>,
    ) -> Self {
        let node = Rc::new(CoreOkIdentifierInDeclNode {
            name: token,
            generic_type_decls,
        });
        OkIdentifierInDeclNode(node)
    }

    pub fn token_value(&self, code: &JarvilCodeHandler, interner: &mut Interner) -> StrId {
        self.0.as_ref().name.token_value(code, interner)
    }

    pub fn token_value_str(&self, code: &JarvilCodeHandler) -> String {
        self.0.as_ref().name.token_value_str(code).to_owned()
    }

    impl_core_ref!(CoreOkIdentifierInDeclNode);
}

impl Node for OkIdentifierInDeclNode {
    fn range(&self) -> TextRange {
        match &self.core_ref().generic_type_decls {
            Some((_, _, rangle)) => impl_range!(self.core_ref().name, rangle),
            None => self.core_ref().name.range(),
        }
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().name.start_line_number()
    }
}

impl PartialEq for OkIdentifierInDeclNode {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for OkIdentifierInDeclNode {}

impl Hash for OkIdentifierInDeclNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr = Rc::as_ptr(&self.0);
        ptr.hash(state);
    }
}

impl GenericTypeDeclNode {
    pub fn new(
        generic_type_name: IdentifierInDeclNode,
        interface_bounds: Option<(TokenNode, SymbolSeparatedSequenceNode<IdentifierInUseNode>)>,
    ) -> Self {
        let node = Rc::new(CoreGenericTypeDeclNode {
            generic_type_name,
            interface_bounds,
        });
        GenericTypeDeclNode(node)
    }

    impl_core_ref!(CoreGenericTypeDeclNode);
}

impl Node for GenericTypeDeclNode {
    fn range(&self) -> TextRange {
        match &self.core_ref().interface_bounds {
            Some((_, interfaces)) => impl_range!(self.core_ref().generic_type_name, interfaces),
            None => self.core_ref().generic_type_name.range(),
        }
    }
    fn start_line_number(&self) -> usize {
        self.core_ref().generic_type_name.start_line_number()
    }
}
