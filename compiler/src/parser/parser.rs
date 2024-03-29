use super::resolver::BlockKind;
use crate::ast::ast::{
    AssignmentNode, AtomNode, AtomStartNode, AtomicExpressionNode, BlockNode, CallableBodyNode,
    CallableKind, CallablePrototypeNode, ConditionalBlockNode, ConditionalStatementNode,
    ExpressionNode, ForLoopStatementNode, GenericTypeDeclNode, IdentifierInDeclNode,
    IdentifierInUseNode, InterfaceDeclarationNode, InterfaceMethodPrototypeWrapperNode,
    MatchCaseStatementNode, NameTypeSpecNode, OkTokenNode, SelfKeywordNode, SkippedTokenNode,
    StatementNode, SymbolSeparatedSequenceNode, TokenNode, TypeDeclarationNode, TypeExpressionNode,
    UnaryExpressionNode, VariableDeclarationNode, WhileLoopStatementNode,
};
use crate::ast::traits::{ErrornousNode, Node};
use crate::code::JarvilCodeHandler;
use crate::constants::common::{ENDMARKER, IDENTIFIER, SELF};
use crate::context;
use crate::error::diagnostics::Diagnostics;
use crate::lexer::token::{CoreToken, Token};
use crate::parser::components;
use crate::parser::helper::{IndentResult, IndentResultKind};
use serde::Serialize;
use std::cell::RefCell;

pub struct JarvilParser<'ctx> {
    token_vec: Vec<Token>,
    lookahead: usize,
    indent_level: i64,
    code_handler: &'ctx JarvilCodeHandler<'ctx>,
    ignore_all_errors: bool, // if this is set, no errors during parsing is saved inside error logs
    correction_indent: i64,
    errors: RefCell<Vec<Diagnostics>>,
}

impl<'ctx> JarvilParser<'ctx> {
    pub fn new(code_handler: &'ctx JarvilCodeHandler<'ctx>) -> Self {
        JarvilParser {
            token_vec: Vec::new(),
            lookahead: 0,
            indent_level: -1,
            code_handler,
            ignore_all_errors: false,
            correction_indent: 0,
            errors: RefCell::new(vec![]),
        }
    }

    pub fn parse(mut self, token_vec: Vec<Token>) -> (BlockNode, Vec<Diagnostics>) {
        let code_node = self.code(token_vec);
        (code_node, self.errors.into_inner())
    }

    pub fn ignore_all_errors(&self) -> bool {
        self.ignore_all_errors
    }

    pub fn log_error(&self, err: Diagnostics) {
        self.errors.borrow_mut().push(err);
    }
}

impl<'ctx> JarvilParser<'ctx> {
    // ------------------- parsing utilities -------------------
    pub fn set_token_vec(&mut self, token_vec: Vec<Token>) {
        self.token_vec = token_vec;
    }

    pub fn scan_next_token(&mut self) {
        self.lookahead += 1;
    }

    pub fn curr_indent_level(&self) -> i64 {
        self.indent_level
    }

    pub fn set_indent_level(&mut self, reset_indent: i64) {
        self.indent_level = reset_indent;
    }

    pub fn is_ignore_all_errors(&self) -> bool {
        self.ignore_all_errors
    }

    pub fn set_ignore_all_errors(&mut self, is_ignore_all_errors: bool) {
        self.ignore_all_errors = is_ignore_all_errors;
    }

    pub fn correction_indent(&self) -> i64 {
        self.correction_indent
    }

    pub fn set_correction_indent(&mut self, correction_indent: i64) {
        self.correction_indent = correction_indent;
    }

    pub fn add_to_correction_indent(&mut self, addition: i64) {
        self.correction_indent += addition;
    }

    pub fn curr_token(&self) -> &Token {
        &self.token_vec[self.lookahead]
    }

    pub fn check_curr_token(&self, symbol: &str) -> bool {
        self.token_vec[self.lookahead].is_eq(symbol)
    }

    pub fn curr_token_precedence_and_name(&self) -> (u8, &'static str) {
        let token = &self.token_vec[self.lookahead];
        (token.precedence(), token.core_token().to_string())
    }

    pub fn is_curr_token_on_newline(&self) -> bool {
        if self.lookahead == 0 {
            return true;
        }
        self.token_vec[self.lookahead - 1].is_eq("\n")
            || self.token_vec[self.lookahead].is_eq("\n")
            || self.token_vec[self.lookahead].is_eq(ENDMARKER)
    }

    pub fn skip_to_newline(&mut self) -> Vec<SkippedTokenNode> {
        let mut skipped_tokens: Vec<SkippedTokenNode> = vec![];
        loop {
            let token = self.curr_token();
            if token.is_eq("\n") {
                self.log_trailing_skipped_tokens_error(skipped_tokens.clone());
                skipped_tokens.push(SkippedTokenNode::new(token.clone()));
                self.scan_next_token();
                return skipped_tokens;
            } else if token.is_eq(ENDMARKER) {
                self.log_trailing_skipped_tokens_error(skipped_tokens.clone());
                skipped_tokens.push(SkippedTokenNode::new(token.clone()));
                return skipped_tokens;
            } else {
                skipped_tokens.push(SkippedTokenNode::new(token.clone()));
                self.scan_next_token();
            }
        }
    }

    // ------------------- parsing routines for terminals and block indentation -------------------
    pub fn expect(&mut self, symbol: &'static str) -> TokenNode {
        let token = self.curr_token();
        if token.is_eq(symbol) {
            let token_node = TokenNode::new_with_ok(token.clone());
            self.scan_next_token();
            token_node
        } else {
            self.log_missing_token_error(&[symbol], token);
            TokenNode::new_with_missing_tokens(vec![symbol], token.clone())
        }
    }

    pub fn expect_symbol_separated_sequence<
        T: Node + Serialize + Clone,
        U: Fn(&mut JarvilParser) -> T,
    >(
        &mut self,
        entity_parsing_fn: U,
        separator: &'static str,
    ) -> SymbolSeparatedSequenceNode<T> {
        let first_entity_node = entity_parsing_fn(self);
        let token = self.curr_token();
        if token.is_eq(separator) {
            let separator_node = self.expect(separator);
            let remaining_generic_type_args_node =
                self.expect_symbol_separated_sequence(entity_parsing_fn, separator);
            return SymbolSeparatedSequenceNode::new_with_entities(
                first_entity_node,
                remaining_generic_type_args_node,
                separator_node,
            );
        }
        SymbolSeparatedSequenceNode::new_with_single_entity(first_entity_node)
    }

    pub fn expect_generic_type_args(&mut self) -> SymbolSeparatedSequenceNode<TypeExpressionNode> {
        self.expect_symbol_separated_sequence(|parser: &mut JarvilParser| parser.type_expr(), ",")
    }

    pub fn expect_generic_type_decls(
        &mut self,
    ) -> SymbolSeparatedSequenceNode<GenericTypeDeclNode> {
        let parsing_fn = |parser: &mut JarvilParser| {
            let identifier_in_decl_node = parser.expect_identifier();
            let token = parser.curr_token();
            match token.core_token() {
                CoreToken::COLON => {
                    let colon_node = parser.expect(":");
                    let interface_bounds_node = parser.expect_symbol_separated_sequence(
                        |parser: &mut JarvilParser| parser.expect_identifier_in_use(),
                        "+",
                    );
                    GenericTypeDeclNode::new(
                        identifier_in_decl_node,
                        Some((colon_node, interface_bounds_node)),
                    )
                }
                _ => GenericTypeDeclNode::new(identifier_in_decl_node, None),
            }
        };
        self.expect_symbol_separated_sequence(parsing_fn, ",")
    }

    pub fn expect_identifier_in<
        T,
        U: Node + Serialize + Clone,
        F: Fn(&mut JarvilParser) -> SymbolSeparatedSequenceNode<U>,
        V: Fn(OkTokenNode, Option<(TokenNode, SymbolSeparatedSequenceNode<U>, TokenNode)>) -> T,
        W: Fn(Vec<&'static str>, Token) -> T,
    >(
        &mut self,
        angle_bracketed_content_parsing_fn: F,
        node_creation_method_with_some: V,
        node_creation_method_with_err: W,
    ) -> T {
        let token = self.curr_token();
        let symbol = IDENTIFIER;
        if token.is_eq(symbol) {
            let ok_token_node = OkTokenNode::new(token.clone());
            self.scan_next_token();
            let next_token = self.curr_token();
            match next_token.core_token() {
                CoreToken::LBRACKET => {
                    let langle_node = self.expect("<");
                    let angle_bracketed_content_node = angle_bracketed_content_parsing_fn(self);
                    let rangle_node = self.expect(">");
                    node_creation_method_with_some(
                        ok_token_node,
                        Some((langle_node, angle_bracketed_content_node, rangle_node)),
                    )
                }
                _ => node_creation_method_with_some(ok_token_node, None),
            }
        } else {
            self.log_missing_token_error(&[symbol], token);
            node_creation_method_with_err(vec![symbol], token.clone())
        }
    }

    pub fn expect_identifier(&mut self) -> IdentifierInDeclNode {
        let token = self.curr_token();
        let symbol = IDENTIFIER;
        if token.is_eq(symbol) {
            let ok_token_node = OkTokenNode::new(token.clone());
            self.scan_next_token();
            IdentifierInDeclNode::new_with_ok(ok_token_node, None)
        } else {
            self.log_missing_token_error(&[symbol], token);
            IdentifierInDeclNode::new_with_missing_tokens(vec![symbol], token.clone())
        }
    }

    pub fn expect_identifier_in_use(&mut self) -> IdentifierInUseNode {
        let angle_bracketed_content_parsing_fn = |parser: &mut JarvilParser| -> SymbolSeparatedSequenceNode<
            TypeExpressionNode,
        > { parser.expect_generic_type_args() };
        let node_creation_method_with_some = |ident_name: OkTokenNode,
                                              symbol_separated_sequence: Option<(
            TokenNode,
            SymbolSeparatedSequenceNode<TypeExpressionNode>,
            TokenNode,
        )>| {
            IdentifierInUseNode::new_with_ok(ident_name, symbol_separated_sequence)
        };
        let node_creation_method_with_err =
            |expected_symbols: Vec<&'static str>, received_token: Token| {
                IdentifierInUseNode::new_with_missing_tokens(expected_symbols, received_token)
            };
        self.expect_identifier_in(
            angle_bracketed_content_parsing_fn,
            node_creation_method_with_some,
            node_creation_method_with_err,
        )
    }

    pub fn expect_identifier_in_decl(&mut self) -> IdentifierInDeclNode {
        let angle_bracketed_content_parsing_fn = |parser: &mut JarvilParser| -> SymbolSeparatedSequenceNode<
            GenericTypeDeclNode,
        > { parser.expect_generic_type_decls() };
        let node_creation_method_with_some = |ident_name: OkTokenNode,
                                              symbol_separated_sequence: Option<(
            TokenNode,
            SymbolSeparatedSequenceNode<GenericTypeDeclNode>,
            TokenNode,
        )>| {
            IdentifierInDeclNode::new_with_ok(ident_name, symbol_separated_sequence)
        };
        let node_creation_method_with_err =
            |expected_symbols: Vec<&'static str>, received_token: Token| {
                IdentifierInDeclNode::new_with_missing_tokens(expected_symbols, received_token)
            };
        self.expect_identifier_in(
            angle_bracketed_content_parsing_fn,
            node_creation_method_with_some,
            node_creation_method_with_err,
        )
    }

    pub fn expect_self(&mut self) -> SelfKeywordNode {
        let token = self.curr_token();
        let symbol = SELF;
        if token.is_eq(symbol) {
            let ok_token_node = OkTokenNode::new(token.clone());
            self.scan_next_token();
            SelfKeywordNode::new_with_ok(ok_token_node)
        } else {
            self.log_missing_token_error(&[symbol], token);
            SelfKeywordNode::new_with_missing_tokens(vec![symbol], token.clone())
        }
    }

    pub fn expect_terminators(&mut self) -> TokenNode {
        let symbols = &["\n", ENDMARKER];
        let token = self.curr_token();
        if token.is_eq("\n") {
            let token_node = TokenNode::new_with_ok(token.clone());
            self.scan_next_token();
            token_node
        } else if token.is_eq(ENDMARKER) {
            return TokenNode::new_with_ok(token.clone());
        } else {
            self.log_missing_token_error(symbols, token);
            return TokenNode::new_with_missing_tokens(symbols.to_vec(), token.clone());
        }
    }

    pub fn expect_indent_spaces(&mut self) -> IndentResult {
        let mut skipped_tokens: Vec<SkippedTokenNode> = vec![];
        let mut extra_newlines: Vec<SkippedTokenNode> = vec![];
        if !self.is_curr_token_on_newline() {
            skipped_tokens = self.skip_to_newline();
        }
        let mut expected_indent_spaces = context::indent_spaces() as i64 * self.indent_level;
        loop {
            let token = &self.token_vec[self.lookahead];
            match &token.core_token() {
                CoreToken::NEWLINE => {
                    extra_newlines.push(SkippedTokenNode::new(token.clone()));
                }
                CoreToken::ENDMARKER => {
                    return IndentResult {
                        kind: IndentResultKind::BlockOver,
                        skipped_tokens,
                        extra_newlines,
                    }
                }
                _ => {
                    // At this point we are sure that the token index is set to the first token on a newline
                    let indent_spaces = (token.start_index()
                        - self.code_handler.line_start_index(token.line_number()))
                        as i64;
                    //let alternate_line_index = match &token.trivia {
                    //    Some(trivia) => {
                    //        // this index is bounded as we only have `Some` trivia if it's length > 0
                    //        trivia[0].start_index()
                    //    }
                    //    None => token.start_index(),
                    //};
                    // debug_assert!(alternate_line_index == self.code.get_line_start_index(token.line_number));
                    expected_indent_spaces += self.correction_indent();
                    if indent_spaces == expected_indent_spaces {
                        return IndentResult {
                            kind: IndentResultKind::CorrectIndentation,
                            skipped_tokens,
                            extra_newlines,
                        };
                    }
                    if indent_spaces > expected_indent_spaces {
                        return IndentResult {
                            kind: IndentResultKind::IncorrectIndentation((
                                expected_indent_spaces,
                                indent_spaces,
                            )),
                            skipped_tokens,
                            extra_newlines,
                        };
                    }
                    return IndentResult {
                        kind: IndentResultKind::BlockOver,
                        skipped_tokens,
                        extra_newlines,
                    };
                }
            }
            self.scan_next_token();
        }
    }

    // ------------------- production rule matching function for terminals and non-terminals -------------------
    pub fn code(&mut self, token_vec: Vec<Token>) -> BlockNode {
        components::code::code(self, token_vec)
    }

    pub fn block<F: Fn(&Token) -> bool, G: Fn(&mut JarvilParser) -> StatementNode>(
        &mut self,
        is_starting_with_fn: F,
        statement_parsing_fn: G,
        expected_symbols: &[&'static str],
        kind: BlockKind,
    ) -> BlockNode {
        components::block::block(
            self,
            is_starting_with_fn,
            statement_parsing_fn,
            expected_symbols,
            kind,
        )
    }

    pub fn stmt(&mut self) -> StatementNode {
        components::statement::core::stmt(self)
    }

    pub fn assignment(&mut self, expr: ExpressionNode) -> AssignmentNode {
        components::assignment::assignment(self, expr)
    }

    pub fn conditional(&mut self) -> ConditionalStatementNode {
        components::conditional::conditional(self)
    }

    pub fn conditional_block(
        &mut self,
        conditional_keyword_str: &'static str,
    ) -> ConditionalBlockNode {
        components::conditional::conditional_block(self, conditional_keyword_str)
    }

    pub fn type_expr(&mut self) -> TypeExpressionNode {
        components::expression::type_expression::type_expr(self)
    }

    pub fn atomic_expr(&mut self) -> AtomicExpressionNode {
        components::expression::core::atomic_expr(self)
    }

    pub fn unary_expr(&mut self) -> UnaryExpressionNode {
        components::expression::core::unary_expr(self)
    }

    pub fn expr(&mut self) -> ExpressionNode {
        components::expression::core::expr(self)
    }

    pub fn pratt_expr(&mut self, precedence: u8) -> ExpressionNode {
        components::expression::pratt::pratt_expr(self, precedence)
    }

    pub fn infix(
        &mut self,
        left_expr: &ExpressionNode,
        operator_node: &TokenNode,
        operator_precedence: u8,
    ) -> ExpressionNode {
        components::expression::pratt::infix(self, left_expr, operator_node, operator_precedence)
    }

    pub fn infix_comparison_expr(
        &mut self,
        left_expr: &ExpressionNode,
        operator_node: &TokenNode,
        operator_precedence: u8,
    ) -> ExpressionNode {
        components::expression::pratt::infix_comparison_expr(
            self,
            left_expr,
            operator_node,
            operator_precedence,
        )
    }

    pub fn infix_binary_expr(
        &mut self,
        left_expr: &ExpressionNode,
        operator_node: &TokenNode,
        operator_precedence: u8,
    ) -> ExpressionNode {
        components::expression::pratt::infix_binary_expr(
            self,
            left_expr,
            operator_node,
            operator_precedence,
        )
    }

    pub fn trailing_atom(&mut self, atom_start: AtomNode) -> AtomNode {
        components::expression::atom::trailing_atom(self, atom_start)
    }

    pub fn atom(&mut self) -> AtomNode {
        components::expression::atom::atom(self)
    }

    pub fn atom_start(&mut self) -> AtomStartNode {
        components::expression::atom::atom_start(self)
    }

    pub fn params(&mut self) -> SymbolSeparatedSequenceNode<ExpressionNode> {
        components::expression::common::params(self)
    }

    pub fn variable_decl(&mut self) -> VariableDeclarationNode {
        components::variable_declaration::variable_decl(self)
    }

    pub fn name_type_spec(&mut self) -> NameTypeSpecNode {
        components::common::name_type_spec(self)
    }

    pub fn name_type_specs(&mut self) -> SymbolSeparatedSequenceNode<NameTypeSpecNode> {
        components::common::name_type_specs(self)
    }

    pub fn type_tuple(&mut self) -> (SymbolSeparatedSequenceNode<TypeExpressionNode>, usize) {
        components::common::type_tuple(self)
    }

    pub fn function_stmt(&mut self, callable_kind: CallableKind) -> StatementNode {
        components::common::function_stmt(self, callable_kind)
    }

    pub fn callable_prototype(&mut self) -> CallablePrototypeNode {
        components::common::callable_prototype(self)
    }

    pub fn callable_body(&mut self, block_kind: BlockKind) -> CallableBodyNode {
        components::common::callable_body(self, block_kind)
    }

    pub fn struct_stmt(&mut self) -> StatementNode {
        components::statement::core::struct_stmt(self)
    }

    pub fn enum_stmt(&mut self) -> StatementNode {
        components::statement::core::enum_stmt(self)
    }

    pub fn case_branch_stmt(&mut self) -> StatementNode {
        components::statement::core::case_branch_stmt(self)
    }

    pub fn match_case(&mut self) -> MatchCaseStatementNode {
        components::match_case::match_case(self)
    }

    pub fn while_loop_stmt(&mut self) -> WhileLoopStatementNode {
        components::loops::while_loop_stmt(self)
    }

    pub fn for_loop_stmt(&mut self) -> ForLoopStatementNode {
        components::loops::for_loop_stmt(self)
    }

    pub fn interface_stmt(&mut self) -> StatementNode {
        components::statement::core::interface_stmt(self)
    }

    pub fn interface_method_prototype_wrapper(&mut self) -> InterfaceMethodPrototypeWrapperNode {
        components::interface_declaration::interface_method_prototype_wrapper(self)
    }

    pub fn interface_decl(&mut self) -> InterfaceDeclarationNode {
        components::interface_declaration::interface_decl(self)
    }

    pub fn type_decl(&mut self) -> TypeDeclarationNode {
        components::type_declaration::type_decl(self)
    }
}
