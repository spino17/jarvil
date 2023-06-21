use crate::ast::ast::{
    AssignmentNode, AtomNode, AtomStartNode, AtomicExpressionNode, BlockNode, CallableBodyNode,
    CallableKind, CallablePrototypeNode, ErrornousNode, ExpressionNode, GenericTypeDeclNode,
    IdentifierInDeclNode, IdentifierInUseNode, IdentifierNode, NameTypeSpecNode, Node, OkTokenNode,
    SelfKeywordNode, SkippedTokenNode, StatementNode, SymbolSeparatedSequenceNode, TokenNode,
    TypeDeclarationNode, TypeExpressionNode, UnaryExpressionNode, VariableDeclarationNode,
};
use crate::code::JarvilCode;
use crate::constants::common::{ENDMARKER, IDENTIFIER, SELF};
use crate::context;
use crate::error::diagnostics::{
    Diagnostics, IncorrectlyIndentedBlockError, InvalidLValueError, InvalidTrailingTokensError,
    MissingTokenError, NoValidStatementFoundInsideBlockBodyError, SingleSubTypeFoundInTupleError,
};
use crate::lexer::token::{CoreToken, Token};
use crate::parser::components;
use crate::parser::helper::{IndentResult, IndentResultKind};
use std::rc::Rc;
use text_size::TextRange;

pub trait Parser {
    fn parse(self, token_vec: Vec<Token>) -> (BlockNode, Vec<Diagnostics>, JarvilCode);
}

pub struct JarvilParser {
    token_vec: Vec<Token>,
    lookahead: usize,
    indent_level: i64,
    code: JarvilCode,
    ignore_all_errors: bool, // if this is set, no errors during parsing is saved inside error logs
    correction_indent: i64,
    errors: Vec<Diagnostics>,
}

impl JarvilParser {
    pub fn new(code: JarvilCode) -> Self {
        JarvilParser {
            token_vec: Vec::new(),
            lookahead: 0,
            indent_level: -1,
            code,
            ignore_all_errors: false,
            correction_indent: 0,
            errors: vec![],
        }
    }
}

impl Parser for JarvilParser {
    fn parse(mut self, token_vec: Vec<Token>) -> (BlockNode, Vec<Diagnostics>, JarvilCode) {
        let code_node = self.code(token_vec);
        (code_node, self.errors, self.code)
    }
}

impl JarvilParser {
    // ------------------- parsing utilities -------------------
    pub fn set_token_vec(&mut self, token_vec: Vec<Token>) {
        self.token_vec = token_vec;
    }

    pub fn scan_next_token(&mut self) {
        self.lookahead = self.lookahead + 1;
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
        self.correction_indent = self.correction_indent + addition;
    }

    pub fn curr_token(&mut self) -> Token {
        self.token_vec[self.lookahead].clone()
    }

    pub fn check_curr_token(&self, symbol: &str) -> bool {
        self.token_vec[self.lookahead].is_eq(symbol)
    }

    pub fn curr_token_precedence_and_name(&self) -> (u8, &'static str) {
        let token = &self.token_vec[self.lookahead];
        (token.get_precedence(), token.core_token.to_string())
    }

    pub fn is_curr_token_on_newline(&self) -> bool {
        if self.lookahead == 0 {
            return true;
        }
        if self.token_vec[self.lookahead - 1].is_eq("\n")
            || self.token_vec[self.lookahead].is_eq("\n")
            || self.token_vec[self.lookahead].is_eq(ENDMARKER)
        {
            true
        } else {
            false
        }
    }

    pub fn skip_to_newline(&mut self) -> Vec<SkippedTokenNode> {
        let mut skipped_tokens: Vec<SkippedTokenNode> = vec![];
        loop {
            let token = &self.token_vec[self.lookahead].clone();
            if token.is_eq("\n") {
                self.log_trailing_skipped_tokens_error(&skipped_tokens);
                skipped_tokens.push(SkippedTokenNode::new(&token));
                self.scan_next_token();
                return skipped_tokens;
            } else if token.is_eq(ENDMARKER) {
                self.log_trailing_skipped_tokens_error(&skipped_tokens);
                skipped_tokens.push(SkippedTokenNode::new(&token));
                // self.scan_next_token();
                return skipped_tokens;
            } else {
                skipped_tokens.push(SkippedTokenNode::new(&token));
                self.scan_next_token();
            }
        }
    }

    // ------------------- error logging utilities for terminal-based compilation -------------------
    pub fn log_missing_token_error(
        &mut self,
        expected_symbols: &[&'static str],
        received_token: &Token,
    ) {
        if self.ignore_all_errors {
            return;
        }
        // -> TODO - check whether error on same line already exists
        let err = MissingTokenError::new(expected_symbols, received_token);
        self.errors.push(Diagnostics::MissingToken(err));
    }

    pub fn log_trailing_skipped_tokens_error(&mut self, skipped_tokens: &Vec<SkippedTokenNode>) {
        if self.ignore_all_errors {
            return;
        }
        // -> TODO - check whether error on same line already exists
        let err = InvalidTrailingTokensError::new(
            skipped_tokens[0].range().start().into(),
            skipped_tokens[skipped_tokens.len() - 1]
                .range()
                .end()
                .into(),
        );
        self.errors.push(Diagnostics::InvalidTrailingTokens(err));
    }

    pub fn log_incorrectly_indented_block_error(
        &mut self,
        range: TextRange,
        expected_indent: i64,
        received_indent: i64,
    ) {
        if self.ignore_all_errors {
            return;
        }
        // -> TODO - check whether error on same line already exists
        let err = IncorrectlyIndentedBlockError::new(expected_indent, received_indent, range);
        self.errors.push(Diagnostics::IncorrectlyIndentedBlock(err));
    }

    pub fn log_no_valid_statement_inside_block_error(&mut self, range: TextRange) {
        if self.ignore_all_errors {
            return;
        }
        let err = NoValidStatementFoundInsideBlockBodyError::new(range);
        self.errors
            .push(Diagnostics::NoValidStatementFoundInsideBlockBody(err));
    }

    pub fn log_invalid_l_value_error(&mut self, range: TextRange) {
        if self.ignore_all_errors {
            return;
        }
        // -> TODO - check whether error on same line already exists
        let err = InvalidLValueError::new(range);
        self.errors.push(Diagnostics::InvalidLValue(err));
    }

    pub fn log_single_sub_type_in_tuple_error(&mut self, range: TextRange) {
        if self.ignore_all_errors {
            return;
        }
        let err = SingleSubTypeFoundInTupleError::new(range);
        self.errors
            .push(Diagnostics::SingleSubTypeFoundInTuple(err));
    }

    // ------------------- parsing routines for terminals and block indentation -------------------
    pub fn expect(&mut self, symbol: &'static str) -> TokenNode {
        let token = self.curr_token();
        if token.is_eq(symbol) {
            self.scan_next_token();
            TokenNode::new_with_ok(&token)
        } else {
            self.log_missing_token_error(&[symbol], &token);
            TokenNode::new_with_missing_tokens(&Rc::new(vec![symbol]), &token)
        }
    }

    pub fn expect_symbol_separated_sequence<T: Clone, U: Fn(&mut JarvilParser) -> T>(
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
                &first_entity_node,
                &remaining_generic_type_args_node,
                &separator_node,
            );
        }
        return SymbolSeparatedSequenceNode::new_with_single_entity(&first_entity_node);
    }

    pub fn expect_generic_type_args(&mut self) -> SymbolSeparatedSequenceNode<TypeExpressionNode> {
        return self.expect_symbol_separated_sequence(|parser: &mut JarvilParser| {
            return parser.type_expr();
        }, ",");
    }

    pub fn expect_generic_type_decls(
        &mut self,
    ) -> SymbolSeparatedSequenceNode<GenericTypeDeclNode> {
        let parsing_fn = |parser: &mut JarvilParser| {
            let identifier_in_decl_node = parser.expect_identifier();
            let token = parser.curr_token();
            match token.core_token {
                CoreToken::COLON => {
                    let colon_node = parser.expect(":");
                    let interface_bounds_node = parser.expect_symbol_separated_sequence(
                        |parser: &mut JarvilParser| {
                            return parser.expect_identifier_in_use();
                        },
                        "+",
                    );
                    return GenericTypeDeclNode::new(
                        &identifier_in_decl_node,
                        Some((&colon_node, &interface_bounds_node)),
                    );
                }
                _ => {
                    return GenericTypeDeclNode::new(&identifier_in_decl_node, None);
                }
            }
        };
        return self.expect_symbol_separated_sequence(parsing_fn, ",");
    }

    pub fn expect_identifier(&mut self) -> IdentifierInDeclNode {
        let token = self.curr_token();
        let symbol = IDENTIFIER;
        if token.is_eq(symbol) {
            self.scan_next_token();
            let ok_token_node = OkTokenNode::new(&token);
            return IdentifierInDeclNode::new_with_ok(&ok_token_node, None);
        } else {
            self.log_missing_token_error(&[symbol], &token);
            return IdentifierInDeclNode::new_with_missing_tokens(&Rc::new(vec![symbol]), &token);
        }
    }

    pub fn expect_identifier_in_use(&mut self) -> IdentifierInUseNode {
        let token = self.curr_token();
        let symbol = IDENTIFIER;
        if token.is_eq(symbol) {
            self.scan_next_token();
            let ok_token_node = OkTokenNode::new(&token);
            let next_token = self.curr_token();
            match next_token.core_token {
                CoreToken::LBRACKET => {
                    let langle_node = self.expect("<");
                    let generic_type_args_node = self.expect_generic_type_args();
                    let rangle_node = self.expect(">");
                    return IdentifierInUseNode::new_with_ok(
                        &ok_token_node,
                        Some((&langle_node, &generic_type_args_node, &rangle_node)),
                    );
                }
                _ => {
                    return IdentifierInUseNode::new_with_ok(&ok_token_node, None);
                }
            }
        } else {
            self.log_missing_token_error(&[symbol], &token);
            IdentifierInUseNode::new_with_missing_tokens(&Rc::new(vec![symbol]), &token)
        }
    }

    pub fn expect_identifier_in_decl(&mut self) -> IdentifierInDeclNode {
        let token = self.curr_token();
        let symbol = IDENTIFIER;
        if token.is_eq(symbol) {
            self.scan_next_token();
            let ok_token_node = OkTokenNode::new(&token);
            let next_token = self.curr_token();
            match next_token.core_token {
                CoreToken::LBRACKET => {
                    let langle_node = self.expect("<");
                    let generic_type_decls_node = self.expect_generic_type_decls();
                    let rangle_node = self.expect(">");
                    return IdentifierInDeclNode::new_with_ok(
                        &ok_token_node,
                        Some((&langle_node, &generic_type_decls_node, &rangle_node)),
                    );
                }
                _ => {
                    return IdentifierInDeclNode::new_with_ok(&ok_token_node, None);
                }
            }
        } else {
            self.log_missing_token_error(&[symbol], &token);
            return IdentifierInDeclNode::new_with_missing_tokens(&Rc::new(vec![symbol]), &token);
        }
    }

    pub fn expect_ident(&mut self) -> IdentifierNode {
        let token = self.curr_token();
        let symbol = IDENTIFIER;
        if token.is_eq(symbol) {
            self.scan_next_token();
            let ok_token_node = OkTokenNode::new(&token);
            IdentifierNode::new_with_ok(&ok_token_node)
        } else {
            self.log_missing_token_error(&[symbol], &token);
            IdentifierNode::new_with_missing_tokens(&Rc::new(vec![symbol]), &token)
        }
    }

    pub fn expect_self(&mut self) -> SelfKeywordNode {
        let token = self.curr_token();
        let symbol = SELF;
        if token.is_eq(symbol) {
            self.scan_next_token();
            let ok_token_node = OkTokenNode::new(&token);
            SelfKeywordNode::new_with_ok(&ok_token_node)
        } else {
            self.log_missing_token_error(&[symbol], &token);
            SelfKeywordNode::new_with_missing_tokens(&Rc::new(vec![symbol]), &token)
        }
    }

    pub fn expect_terminators(&mut self) -> TokenNode {
        let symbols = &["\n", ENDMARKER];
        let token = self.curr_token();
        if token.is_eq("\n") || token.is_eq(ENDMARKER) {
            self.scan_next_token();
            return TokenNode::new_with_ok(&token);
        } else {
            self.log_missing_token_error(symbols, &token);
            return TokenNode::new_with_missing_tokens(&Rc::new(symbols.to_vec()), &token);
        }
    }

    pub fn expect_indent_spaces(&mut self) -> IndentResult {
        let mut skipped_tokens: Vec<SkippedTokenNode> = vec![];
        let mut extra_newlines: Vec<SkippedTokenNode> = vec![];
        if !self.is_curr_token_on_newline() {
            skipped_tokens = self.skip_to_newline();
        }
        let mut expected_indent_spaces = context::indent_spaces() as i64 * self.indent_level;
        let mut indent_spaces = 0;
        loop {
            let token = &self.token_vec[self.lookahead];
            match &token.core_token {
                CoreToken::NEWLINE => {
                    extra_newlines.push(SkippedTokenNode::new(token));
                    indent_spaces = 0;
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
                    indent_spaces = (token.start_index()
                        - self.code.get_line_start_index(token.line_number))
                        as i64;
                    let alternate_line_index = match &token.trivia {
                        Some(trivia) => {
                            // this index is bounded as we only have `Some` trivia if it's length > 0
                            trivia[0].start_index()
                        }
                        None => token.start_index(),
                    };
                    // assert!(alternate_line_index == self.code.get_line_start_index(token.line_number));
                    expected_indent_spaces = expected_indent_spaces + self.correction_indent();
                    if indent_spaces == expected_indent_spaces {
                        return IndentResult {
                            kind: IndentResultKind::CorrectIndentation,
                            skipped_tokens,
                            extra_newlines,
                        };
                    } else if indent_spaces > expected_indent_spaces {
                        return IndentResult {
                            kind: IndentResultKind::IncorrectIndentation((
                                expected_indent_spaces,
                                indent_spaces,
                            )),
                            skipped_tokens,
                            extra_newlines,
                        };
                    } else {
                        return IndentResult {
                            kind: IndentResultKind::BlockOver,
                            skipped_tokens,
                            extra_newlines,
                        };
                    }
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
    ) -> BlockNode {
        components::block::block(
            self,
            is_starting_with_fn,
            statement_parsing_fn,
            expected_symbols,
        )
    }

    pub fn stmt(&mut self) -> StatementNode {
        components::statement::core::stmt(self)
    }

    pub fn assignment(&mut self, expr: &ExpressionNode) -> AssignmentNode {
        components::assignment::assignment(self, expr)
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

    pub fn callable_body(&mut self) -> CallableBodyNode {
        components::common::callable_body(self)
    }

    pub fn struct_stmt(&mut self) -> StatementNode {
        components::statement::core::struct_stmt(self)
    }

    pub fn type_decl(&mut self) -> TypeDeclarationNode {
        components::type_declaration::type_decl(self)
    }
}
