// jarvil parser is designed to be IDE-first (and not terminal-first), meaning it always build the AST even if there are errors 
// in the code. To deal with such kind of robust error-tolerant recovery while parsing we use an approach which is popularized by 
// Microsoft and has been used in their technologies like Roslyn, TypeScript and tolerant-php-parser.
// See `https://github.com/microsoft/tolerant-php-parser/blob/main/docs/HowItWorks.md` for more information.
// Below are similar docs for rust and swift:
// Rust  - `https://github.com/rust-lang/rust-analyzer/blob/1d53f695f0408f47c5cce5cefa471eb0e86b0db7/docs/dev/guide.md`
// Swift - `https://github.com/apple/swift/tree/5e2c815edfd758f9b1309ce07bfc01c4bc20ec23/lib/Syntax`

use crate::ast::ast::{BlockNode, StatemenIndentWrapper, SkippedTokens, SkippedTokenNode, StatementNode};
use crate::constants::common::ENDMARKER;
use crate::parser::helper::{IndentResultKind};
use crate::parser::parser::{PackratParser};
use std::rc::Rc;
use std::mem;
use std::cell::RefCell;
use crate::lexer::token::Token;

pub fn block<F: Fn(&Token) -> bool, G: Fn(&mut PackratParser) -> StatementNode>(parser: &mut PackratParser, 
    is_starting_with_fn: F, statement_parsing_fn: G, expected_symbols: &[&'static str]) -> BlockNode {
    let newline_node = parser.expect("\n");
    parser.set_indent_level(parser.curr_indent_level() + 1);
    let stmts_vec: Rc<RefCell<Vec<StatemenIndentWrapper>>> = Rc::new(RefCell::new(vec![]));
    let mut leading_skipped_tokens: Vec<SkippedTokenNode> = vec![];
    loop {
        let indent_result = parser.expect_indent_spaces();
        let skipped_tokens = indent_result.skipped_tokens;
        if skipped_tokens.len() > 0 {
            stmts_vec.as_ref().borrow_mut().push(StatemenIndentWrapper::TRAILING_SKIPPED_TOKENS(
                SkippedTokens::new_with_trailing_skipped_tokens(&Rc::new(skipped_tokens))
            ));
        }
        let extra_newlines = indent_result.extra_newlines;
        if extra_newlines.len() > 0 {
            stmts_vec.as_ref().borrow_mut().push(StatemenIndentWrapper::EXTRA_NEWLINES(
                SkippedTokens::new_with_extra_newlines(&Rc::new(extra_newlines))
            ));
        }
        let incorrect_indent_data
        = match indent_result.kind {
            IndentResultKind::CORRECT_INDENTATION => None,
            IndentResultKind::INCORRECT_INDENTATION(indent_data) => Some(indent_data),
            IndentResultKind::BLOCK_OVER => {
                parser.set_indent_level(parser.curr_indent_level() - 1);
                return BlockNode::new(&stmts_vec, &newline_node)
            }
        };
        while !is_starting_with_fn(&parser.curr_token()) {
            let token = &parser.curr_token();
            leading_skipped_tokens.push(SkippedTokenNode::new(token, parser.curr_lookahead()));
            parser.log_missing_token_error_for_multiple_expected_symbols(expected_symbols, token);
            if token.is_eq("\n") || token.is_eq(ENDMARKER) {
                break;
            }
            parser.scan_next_token();
        }
        if leading_skipped_tokens.len() > 0 {
            stmts_vec.as_ref().borrow_mut().push(StatemenIndentWrapper::LEADING_SKIPPED_TOKENS(
                SkippedTokens::new_with_leading_skipped_tokens(&Rc::new(mem::take(&mut leading_skipped_tokens)))
            ));
        }
        let token = &parser.curr_token();
        if token.is_eq(ENDMARKER) {
            parser.set_indent_level(parser.curr_indent_level() - 1);
            return BlockNode::new(&stmts_vec, &newline_node)
        }
        if token.is_eq("\n") {
            continue;
        }
        match incorrect_indent_data {
            Some(indent_data) => {
                let stmt_node = if parser.is_ignore_all_errors() {
                    // a sub stmt of already incorrectly indented stmt some levels higher
                    let saved_correction_indent = parser.correction_indent();
                    parser.add_to_correction_indent(indent_data.1 - indent_data.0);
                    let stmt_node = statement_parsing_fn(parser);
                    parser.set_correction_indent(saved_correction_indent);
                    stmt_node
                } else {
                    // the highest level incorrectly indented stmt
                    parser.set_ignore_all_errors(true);
                    let before_line_number = parser.curr_line_number();
                    parser.set_correction_indent(indent_data.1 - indent_data.0);
                    let stmt_node = statement_parsing_fn(parser);
                    parser.set_ignore_all_errors(false);
                    let mut after_line_number = parser.curr_line_number();
                    parser.set_correction_indent(0);
                    if after_line_number > before_line_number {
                        after_line_number = after_line_number - 1;
                    }
                    parser.log_incorrectly_indented_block_error(before_line_number, 
                        after_line_number, indent_data.0, indent_data.1);
                    stmt_node
                };
                stmts_vec.as_ref().borrow_mut().push(StatemenIndentWrapper::INCORRECTLY_INDENTED((stmt_node, indent_data)));
            },
            None => {
                let stmt_node = statement_parsing_fn(parser);
                stmts_vec.as_ref().borrow_mut().push(StatemenIndentWrapper::CORRECTLY_INDENTED(stmt_node));
            }
        }
    }
}