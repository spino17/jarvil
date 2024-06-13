// Jarvil parser is designed to be IDE-first (and not terminal-first), meaning it always build the AST even if there are errors
// in the code. To deal with such kind of robust error-tolerant recovery while parsing we use an approach which is popularized by
// Microsoft and has been used in their technologies like Roslyn, TypeScript and tolerant-php-parser.
// See `https://github.com/microsoft/tolerant-php-parser/blob/main/docs/HowItWorks.md` for more information.
// Below are similar docs for rust and swift:
// Rust  - `https://github.com/rust-lang/rust-analyzer/blob/1d53f695f0408f47c5cce5cefa471eb0e86b0db7/docs/dev/guide.md`
// Swift - `https://github.com/apple/swift/tree/5e2c815edfd758f9b1309ce07bfc01c4bc20ec23/lib/Syntax`

// block_kind: statement, struct, interface, implementation of struct, function

use crate::ast::ast::{
    BlockNode, SkippedTokenNode, SkippedTokensNode, StatementIndentWrapperNode, StatementNode,
};
use crate::ast::traits::Node;
use crate::constants::common::ENDMARKER;
use crate::lexer::token::Token;
use crate::parser::helper::IndentResultKind;
use crate::parser::parser::JarvilParser;
use crate::parser::resolver::BlockKind;

pub fn block<F, G>(
    parser: &mut JarvilParser,
    is_starting_with_fn: F,
    statement_parsing_fn: G,
    expected_symbols: &[&'static str],
    kind: BlockKind,
) -> BlockNode
where
    F: Fn(&Token) -> bool,
    G: Fn(&mut JarvilParser) -> StatementNode,
{
    let newline_node = parser.expect_terminators();

    let mut stmts_vec: Vec<StatementIndentWrapperNode> = vec![];
    let mut has_atleast_one_stmt = false;

    parser.set_indent_level(parser.curr_indent_level() + 1);

    loop {
        let mut leading_skipped_tokens: Vec<SkippedTokenNode> = vec![];

        let indent_result = parser.expect_indent_spaces();
        let skipped_tokens = indent_result.skipped_tokens;

        if !skipped_tokens.is_empty() {
            stmts_vec.push(
                StatementIndentWrapperNode::new_with_trailing_skipped_tokens(
                    SkippedTokensNode::new_with_trailing_skipped_tokens(skipped_tokens),
                ),
            );
        }

        let extra_newlines = indent_result.extra_newlines;

        if !extra_newlines.is_empty() {
            stmts_vec.push(StatementIndentWrapperNode::new_with_extra_newlines(
                SkippedTokensNode::new_with_extra_newlines(extra_newlines),
            ));
        }

        let incorrect_indent_data = match indent_result.kind {
            IndentResultKind::CorrectIndentation => None,
            IndentResultKind::IncorrectIndentation(indent_data) => Some(indent_data),
            IndentResultKind::BlockOver => {
                parser.set_indent_level(parser.curr_indent_level() - 1);

                if !has_atleast_one_stmt {
                    parser.log_no_valid_statement_inside_block_error(newline_node.range());
                }

                return BlockNode::new(stmts_vec, newline_node, kind);
            }
        };

        while !is_starting_with_fn(parser.curr_token()) {
            let token = parser.curr_token();

            leading_skipped_tokens.push(SkippedTokenNode::new(token.clone()));
            parser.log_missing_token_error(expected_symbols, token);

            if token.is_eq("\n") || token.is_eq(ENDMARKER) {
                break;
            }

            parser.scan_next_token();
        }

        if !leading_skipped_tokens.is_empty() {
            stmts_vec.push(StatementIndentWrapperNode::new_with_leading_skipped_tokens(
                SkippedTokensNode::new_with_leading_skipped_tokens(leading_skipped_tokens),
            ));
        }

        let token = parser.curr_token();

        if token.is_eq(ENDMARKER) {
            parser.set_indent_level(parser.curr_indent_level() - 1);

            if !has_atleast_one_stmt {
                parser.log_no_valid_statement_inside_block_error(newline_node.range());
            }

            return BlockNode::new(stmts_vec, newline_node, kind);
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
                    parser.set_correction_indent(indent_data.1 - indent_data.0);

                    let stmt_node = statement_parsing_fn(parser);

                    parser.set_ignore_all_errors(false);
                    parser.set_correction_indent(0);

                    parser.log_incorrectly_indented_block_error(
                        stmt_node.range(),
                        indent_data.0,
                        indent_data.1,
                    );

                    stmt_node
                };

                stmts_vec.push(StatementIndentWrapperNode::new_with_incorrectly_indented(
                    stmt_node,
                    indent_data.0,
                    indent_data.1,
                ));

                has_atleast_one_stmt = true;
            }
            None => {
                let stmt_node = statement_parsing_fn(parser);

                stmts_vec.push(StatementIndentWrapperNode::new_with_correctly_indented(
                    stmt_node,
                ));

                has_atleast_one_stmt = true;
            }
        }
    }
}
