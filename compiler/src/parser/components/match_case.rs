use crate::{
    ast::ast::MatchCaseStatementNode,
    constants::common::CASE,
    lexer::token::CoreToken,
    parser::{parser::JarvilParser, resolver::BlockKind},
};

pub fn match_case(parser: &mut JarvilParser) -> MatchCaseStatementNode {
    let match_keyword_node = parser.expect("match");
    let expr_node = parser.expr();
    let colon_node = parser.expect(":");
    let block_node = parser.block(
        |token| match token.core_token() {
            CoreToken::CASE => true,
            _ => false,
        },
        |parser| parser.case_branch_stmt(),
        &[CASE],
        BlockKind::Match,
    );

    MatchCaseStatementNode::new(match_keyword_node, expr_node, colon_node, block_node)
}
