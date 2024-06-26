use crate::ast::ast::LambdaDeclarationNode;
use crate::constants::common::LAMBDA_KEYWORD;
use crate::parser::resolver::BlockKind;
use crate::{ast::ast::RVariableDeclarationNode, lexer::token::CoreToken};
use crate::{ast::ast::VariableDeclarationNode, parser::parser::JarvilParser};

pub fn variable_decl(parser: &mut JarvilParser) -> VariableDeclarationNode {
    let mut optional_ty_annotation_node = None;
    let let_keyword_node = parser.expect("let");
    let identifier_node = parser.expect_identifier();
    let curr_token = parser.curr_token();

    if curr_token.is_eq(":") {
        let colon_node = parser.expect(":");
        let variable_ty_expr_node = parser.ty_expr();

        optional_ty_annotation_node = Some((colon_node, variable_ty_expr_node));
    }

    let equal_node = parser.expect("=");
    let token = parser.curr_token();
    let r_node = match token.core_token() {
        CoreToken::LAMBDA_KEYWORD => {
            let lambda_keyword_node = parser.expect(LAMBDA_KEYWORD);
            let callable_body = parser.callable_body(BlockKind::Lambda);
            let lambda_decl_node = LambdaDeclarationNode::new(lambda_keyword_node, callable_body);

            RVariableDeclarationNode::new_with_lambda(lambda_decl_node)
        }
        _ => {
            // TODO - change this when `expr` like conditionals and loops will be introduced
            let expr_node = parser.expr();
            let newline_node = parser.expect_terminators();

            RVariableDeclarationNode::new_with_expr(expr_node, newline_node)
        }
    };

    VariableDeclarationNode::new(
        identifier_node,
        r_node,
        let_keyword_node,
        equal_node,
        optional_ty_annotation_node,
    )
}
