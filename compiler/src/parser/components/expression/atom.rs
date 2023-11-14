use crate::ast::ast::{
    AtomNode, AtomStartNode, CallExpressionNode, ExpressionNode, SymbolSeparatedSequenceNode,
    TokenNode,
};
use crate::lexer::token::CoreToken;
use crate::parser::parser::JarvilParser;

pub fn trailing_atom(parser: &mut JarvilParser, atom_start_node: AtomNode) -> AtomNode {
    let token = parser.curr_token();
    match token.core_token {
        CoreToken::DOT => {
            let dot_node = parser.expect(".");
            let property_or_method_name_node = parser.expect_identifier_in_use();
            match parser.curr_token().core_token {
                CoreToken::LPAREN => {
                    let lparen_node = parser.expect("(");
                    let mut params_node: Option<SymbolSeparatedSequenceNode<ExpressionNode>> = None;
                    if !parser.check_curr_token(")") {
                        params_node = Some(parser.params());
                    }
                    let rparen_node = parser.expect(")");
                    let atom_node = AtomNode::new_with_method_access(
                        atom_start_node,
                        property_or_method_name_node,
                        params_node,
                        lparen_node,
                        rparen_node,
                        dot_node,
                    );
                    parser.trailing_atom(atom_node)
                }
                _ => {
                    let atom_node = AtomNode::new_with_propertry_access(
                        atom_start_node,
                        property_or_method_name_node,
                        dot_node,
                    );
                    parser.trailing_atom(atom_node)
                }
            }
        }
        CoreToken::LPAREN => {
            let lparen_node = parser.expect("(");
            let mut params_node: Option<SymbolSeparatedSequenceNode<ExpressionNode>> = None;
            if !parser.check_curr_token(")") {
                params_node = Some(parser.params());
            }
            let rparen_node = parser.expect(")");
            let atom_node =
                AtomNode::new_with_call(atom_start_node, params_node, lparen_node, rparen_node);
            parser.trailing_atom(atom_node)
        }
        CoreToken::LSQUARE => {
            let lsquare_node = parser.expect("[");
            let index_expr_node = parser.expr();
            let rsquare_node = parser.expect("]");
            let atom_node = AtomNode::new_with_index_access(
                atom_start_node,
                index_expr_node,
                lsquare_node,
                rsquare_node,
            );
            parser.trailing_atom(atom_node)
        }
        _ => atom_start_node,
    }
}

pub fn atom_start(parser: &mut JarvilParser) -> AtomStartNode {
    let token = parser.curr_token();
    match token.core_token {
        CoreToken::IDENTIFIER => {
            let leading_identifier_node = parser.expect_identifier_in_use();
            let token = parser.curr_token();
            match token.core_token {
                CoreToken::LPAREN => {
                    let lparen_node = parser.expect("(");
                    let mut params_node: Option<SymbolSeparatedSequenceNode<ExpressionNode>> = None;
                    if !parser.check_curr_token(")") {
                        params_node = Some(parser.params());
                    }
                    let rparen_node = parser.expect(")");
                    let call_expr_node = CallExpressionNode::new(
                        leading_identifier_node,
                        params_node,
                        lparen_node,
                        rparen_node,
                    );
                    AtomStartNode::new_with_function_call(call_expr_node)
                }
                CoreToken::DOUBLE_COLON => {
                    let mut contained_params_node: Option<(
                        TokenNode,
                        Option<SymbolSeparatedSequenceNode<ExpressionNode>>,
                        TokenNode,
                    )> = None;
                    let double_colon_node = parser.expect("::");
                    let ty_name = parser.expect_identifier_in_use();
                    if parser.curr_token().is_eq("(") {
                        let lparen_node = parser.expect("(");
                        let mut params_node: Option<SymbolSeparatedSequenceNode<ExpressionNode>> =
                            None;
                        if !parser.check_curr_token(")") {
                            params_node = Some(parser.params());
                        }
                        let rparen_node = parser.expect(")");
                        contained_params_node = Some((lparen_node, params_node, rparen_node));
                    }
                    AtomStartNode::new_with_enum_variant_expr_or_class_method_call(
                        leading_identifier_node,
                        ty_name,
                        contained_params_node,
                        double_colon_node,
                    )
                }
                _ => AtomStartNode::new_with_identifier(leading_identifier_node),
            }
        }
        CoreToken::SELF => {
            let self_keyword_node = parser.expect_self();
            AtomStartNode::new_with_self_keyword(self_keyword_node)
        }
        _ => unreachable!(),
    }
}

pub fn atom(parser: &mut JarvilParser) -> AtomNode {
    let atom_start_node = parser.atom_start();
    let atom_node = AtomNode::new_with_atom_start(atom_start_node);
    parser.trailing_atom(atom_node)
}
