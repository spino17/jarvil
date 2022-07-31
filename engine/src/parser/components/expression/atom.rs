use crate::ast::ast::{AtomNode, AtomStartNode, CallExpressionNode};
use crate::lexer::token::CoreToken;
use crate::{constants::common::IDENTIFIER, parser::parser::PackratParser};

pub fn trailing_atom(parser: &mut PackratParser, atom_start: AtomNode) -> AtomNode {
    // parser.ignore_newlines();
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::DOT => {
            let dot_node = parser.expect(".");
            let property_or_method_name = parser.expect(IDENTIFIER);
            match &parser.curr_token().core_token {
                CoreToken::LPAREN => {
                    let (params_node, lparen_node, rparen_node) =
                        parser.params_within_parenthesis();
                    let atom_node = AtomNode::new_with_method_access(
                        &atom_start,
                        &property_or_method_name,
                        &params_node,
                        &lparen_node,
                        &rparen_node,
                        &dot_node,
                    );
                    return parser.trailing_atom(atom_node);
                }
                _ => {
                    let atom_node = AtomNode::new_with_propertry_access(
                        &atom_start,
                        &property_or_method_name,
                        &dot_node,
                    );
                    return parser.trailing_atom(atom_node);
                }
            }
        }
        CoreToken::LPAREN => {
            // TODO - add a function call node
            let (params_node, lparen_node, rparen_node) = parser.params_within_parenthesis();
            let atom_node =
                AtomNode::new_with_call(&atom_start, &params_node, &lparen_node, &rparen_node);
            return parser.trailing_atom(atom_node);
        }
        CoreToken::LSQUARE => {
            let lsquare_node = parser.expect("[");
            let index_expr = parser.expr();
            let rsquare_node = parser.expect("]");
            let atom_node = AtomNode::new_with_index_access(
                &atom_start,
                &index_expr,
                &lsquare_node,
                &rsquare_node,
            );
            return parser.trailing_atom(atom_node);
        }
        _ => {
            // TODO - add FOLLOW(atom) to handle this more robustly
            return atom_start;
        }
    }
}

pub fn atom(parser: &mut PackratParser) -> AtomNode {
    let leading_identifier_node = parser.expect(IDENTIFIER);
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::LPAREN => {
            let (params_node, lparen_node, rparen_node) = parser.params_within_parenthesis();
            let call_expr = CallExpressionNode::new(
                &leading_identifier_node,
                &params_node,
                &lparen_node,
                &rparen_node,
            );
            let atom_start_node = AtomStartNode::new_with_function_call(&call_expr);
            let atom_node = AtomNode::new_with_atom_start(&atom_start_node);
            return parser.trailing_atom(atom_node);
        }
        CoreToken::DOUBLE_COLON => {
            let double_colon_node = parser.expect("::");
            let class_method_name = parser.expect(IDENTIFIER);
            let (params_node, lparen_node, rparen_node) = parser.params_within_parenthesis();
            let atom_start_node = AtomStartNode::new_with_class_method_call(
                &leading_identifier_node,
                &class_method_name,
                &params_node,
                &double_colon_node,
                &lparen_node,
                &rparen_node,
            );
            let atom_node = AtomNode::new_with_atom_start(&atom_start_node);
            return parser.trailing_atom(atom_node);
        }
        _ => {
            let atom_start_node = AtomStartNode::new_with_identifier(&leading_identifier_node);
            let atom_node = AtomNode::new_with_atom_start(&atom_start_node);
            return parser.trailing_atom(atom_node);
        }
    }
}
