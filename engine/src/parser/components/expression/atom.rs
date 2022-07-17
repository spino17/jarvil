use crate::ast::ast::{AtomStartNode, AtomNode, ParamsNode, CallExpressionNode};
use crate::{parser::parser::PackratParser, constants::common::IDENTIFIER};
use crate::lexer::token::CoreToken;

pub fn atom(parser: &mut PackratParser, atom_start: AtomNode) -> AtomNode {
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::DOT => {
            let dot_node = parser.expect(".", false);
            let property_or_method_name = parser.expect(IDENTIFIER, false);
            match &parser.curr_token().core_token {
                CoreToken::LPAREN => {
                    let params_node = parser.params_within_parenthesis();
                    let atom_node = AtomNode::new_with_method_access(
                        &atom_start, &property_or_method_name, &params_node
                    );
                    return parser.atom(atom_node)
                },
                _ => {
                    let atom_node = AtomNode::new_with_propertry_access(
                        &atom_start, &property_or_method_name
                    );
                    return parser.atom(atom_node)
                }
            }
        },
        CoreToken::LSQUARE => {
            let lsquare_node = parser.expect("[", false);
            let index_expr = parser.expr();
            let rsquare_node = parser.expect("]", false);
            let atom_node = AtomNode::new_with_index_access(&atom_start, &index_expr);
            return parser.atom(atom_node)
        },
        _ => {
            return atom_start
        }
    }
}

pub fn atom_start(parser: &mut PackratParser) -> AtomNode {
    let leading_identifier_node = parser.expect(IDENTIFIER, false);
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::DOT              => {
            // return parser.atom(leading_identifier_node);
            let atom_start_node = AtomStartNode::new_with_identifier(&leading_identifier_node);
            let atom_node = AtomNode::new_with_atom_start(&atom_start_node);
            return parser.atom(atom_node)
        },
        CoreToken::LPAREN           => {
            let params_node = parser.params_within_parenthesis();
            let call_expr = CallExpressionNode::new(&leading_identifier_node, &params_node);
            let atom_start_node = AtomStartNode::new_with_function_call(&call_expr);
            let atom_node = AtomNode::new_with_atom_start(&atom_start_node);
            return parser.atom(atom_node)
        },
        CoreToken::DOUBLE_COLON     => {
            let double_colon_node = parser.expect("::", false);
            let class_method_name = parser.expect(IDENTIFIER, false);
            let params_node = parser.params_within_parenthesis();
            let atom_start_node = AtomStartNode::new_with_class_method_call(
                &leading_identifier_node, &class_method_name, &params_node
            );
            let atom_node = AtomNode::new_with_atom_start(&atom_start_node);
            return parser.atom(atom_node)
        },
        CoreToken::LSQUARE          => {
            let atom_start_node = AtomStartNode::new_with_identifier(&leading_identifier_node);
            let atom_node = AtomNode::new_with_atom_start(&atom_start_node);
            return parser.atom(atom_node)
        },
        _                           => {
            let atom_start_node = AtomStartNode::new_with_identifier(&leading_identifier_node);
            AtomNode::new_with_atom_start(&atom_start_node)
        }
    }
}