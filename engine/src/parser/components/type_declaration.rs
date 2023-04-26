use crate::ast::ast::{BlockKind, CallablePrototypeNode, ErrornousNode, NameTypeSpecsNode};
use crate::ast::ast::{LambdaTypeDeclarationNode, TypeDeclarationNode};
use crate::lexer::token::CoreToken;
use crate::{constants::common::IDENTIFIER, parser::parser::PackratParser};
use std::rc::Rc;

pub fn type_decl(parser: &mut PackratParser) -> TypeDeclarationNode {
    let type_keyword_node = parser.expect("type");
    let type_name_node = parser.expect_ident();
    let colon_node = parser.expect(":");
    let token = &parser.curr_token();
    let type_decl_node = match token.core_token {
        CoreToken::NEWLINE => {
            // struct type
            let block_node = parser.block(
                |token| match token.core_token {
                    CoreToken::IDENTIFIER => true,
                    _ => false,
                },
                |parser| parser.struct_stmt(),
                &[IDENTIFIER],
                BlockKind::STRUCT,
            );
            TypeDeclarationNode::new_with_struct(
                &type_name_node,
                &block_node,
                &type_keyword_node,
                &colon_node,
            )
        }
        CoreToken::LPAREN => {
            // lambda type
            let lparen_node = parser.expect("(");
            let mut args_node: Option<&NameTypeSpecsNode> = None;
            let name_type_specs_node: NameTypeSpecsNode;
            if !parser.check_curr_token(")") {
                name_type_specs_node = parser.name_type_specs();
                args_node = Some(&name_type_specs_node);
            }
            let rparen_node = parser.expect(")");
            let token = &parser.curr_token();
            let lambda_node = match token.core_token {
                CoreToken::RIGHT_ARROW => {
                    let r_arrow_node = parser.expect("->");
                    let return_type_node = parser.type_expr();
                    let newline_node = parser.expect_terminators();
                    let callable_prototype = CallablePrototypeNode::new(
                        args_node,
                        Some(&return_type_node),
                        &lparen_node,
                        &rparen_node,
                        Some(&r_arrow_node),
                    );
                    LambdaTypeDeclarationNode::new(
                        &type_name_node,
                        &type_keyword_node,
                        &colon_node,
                        &callable_prototype,
                        &newline_node,
                    )
                }
                CoreToken::NEWLINE | CoreToken::ENDMARKER => {
                    let newline_node = parser.expect_terminators();
                    let callable_prototype = CallablePrototypeNode::new(
                        args_node,
                        None,
                        &lparen_node,
                        &rparen_node,
                        None,
                    );
                    LambdaTypeDeclarationNode::new(
                        &type_name_node,
                        &type_keyword_node,
                        &colon_node,
                        &callable_prototype,
                        &newline_node,
                    )
                }
                _ => {
                    parser.log_missing_token_error(&["->", "\n"], token);
                    let lambda_node = LambdaTypeDeclarationNode::new_with_missing_tokens(
                        &Rc::new(["->", "\n"].to_vec()),
                        token,
                    );
                    return TypeDeclarationNode::new_with_lambda(&lambda_node);
                }
            };
            TypeDeclarationNode::new_with_lambda(&lambda_node)
        }
        _ => {
            parser.log_missing_token_error(&["\n", "("], token);
            return TypeDeclarationNode::new_with_missing_tokens(
                &Rc::new(["\n", "("].to_vec()),
                token,
            );
        }
    };
    type_decl_node
}
