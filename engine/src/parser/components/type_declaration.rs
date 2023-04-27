use crate::ast::ast::{BlockKind, ErrornousNode, TypeTupleNode};
use crate::ast::ast::{LambdaTypeDeclarationNode, TypeDeclarationNode};
use crate::lexer::token::CoreToken;
use crate::{constants::common::IDENTIFIER, parser::parser::PackratParser};
use std::rc::Rc;

pub fn type_decl(parser: &mut PackratParser) -> TypeDeclarationNode {
    let type_keyword_node = parser.expect("type");
    let type_name_node = parser.expect_ident();
    let token = &parser.curr_token();
    let type_decl_node = match token.core_token {
        CoreToken::STRUCT_KEYWORD => {
            let struct_keyword = parser.expect("struct");
            let colon_node = parser.expect(":");
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
                &struct_keyword,
                &colon_node,
            )
        }
        CoreToken::LAMBDA_KEYWORD => {
            let lambda_keyword = parser.expect("lambda");
            let equal_node = parser.expect("=");
            let callable_prototype = parser.callable_prototype();
            let token = &parser.curr_token();
            let lambda_node = match token.core_token {
                CoreToken::NEWLINE | CoreToken::ENDMARKER => {
                    let newline_node = parser.expect_terminators();
                    LambdaTypeDeclarationNode::new(
                        &type_name_node,
                        &type_keyword_node,
                        &lambda_keyword,
                        &equal_node,
                        &callable_prototype,
                        &newline_node,
                    )
                }
                _ => {
                    parser.log_missing_token_error(&["\n"], token);
                    let lambda_node = LambdaTypeDeclarationNode::new_with_missing_tokens(
                        &Rc::new(["\n"].to_vec()),
                        token,
                    );
                    return TypeDeclarationNode::new_with_lambda(&lambda_node);
                }
            };
            TypeDeclarationNode::new_with_lambda(&lambda_node)
        }
        _ => {
            parser.log_missing_token_error(&["struct", "lambda"], token);
            return TypeDeclarationNode::new_with_missing_tokens(
                &Rc::new(["struct", "lambda"].to_vec()),
                token,
            );
        }
    };
    type_decl_node
}
