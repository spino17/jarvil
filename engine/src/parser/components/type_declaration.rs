use crate::{parser::parser::PackratParser, constants::common::IDENTIFIER};
use crate::lexer::token::CoreToken;

pub fn type_decl(parser: &mut PackratParser) {
    let type_keyword_node = parser.expect("type", false);
    let type_name_node = parser.expect(IDENTIFIER, false);
    let colon_node = parser.expect(":", false);
    let token = &parser.curr_token();
    match token.core_token {
        CoreToken::NEWLINE  => {
            // struct type
            let block_node = parser.block(|token| {
                match token.core_token {
                    CoreToken::IDENTIFIER(_) => true,
                    _ => false,
                }
            }, 
            |parser| {
                parser.struct_stmt()
            }, &[IDENTIFIER]);
        },
        CoreToken::LPAREN   => {
            // lambda type
        },
        _                   => {
            todo!()
        }
    }
}