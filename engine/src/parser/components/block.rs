use crate::ast::ast::{StatementNode, ParamNode, BlockNode};
use crate::context;
use crate::parser::parser::{PackratParser, ParseSuccess};
use crate::errors::SyntaxError;

pub fn check_block_indentation(parser: &mut PackratParser, 
    indent_spaces: i64, err: SyntaxError, curr_lookahead: usize, 
    params: Vec<ParamNode>, stmts: Vec<StatementNode>) -> Result<(ParseSuccess, BlockNode), SyntaxError> {
    let indent_spaces_unit = context::get_indent();
    let indent_factor = indent_spaces / indent_spaces_unit as i64;
    let indent_remainder = indent_spaces - indent_factor * indent_spaces_unit;
    if indent_remainder > 0 {
        return Err(err)
    } else {
        if indent_spaces > indent_spaces_unit * parser.get_indent_level() {
            return Err(err)
        } else {
            // block is over
            parser.reset_indent_level(parser.get_indent_level() - 1);
            parser.reset_lookahead(curr_lookahead);
            let node = BlockNode::new(stmts, params);
            return Ok((ParseSuccess{
                lookahead: parser.get_lookahead(),
                possible_err: None,
            }, node))
        }
    }
}

pub fn block(parser: &mut PackratParser, params: Vec<ParamNode>) -> Result<(ParseSuccess, BlockNode), SyntaxError> {
    parser.expect("\n")?;
    let mut curr_lookahead = parser.get_lookahead();
    parser.reset_indent_level(parser.get_indent_level() + 1);
    let mut stmts_vec: Vec<StatementNode> = vec![];
    loop {
        let (response, indent_spaces) = parser.expect_indent_spaces()?;
        if let Some(err) = response.possible_err {
            return parser.check_block_indentation(indent_spaces, err, curr_lookahead, params, stmts_vec)
        }
        match parser.stmt() {
            Ok((_, stmt_node)) => {
                stmts_vec.push(stmt_node.clone())
            },
            Err(err) => {
                if parser.check_next_token("endmarker") {
                    let node = BlockNode::new(stmts_vec, params);
                    return Ok((ParseSuccess{
                        lookahead: parser.get_lookahead(),
                        possible_err: Some(err),
                    }, node))
                } else {
                    return Err(err)
                }
            }
        }
        curr_lookahead = parser.get_lookahead();
    }
}