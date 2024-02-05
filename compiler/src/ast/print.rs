use super::ast::BlockNode;
use serde_json::Result;

pub fn print_ast(ast: &BlockNode) -> Result<String> {
    return serde_json::to_string(ast);
}
