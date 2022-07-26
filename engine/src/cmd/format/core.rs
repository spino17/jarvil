// The genecis of jarvil formatter traces back to my realization of how well golang has imposed a standard coding style 
// not through some kind of boring long style doc which no one reads but rather through a formatting tool `gofmt`.
// jarvil formatter use rules mostly similar to `black` python code-formatting tool.
// The algorithm used for picking up optimal line breaks is heavily inspired by the writings of Bob Nystrom in the post:
// http://journal.stuffwithstuff.com/2015/09/08/the-hardest-program-ive-ever-written/

use crate::ast::ast::{StatementNode, BlockNode};
use crate::errors::ParseError;
use crate::utils::common::build_ast;
use crate::context;
use std::mem;

pub struct Formatter {
    formatted_code_str: String,
    indent_level: i64,
    line_number: usize,
}

impl Formatter {
    pub fn new() -> Self {
        Formatter {
            formatted_code_str: String::default(),
            indent_level: 0,
            line_number: 1,
        }
    }

    pub fn format_block(&mut self, block_node: BlockNode) {
        todo!()
    }

    pub fn format_statement(&mut self, stmt_node: StatementNode) {
        todo!()
    }

    pub fn format(code_vec: Vec<char>) -> Result<String, ParseError> {
        let ast = build_ast(code_vec);
        match context::first_error() {
            Some(err) => {
                return Err(err)
            },
            None => {}
        }
        let mut formatter = Formatter::new();
        // TODO - use `ast` to get the formatted version of code
        // 1. walk the ast and get chunks, rules and spans tree
        // 2. use A* algorithm with number of overflowing chars and minimum splits as heuristics function
        // 3. For each value of rule, have a print method which will add appropiate line-breaks, indentation and comments
        formatter.format_block(ast);
        Ok(mem::take(&mut formatter.formatted_code_str))
    }
}