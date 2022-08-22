// The genecis of jarvil formatter traces back to my realization of how well golang has imposed a standard coding style
// not through some kind of boring long style doc which no one reads but rather through a formatting tool `gofmt`.
// jarvil formatter use rules mostly similar to `black` python code-formatting tool.
// The algorithm used for picking up optimal line breaks is heavily inspired by the writings of Bob Nystrom in the post:
// http://journal.stuffwithstuff.com/2015/09/08/the-hardest-program-ive-ever-written/

use crate::ast::ast::{BlockNode, StatementNode};
use crate::error::core::JarvilError;
use crate::utils::common::build_ast;
use crate::{code::Code, context};
use std::{mem, error};

pub struct Formatter {
    formatted_code_str: String,
    indent_level: i64,
    line_number: usize,
    code: Code,
}

impl Formatter {
    pub fn new(code: &Code) -> Self {
        Formatter {
            formatted_code_str: String::default(),
            indent_level: 0,
            line_number: 1,
            code: code.clone(),
        }
    }

    pub fn format(code_vec: Vec<char>) -> Result<String, JarvilError> {
        let mut code = Code::new(code_vec);
        let (ast, errors) = build_ast(&mut code);
        if errors.len() > 0 {
            return Err(errors[0].clone())
        }
        let mut formatter = Formatter::new(&code);
        // TODO - use `ast` to get the formatted version of code
        // 1. walk the ast and get chunks, rules and spans tree
        // 2. use A* algorithm with number of overflowing chars and minimum splits as heuristics function
        // 3. For each value of rule, have a print method which will add appropiate line-breaks, indentation and comments
        Ok(mem::take(&mut formatter.formatted_code_str))
    }
}
