// The genecis of jarvil formatter traces back to my realization of how well golang has imposed a standard coding style 
// not through some kind of boring long style doc which no one reads but rather through a formatting tool `gofmt`.
// jarvil formatter use rules mostly similar to `black` python code-formatting tool.
// The algorithm used for picking up optimal line breaks is heavily inspired by the writings of Bob Nystrom in the post:
// http://journal.stuffwithstuff.com/2015/09/08/the-hardest-program-ive-ever-written/

use crate::code::Code;
use crate::errors::ParseError;
use crate::utils::common::build_ast;
use crate::context;

pub struct Formatter {

}

impl Formatter {
    pub fn new() -> Self {
        Formatter {

        }
    }

    pub fn format(code_vec: Vec<char>) -> Result<(), ParseError> {
        let ast = build_ast(code_vec);
        match context::first_error() {
            Some(err) => {
                return Err(err)
            },
            None => {}
        }
        let formatter = Formatter::new();
        // TODO - use `ast` to get the formatter version of code
        Ok(())
    }
}