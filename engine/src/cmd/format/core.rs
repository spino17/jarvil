// The genecis of jarvil formatter traces back to my realization of how well golang has imposed a standard coding style 
// not through some kind of boring long style doc which no one reads but rather through a formatting tool `gofmt`.
// jarvil formatter use rules mostly similar to `black` python code-formatting tool.
// The algorithm used for picking up optimal line breaks is heavily inspired by the writings of Bob Nystrom in the post:
// http://journal.stuffwithstuff.com/2015/09/08/the-hardest-program-ive-ever-written/

use crate::code::Code;

pub trait Formatter {
    fn format(code: &mut Code) -> String;
}