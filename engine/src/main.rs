mod errors;

use std::fs;
use errors::CompilationError;

fn main() -> Result<(), CompilationError> {
    let contents = fs::read_to_string("/Users/bhavyabhatt/Desktop/main.jv")?;
    print!("{}", contents);
    Ok(())
}