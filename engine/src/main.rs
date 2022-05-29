mod errors;

use std::fs;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let contents = fs::read_to_string("/Users/bhavyabhatt/Desktop/file.jv")?;
    Ok(())
}