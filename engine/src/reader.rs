use std::io;
use std::{fs};
use std::rc::Rc;

pub fn read_file(file_name: &str) -> io::Result<Rc<Vec<char>>> {
    let contents = fs::read_to_string(file_name)?;
    let char_vec: Vec<char> = contents.chars().collect();
    Ok(Rc::new(char_vec))
}