use std::io;
use std::{fs};
use crate::code::Code;

pub fn read_file(file_name: &str) -> io::Result<Vec<char>> {
    let contents = fs::read_to_string(file_name)?;
    let char_vec: Vec<char> = contents.chars().collect();
    Ok(char_vec)
}