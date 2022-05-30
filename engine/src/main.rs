mod errors;
mod lexer;
mod env;
mod context;
mod constants;
mod reader;

use std::{fs, vec};
use errors::CompilationError;
use crate::env::Env;
use crate::reader::read_file;

fn print_string(name: &str) {
    println!("{}", name);
}

struct Node {
    name: String,
}

fn main() -> Result<(), CompilationError> {
    let char_vec: Vec<char> = read_file("/Users/bhavyabhatt/Desktop/main.jv")?;  // pass this vector of char to lexer
    let mut counter = 0;
    for &i in &char_vec {
        if (i == ' ') {
            println!("line changed");
        }
        counter = counter + 1;
    }
    let vec_slice = &char_vec[13..14];
    let s: &String = &vec_slice.iter().collect();
    if s.eq("/") {
        println!("Horray")
    }
    

    // call init on symbol_table to set keywords before lexical phase.
    // call scan from lexical analyzer to return iter of tokens.
    // attempt parsing for syntax and semantical analysis.
    let mut scope = Env::new();  // used to set global variable declarations
    scope.set(String::from("v_ref"), String::from("int"));
    scope.set(String::from("v"), String::from("hashmap"));
    let mut scope_1 = Env::new_with_parent_env(&scope);
    scope_1.set(String::from("f"), String::from("uint"));
    scope_1.set(String::from("varima"), String::from("array"));
    let mut scope_2 = Env::new_with_parent_env(&scope_1);
    scope_2.set(String::from("bhatt"), String::from("vector"));
    scope_2.set(String::from("wds"), String::from("keyword"));
    scope.set(String::from("pandey"), String::from("string"));
    println!("{:?}", scope_1.is_keyword("while"));
    Ok(())
}