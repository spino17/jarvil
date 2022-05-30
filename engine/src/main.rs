mod errors;
mod lexer;
mod env;
mod context;
mod constants;

use std::fs;
use errors::CompilationError;
use crate::env::Env;

fn main() -> Result<(), CompilationError> {
    let contents = fs::read_to_string("/Users/bhavyabhatt/Desktop/main.jv")?;
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
    println!("{:?}", scope_1.is_type("String"));
    Ok(())
}