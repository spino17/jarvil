mod errors;
mod lexer;
mod env;
mod context;
mod constants;
mod reader;

use errors::CompilationError;
use std::rc::Rc;
use crate::env::Env;
use crate::reader::read_file;
use crate::lexer::{token::{Token, TokenValue}, lexer::CoreLexer};

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

    let mut core_lexer = CoreLexer::new();
    
    // call init on symbol_table to set keywords before lexical phase.
    // call scan from lexical analyzer to return iter of tokens.
    // attempt parsing for syntax and semantical analysis.
    let v_ref = TokenValue(Rc::new(String::from("v_ref")));
    let v = TokenValue(Rc::new(String::from("v")));
    let f = TokenValue(Rc::new(String::from("f")));
    let g = TokenValue(Rc::new(String::from("f")));
    let varima = TokenValue(Rc::new(String::from("varima")));
    let bhatt = TokenValue(Rc::new(String::from("bhatt")));
    let wds = TokenValue(Rc::new(String::from("wds")));
    let pandey = TokenValue(Rc::new(String::from("pandey")));

    let mut scope = Env::new();  // used to set global variable declarations
    scope.set(&v_ref, String::from("type"));
    scope.set(&v, String::from("hashmap"));


    let mut scope_1 = Env::new_with_parent_env(&scope);
    scope_1.set(&g, String::from("uint"));
    // scope_1.set(&g, String::from("bool"));
    scope_1.set(&varima, String::from("array"));
    println!("{:?}", scope_1.check_declaration(&f));


    let mut scope_2 = Env::new_with_parent_env(&scope);
    scope_2.set(&bhatt, String::from("vector"));
    scope_2.set(&wds, String::from("type"));
    scope.set(&pandey, String::from("type"));


    println!("{:?}", scope_1);
    println!("{:?}", Token::new_with_name_and_value(10, "id", "v").is_type(&scope_2));
    Ok(())
}