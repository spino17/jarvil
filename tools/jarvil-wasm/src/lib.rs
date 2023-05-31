use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn compile(name: &str) -> Result<String, String> {
    if name == "Yamini" {
        return Ok("Hello, Yamini dear".to_string())
    } else {
        return Err("Error occured hahaha".to_string())
    }
}