use super::ast::BlockNode;
use crate::code::JarvilCodeHandler;
use serde_json::Result;
use serde_json::Value;
use text_size::TextRange;

fn process_value(val: &mut Value, code: &JarvilCodeHandler) {
    match val {
        Value::Array(array) => {
            for element in array {
                process_value(element, code);
            }
        }
        Value::Object(map) => {
            if map.contains_key("core_token") {
                let Some(range) = map.get("range") else {
                    unreachable!()
                };
                let range: Vec<u32> = serde_json::from_value(range.clone()).unwrap();
                debug_assert!(range.len() == 2);
                let start_index = range[0];
                let end_index = range[1];
                let span = TextRange::new(start_index.into(), end_index.into());
                let token_value = code.code.token_from_range(span);
                map.insert("value".to_string(), Value::String(token_value));
            }
            for (_, value) in map {
                process_value(value, code);
            }
        }
        _ => (),
    }
}

// Serializes the AST into a `serde_json::Value` with each token's source text
// attached under a `value` key. Both the on-disk `__ast_<name>.json` dump and
// the compact tree used by snapshot tests are rendered from this.
pub fn ast_to_value(ast: &BlockNode, code: &JarvilCodeHandler) -> Result<Value> {
    let serialized_ast = serde_json::to_string(ast)?;
    let mut deserialized: Value = serde_json::from_str(&serialized_ast)?;

    process_value(&mut deserialized, code);

    Ok(deserialized)
}

pub fn serialize_ast(ast: &BlockNode, code: &JarvilCodeHandler) -> Result<String> {
    serde_json::to_string(&ast_to_value(ast, code)?)
}
