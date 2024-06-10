use super::ast::BlockNode;
use crate::code::JarvilCodeHandler;
use crate::core::string_interner::Interner;
use serde_json::Result;
use serde_json::Value;
use text_size::TextRange;

fn process_value(val: &mut Value, code: &JarvilCodeHandler, interner: &Interner) {
    match val {
        Value::Array(array) => {
            for element in array {
                process_value(element, code, interner);
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
                process_value(value, code, interner);
            }
        }
        _ => (),
    }
}

pub fn serialize_ast(
    ast: &BlockNode,
    code: &JarvilCodeHandler,
    interner: &Interner,
) -> Result<String> {
    let serialized_ast = serde_json::to_string(ast)?;
    let mut deserialized: Value = serde_json::from_str(&serialized_ast)?;

    process_value(&mut deserialized, code, interner);

    serde_json::to_string(&deserialized)
}
