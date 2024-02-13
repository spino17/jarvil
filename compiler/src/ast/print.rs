use super::ast::BlockNode;
use crate::code::JarvilCodeHandler;
use crate::core::string_interner::Interner;
use serde_json::Map;
use serde_json::Result;
use serde_json::Value;
use text_size::TextRange;

pub fn modify_array(
    array: &Vec<Value>,
    code: &JarvilCodeHandler,
    interner: &Interner,
) -> Vec<Value> {
    let mut modified_vec: Vec<Value> = vec![];
    for entry in array {
        modified_vec.push(modify_value(entry, code, interner));
    }
    modified_vec
}

pub fn modify_map(
    map: &Map<String, Value>,
    code: &JarvilCodeHandler,
    interner: &Interner,
) -> Map<String, Value> {
    let mut modified_map = Map::new();
    for (key, entry) in map {
        if key != "token" {
            modified_map.insert(key.clone(), modify_value(entry, code, interner));
            continue;
        }
        let Value::Object(token_value) = entry else {
            unreachable!()
        };
        let mut modified_token_value_map = modify_map(token_value, code, interner);
        let Some(range) = token_value.get("range") else {
            unreachable!()
        };
        let range: Vec<u32> = serde_json::from_value(range.clone()).unwrap();
        debug_assert!(range.len() == 2);
        let start_index = range[0];
        let end_index = range[1];
        let span = TextRange::new(start_index.into(), end_index.into());
        let token_value = code.code.token_from_range(span);
        modified_token_value_map.insert("value".to_string(), Value::String(token_value));
        modified_map.insert("token".to_string(), Value::Object(modified_token_value_map));
    }
    modified_map
}

pub fn modify_value(val: &Value, code: &JarvilCodeHandler, interner: &Interner) -> Value {
    match val {
        Value::Array(array) => Value::Array(modify_array(array, code, interner)),
        Value::Object(map) => Value::Object(modify_map(map, code, interner)),
        Value::Null | Value::Bool(_) | Value::Number(_) | Value::String(_) => val.clone(),
    }
}

pub fn json_serialize_ast(
    ast: &BlockNode,
    code: &JarvilCodeHandler,
    interner: &Interner,
) -> Result<String> {
    let serialized_ast = serde_json::to_string(ast)?;
    let deserialized: Value = serde_json::from_str(&serialized_ast)?;
    let modified_serialized_ast: Value = modify_value(&deserialized, code, interner);
    serde_json::to_string(&modified_serialized_ast)
}
