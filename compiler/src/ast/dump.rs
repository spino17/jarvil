// A compact, human-readable rendering of the AST, meant for snapshot tests.
//
// The JSON produced by `serialize_ast` is faithful but unreadable as a test
// artifact: a three-line program expands to several kilobytes, most of it
// trivia and byte offsets. A snapshot is only useful if a reviewer can look at
// its diff and say whether the change was intended, so this renderer drops
// everything positional and collapses the deep single-field wrappers the AST
// uses, leaving the shape of the tree and the source text of each token.
//
// Deliberately omitted:
//   - `range` / `line_number`, which shift whenever anything above them is
//     edited and would churn every snapshot in the corpus
//   - `trivia`, i.e. whitespace and comments hanging off each token
//   - `null` fields, which are the (many) unset `Option`s

use super::ast::BlockNode;
use crate::code::JarvilCodeHandler;
use serde_json::{Result, Value};

// keys carrying position or whitespace information rather than tree structure
const SKIPPED_KEYS: [&str; 4] = ["range", "line_number", "trivia", "core_token"];

// Renders a token as `CORE_TOKEN "source text"`, dropping the text for tokens
// whose spelling is implied by the token kind (keywords, operators) or is pure
// whitespace (newlines, indentation).
fn render_token(map: &serde_json::Map<String, Value>) -> String {
    let kind = match map.get("core_token") {
        Some(Value::String(kind)) => kind.as_str(),
        _ => "?",
    };
    let value = match map.get("value") {
        Some(Value::String(value)) => value.as_str(),
        _ => "",
    };

    if value.trim().is_empty() || value == kind {
        return kind.to_string();
    }

    format!("{} {:?}", kind, value)
}

fn indent_of(depth: usize) -> String {
    "  ".repeat(depth)
}

// Walks the serialized tree, collapsing chains of single-field objects into a
// dotted label (`name.Ok.name: IDENTIFIER "main"`) so that the AST's wrapper
// nodes don't each cost a level of indentation.
fn render(val: &Value, label: &str, depth: usize, out: &mut String) {
    match val {
        Value::Null => (),
        Value::Object(map) => {
            if map.contains_key("core_token") {
                out.push_str(&format!(
                    "{}{}: {}\n",
                    indent_of(depth),
                    label,
                    render_token(map)
                ));

                return;
            }

            let fields: Vec<(&String, &Value)> = map
                .iter()
                .filter(|(key, val)| !SKIPPED_KEYS.contains(&key.as_str()) && !val.is_null())
                .collect();

            match fields.as_slice() {
                // an empty node still deserves a line, or the tree would lie
                // about its shape
                [] => out.push_str(&format!("{}{}\n", indent_of(depth), label)),
                // sole field: fold it into this line's label instead of nesting
                [(key, child)] => {
                    let folded = if label.is_empty() {
                        (*key).to_string()
                    } else {
                        format!("{}.{}", label, key)
                    };

                    render(child, &folded, depth, out);
                }
                _ => {
                    if !label.is_empty() {
                        out.push_str(&format!("{}{}\n", indent_of(depth), label));
                    }

                    for (key, child) in fields {
                        render(child, key, depth + 1, out);
                    }
                }
            }
        }
        Value::Array(items) => {
            let items: Vec<&Value> = items.iter().filter(|item| !item.is_null()).collect();

            if items.is_empty() {
                out.push_str(&format!("{}{}: []\n", indent_of(depth), label));

                return;
            }

            out.push_str(&format!("{}{}\n", indent_of(depth), label));

            for item in items {
                render(item, "-", depth + 1, out);
            }
        }
        _ => out.push_str(&format!("{}{}: {}\n", indent_of(depth), label, val)),
    }
}

pub fn dump_ast(ast: &BlockNode, code: &JarvilCodeHandler) -> Result<String> {
    let value = super::print::ast_to_value(ast, code)?;
    let mut out = String::new();

    render(&value, "", 0, &mut out);

    Ok(out)
}
