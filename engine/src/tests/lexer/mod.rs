use crate::{lexer::helper::token_for_identifier, lexer::token::CoreToken};
use std::vec;

fn str_to_vec(word: &str) -> Vec<char> {
    word.chars().collect()
}

fn assert_token_for_identifier(
    alt_vec: Vec<&str>,
    reserved_word: &str,
    reserved_word_token_type: CoreToken,
) {
    for word in alt_vec {
        let word_vec = str_to_vec(word);
        let word_iter: std::slice::Iter<char> = word_vec.iter();
        let token_type = if word == reserved_word {
            reserved_word_token_type.clone()
        } else {
            CoreToken::IDENTIFIER
        };
        assert_eq!(
            token_for_identifier(word_iter),
            token_type,
            "word passed: {}",
            word
        );
    }
}

#[test]
fn test_general() {
    let alt_vec = vec!["zebra", "dungeon_master", "x", "y"];
    assert_token_for_identifier(alt_vec, "x", CoreToken::IDENTIFIER)
}

#[test]
fn test_for() {
    let alt_vec = vec!["f", "fo", "for", "fog", "fork", "forest"];
    assert_token_for_identifier(alt_vec, "for", CoreToken::FOR)
}

#[test]
fn test_while() {
    let alt_vec = vec!["w", "was", "whi", "while", "whilt", "whales"];
    assert_token_for_identifier(alt_vec, "while", CoreToken::WHILE)
}

#[test]
fn test_continue() {
    let alt_vec = vec!["c", "country", "continue", "continus", "continuation"];
    assert_token_for_identifier(alt_vec, "continue", CoreToken::CONTINUE)
}

#[test]
fn test_break() {
    let alt_vec = vec!["b", "brag", "break", "boat", "brake", "breaking"];
    assert_token_for_identifier(alt_vec, "break", CoreToken::BREAK)
}

#[test]
fn test_if() {
    let alt_vec = vec!["i", "if", "is", "ifconfig"];
    assert_token_for_identifier(alt_vec, "if", CoreToken::IF)
}

#[test]
fn test_else() {
    let alt_vec = vec!["e", "el", "else", "elas", "elsex", "elite"];
    assert_token_for_identifier(alt_vec, "else", CoreToken::ELSE)
}

#[test]
fn test_elif() {
    let alt_vec = vec!["e", "el", "elif", "elas", "elifx", "elite"];
    assert_token_for_identifier(alt_vec, "elif", CoreToken::ELIF)
}

#[test]
fn test_type() {
    let alt_vec = vec!["t", "typo", "type", "tod", "types", "typical"];
    assert_token_for_identifier(alt_vec, "type", CoreToken::TYPE_KEYWORD)
}

#[test]
fn test_interface() {
    let alt_vec = vec!["i", "inter", "interface", "interfold", "intercity", "iota"];
    assert_token_for_identifier(alt_vec, "interface", CoreToken::INTERFACE_KEYWORD)
}

#[test]
fn test_def() {
    let alt_vec = vec!["d", "do", "def", "dog", "define", "deaf", "delta"];
    assert_token_for_identifier(alt_vec, "def", CoreToken::DEF)
}

#[test]
fn test_let() {
    let alt_vec = vec!["l", "leg", "let", "less", "lego", "letin"];
    assert_token_for_identifier(alt_vec, "let", CoreToken::LET)
}

#[test]
fn test_lambda() {
    let alt_vec = vec!["l", "lam", "lamba", "lambda", "lambdas"];
    assert_token_for_identifier(alt_vec, "lambda", CoreToken::LAMBDA_KEYWORD)
}

#[test]
fn test_self() {
    let alt_vec = vec!["s", "sage", "self", "selfish", "sell"];
    assert_token_for_identifier(alt_vec, "self", CoreToken::SELF)
}

#[test]
fn test_impl() {
    let alt_vec = vec!["i", "im", "impl", "iota", "imply", "implementation"];
    assert_token_for_identifier(alt_vec, "impl", CoreToken::IMPL)
}

#[test]
fn test_and() {
    let alt_vec = vec!["a", "ant", "and", "alpha", "androgyne"];
    assert_token_for_identifier(alt_vec, "and", CoreToken::AND)
}

#[test]
fn test_not() {
    let alt_vec = vec!["n", "no", "not", "nose", "now"];
    assert_token_for_identifier(alt_vec, "not", CoreToken::NOT)
}

#[test]
fn test_or() {
    let alt_vec = vec!["o", "or", "on", "orange"];
    assert_token_for_identifier(alt_vec, "or", CoreToken::OR)
}

#[test]
fn test_in() {
    let alt_vec = vec!["i", "in", "is", "inter"];
    assert_token_for_identifier(alt_vec, "in", CoreToken::IN)
}

#[test]
fn test_true() {
    let alt_vec = vec!["T", "Trap", "True", "Trash", "Try"];
    assert_token_for_identifier(alt_vec, "True", CoreToken::TRUE)
}

#[test]
fn test_false() {
    let alt_vec = vec!["F", "Face", "False", "Falsify", "Factor", "Falsy"];
    assert_token_for_identifier(alt_vec, "False", CoreToken::FALSE)
}

#[test]
fn test_return() {
    let alt_vec = vec!["r", "read", "retard", "return", "retrack", "returning"];
    assert_token_for_identifier(alt_vec, "return", CoreToken::RETURN)
}

#[test]
fn test_int() {
    let alt_vec = vec!["i", "is", "ice", "int", "inter", "international"];
    assert_token_for_identifier(alt_vec, "int", CoreToken::ATOMIC_TYPE)
}

#[test]
fn test_float() {
    let alt_vec = vec!["f", "flat", "float", "floating", "flask"];
    assert_token_for_identifier(alt_vec, "float", CoreToken::ATOMIC_TYPE)
}

#[test]
fn test_string() {
    let alt_vec = vec!["s", "stage", "strike", "str", "string", "stringy", "stringfy"];
    assert_token_for_identifier(alt_vec, "str", CoreToken::ATOMIC_TYPE)
}

#[test]
fn test_struct() {
    let alt_vec = vec!["s", "strings", "stru", "struct", "structure", "struce"];
    assert_token_for_identifier(alt_vec, "struct", CoreToken::STRUCT_KEYWORD)
}

#[test]
fn test_bool() {
    let alt_vec = vec!["b", "bob", "bolt", "bool", "booling", "boost"];
    assert_token_for_identifier(alt_vec, "bool", CoreToken::ATOMIC_TYPE)
}
