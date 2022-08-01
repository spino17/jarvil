use std::vec;

use crate::{constants::common::token_for_identifier, lexer::token::CoreToken};

fn str_to_vec(word: &str) -> Vec<char> {
    word.chars().collect()
}

fn assert_token_for_identifier(alt_vec: Vec<&str>, reserved_word: &str, reserved_word_token_type: CoreToken) {
    for word in alt_vec {
        let word_vec = str_to_vec(word);
        let word_iter: std::slice::Iter<char> = word_vec.iter();
        let token_type = if word == reserved_word {
            reserved_word_token_type.clone()
        } else {
            CoreToken::IDENTIFIER
        };
        assert_eq!(token_for_identifier(word_iter), token_type, "word passed: {}", word);
    }
}

#[test]
fn test_general() {
    let alt_vec = vec![
        "zebra", "dungeon_master", "x", "y"
    ];
    assert_token_for_identifier(alt_vec, "x", CoreToken::IDENTIFIER)
}

#[test]
fn test_for() {
    let alt_vec = vec![
        "f", "fo", "for", "fog", "fork", "forest",
    ];
    assert_token_for_identifier(alt_vec, "for", CoreToken::FOR)
}

#[test]
fn test_while() {
    let alt_vec = vec![
        "w", "was", "whi", "while", "whilt", "whales"
    ];
    assert_token_for_identifier(alt_vec, "while", CoreToken::WHILE)
}

#[test]
fn test_continue() {
    let alt_vec = vec![
        "c", "country", "continue", "continus", "continuation"
    ];
    assert_token_for_identifier(alt_vec, "continue", CoreToken::CONTINUE)
}

#[test]
fn test_break() {
    let alt_vec = vec![
        "b", "brag", "break", "boat", "brake", "breaking"
    ];
    assert_token_for_identifier(alt_vec, "break", CoreToken::BREAK)
}

#[test]
fn test_if() {
    let alt_vec = vec![
        "i", "if", "is", "ifconfig"
    ];
    assert_token_for_identifier(alt_vec, "if", CoreToken::IF)
}

#[test]
fn test_else() {
    let alt_vec = vec![
        "e", "el", "else", "elas", "elsex", "elite"
    ];
    assert_token_for_identifier(alt_vec, "else", CoreToken::ELSE)
}