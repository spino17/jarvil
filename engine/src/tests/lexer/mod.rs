use crate::{constants::common::token_for_identifier, lexer::token::CoreToken};

#[test]
fn test_for() {
    let f_vec       = vec!['f'];
    let fo_vec      = vec!['f', 'o'];
    let for_vec     = vec!['f', 'o', 'r'];
    let fork_vec    = vec!['f', 'o', 'r', 'k'];
    let forest_vec  = vec!['f', 'o', 'r', 'e', 's', 't'];
    let alt_vec = vec![
        f_vec, fo_vec, for_vec, fork_vec, forest_vec,
    ];
    for (index, word) in alt_vec.iter().enumerate() {
        let word_iter: std::slice::Iter<char> = word.iter();
        let token_type = if index == 2 {
            CoreToken::FOR
        } else {
            CoreToken::IDENTIFIER
        };
        assert_eq!(token_for_identifier(word_iter), token_type, "word passed: {}", word.iter().collect::<String>());
    }
}

#[test]
fn test_while() {
    let w_vec           = vec!['w'];
    let wh_vec          = vec!['w', 'h'];
    let whi_vec         = vec!['w', 'h', 'i'];
    let whil_vec        = vec!['w', 'h', 'i', 'l'];
    let while_vec       = vec!['w', 'h', 'i', 'l', 'e'];
    let whiles_vec      = vec!['w', 'h', 'i', 'l', 'e', 's'];
    let whilesies_vec   = vec!['w', 'h', 'i', 'l', 'e', 's', 'i', 'e', 's'];
    let alt_vec = vec![
        w_vec, wh_vec, whi_vec, whil_vec, while_vec, whiles_vec, whilesies_vec
    ];
    for (index, word) in alt_vec.iter().enumerate() {
        let word_iter: std::slice::Iter<char> = word.iter();
        let token_type = if index == 4 {
            CoreToken::WHILE
        } else {
            CoreToken::IDENTIFIER
        };
        assert_eq!(token_for_identifier(word_iter), token_type, "word passed: {}", word.iter().collect::<String>());
    }
}