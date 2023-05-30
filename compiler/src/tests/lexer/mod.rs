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
    let alt_vec = vec!["i", "if", "iso", "ifconfig"];
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
    let alt_vec = vec!["i", "im", "implements", "iota", "imply", "implementation"];
    assert_token_for_identifier(alt_vec, "implements", CoreToken::IMPLEMENTS_KEYWORD)
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
    let alt_vec = vec!["i", "in", "iso", "inter"];
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
    let alt_vec = vec!["i", "iso", "ice", "int", "inter", "international"];
    assert_token_for_identifier(alt_vec, "int", CoreToken::ATOMIC_TYPE)
}

#[test]
fn test_float() {
    let alt_vec = vec!["f", "flat", "float", "floating", "flask"];
    assert_token_for_identifier(alt_vec, "float", CoreToken::ATOMIC_TYPE)
}

#[test]
fn test_string() {
    let alt_vec = vec![
        "s", "stage", "strike", "str", "string", "stringy", "stringfy",
    ];
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

#[test]
fn test_None() {
    let alt_vec = vec!["N", "Non", "Nones", "None", "Nont"];
    assert_token_for_identifier(alt_vec, "None", CoreToken::NONE)
}

#[test]
fn test_as() {
    let alt_vec = vec!["a", "ast", "as", "ai", "astro"];
    assert_token_for_identifier(alt_vec, "as", CoreToken::AS)
}

#[test]
fn test_assert() {
    let alt_vec = vec!["a", "ass", "assert", "asserting", "astro"];
    assert_token_for_identifier(alt_vec, "assert", CoreToken::ASSERT_KEYWORD)
}

#[test]
fn test_class() {
    let alt_vec = vec!["c", "clone", "clas", "class", "classic"];
    assert_token_for_identifier(alt_vec, "class", CoreToken::CLASS_KEYWORD)
}

#[test]
fn test_del() {
    let alt_vec = vec!["d", "del", "dell", "det"];
    assert_token_for_identifier(alt_vec, "del", CoreToken::DEL_KEYWORD)
}

#[test]
fn test_except() {
    let alt_vec = vec!["e", "expert", "except", "excepted"];
    assert_token_for_identifier(alt_vec, "except", CoreToken::EXCEPT_KEYWORD)
}

#[test]
fn test_finally() {
    let alt_vec = vec!["f", "final", "finale", "finally", "finallya"];
    assert_token_for_identifier(alt_vec, "finally", CoreToken::FINALLY_KEYWORD)
}

#[test]
fn test_from() {
    let alt_vec = vec!["f", "fry", "frost", "from", "froms"];
    assert_token_for_identifier(alt_vec, "from", CoreToken::FROM_KEYWORD)
}

#[test]
fn test_global() {
    let alt_vec = vec!["g", "globe", "global", "globally"];
    assert_token_for_identifier(alt_vec, "global", CoreToken::GLOBAL_KEYWORD)
}

#[test]
fn test_import() {
    let alt_vec = vec!["i", "imported", "import", "important", "impor"];
    assert_token_for_identifier(alt_vec, "import", CoreToken::IMPORT_KEYWORD)
}

#[test]
fn test_is() {
    let alt_vec = vec!["i", "is", "iso", "it"];
    assert_token_for_identifier(alt_vec, "is", CoreToken::IS)
}

#[test]
fn test_nonlocal() {
    let alt_vec = vec!["n", "non", "nonlocalization", "nonlocal", "notice"];
    assert_token_for_identifier(alt_vec, "nonlocal", CoreToken::NONLOCAL_KEYWORD)
}

#[test]
fn test_pass() {
    let alt_vec = vec!["p", "past", "passed", "pass"];
    assert_token_for_identifier(alt_vec, "pass", CoreToken::PASS_KEYWORD)
}

#[test]
fn test_raise() {
    let alt_vec = vec!["r", "rat", "raisen", "raise", "raised"];
    assert_token_for_identifier(alt_vec, "raise", CoreToken::RAISE_KEYWORD)
}

#[test]
fn test_try() {
    let alt_vec = vec!["t", "trying", "try", "tri"];
    assert_token_for_identifier(alt_vec, "try", CoreToken::TRY_KEYWORD)
}

#[test]
fn test_with() {
    let alt_vec = vec!["w", "witty", "whiting", "with", "witha"];
    assert_token_for_identifier(alt_vec, "with", CoreToken::WITH_KEYWORD)
}

#[test]
fn test_yield() {
    let alt_vec = vec!["y", "yielding", "yield", "yikes"];
    assert_token_for_identifier(alt_vec, "yield", CoreToken::YIELD_KEYWORD)
}

#[test]
fn test_async() {
    let alt_vec = vec!["a", "async", "asyncronization", "ast"];
    assert_token_for_identifier(alt_vec, "async", CoreToken::ASYNC_KEYWORD)
}

#[test]
fn test_await() {
    let alt_vec = vec!["a", "awaiting", "await", "awful"];
    assert_token_for_identifier(alt_vec, "await", CoreToken::AWAIT_KEYWORD)
}

#[test]
fn test_peg_parser() {
    let alt_vec = vec!["_", "__peg_", "__peg_parser__", "__peg_parser___"];
    assert_token_for_identifier(alt_vec, "__peg_parser__", CoreToken::PEG_PARSER)
}
