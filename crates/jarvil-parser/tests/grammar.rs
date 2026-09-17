// Keeps the VS Code TextMate grammar honest.
//
// The grammar hard-codes keyword lists as regex alternations, which will
// silently drift from the lexer the first time a keyword is added. Rather than
// generate the grammar (which would mean a build step for a file that changes
// a couple of times a year), assert here that every keyword the lexer knows
// about appears somewhere in the grammar. A new keyword then fails this test
// instead of just quietly not highlighting.

use std::fs;
use std::path::PathBuf;

// Walks up from this crate to the workspace root, so the test survives the
// crate being moved around the tree -- which is exactly what broke it once.
fn workspace_root() -> PathBuf {
    let mut dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    while !dir.join("Cargo.toml").is_file() || !dir.join("extensions").is_dir() {
        assert!(
            dir.pop(),
            "no workspace root above {}",
            env!("CARGO_MANIFEST_DIR")
        );
    }

    dir
}

fn grammar_source() -> String {
    let path = workspace_root()
        .join("extensions")
        .join("jarvil-vscode")
        .join("syntaxes")
        .join("jarvil.tmLanguage.json");

    fs::read_to_string(&path)
        .unwrap_or_else(|err| panic!("cannot read {}: {}", path.display(), err))
}

// Keywords that make up Jarvil itself.
const LANGUAGE_KEYWORDS: [&str; 26] = [
    "if",
    "elif",
    "else",
    "for",
    "while",
    "continue",
    "break",
    "return",
    "match",
    "case",
    "in",
    "def",
    "let",
    "type",
    "interface",
    "struct",
    "enum",
    "lambda",
    "declare",
    "implements",
    "self",
    "and",
    "or",
    "not",
    "True",
    "False",
];

// Reserved purely so the generated Python stays valid. Using one as an
// identifier is a syntax error, so the grammar flags them as invalid.
const PYTHON_RESERVED: [&str; 20] = [
    "None",
    "as",
    "assert",
    "class",
    "del",
    "except",
    "finally",
    "from",
    "global",
    "import",
    "is",
    "nonlocal",
    "pass",
    "raise",
    "try",
    "with",
    "yield",
    "async",
    "await",
    "__peg_parser__",
];

const ATOMIC_TYPES: [&str; 4] = ["int", "float", "str", "bool"];

fn assert_all_present(words: &[&str], grammar: &str, what: &str) {
    let missing: Vec<&str> = words
        .iter()
        .copied()
        .filter(|word| !grammar.contains(word))
        .collect();

    assert!(
        missing.is_empty(),
        "{} missing from the TextMate grammar: {:?}\n\
         add them to extensions/jarvil-vscode/syntaxes/jarvil.tmLanguage.json",
        what,
        missing
    );
}

#[test]
fn grammar_covers_every_language_keyword() {
    assert_all_present(&LANGUAGE_KEYWORDS, &grammar_source(), "language keywords");
}

#[test]
fn grammar_covers_every_reserved_word() {
    assert_all_present(&PYTHON_RESERVED, &grammar_source(), "reserved words");
}

#[test]
fn grammar_covers_every_atomic_type() {
    assert_all_present(&ATOMIC_TYPES, &grammar_source(), "atomic types");
}

// The lists above are written out by hand so that this test is readable; this
// guards against *that* copy drifting from `constants::common`, which is the
// list the lexer actually uses.
#[test]
fn keyword_lists_match_the_lexer_constants() {
    let constants: PathBuf = [env!("CARGO_MANIFEST_DIR"), "src", "constants", "common.rs"]
        .iter()
        .collect();

    let source = fs::read_to_string(&constants).unwrap();

    // every keyword listed here must still exist as a constant's value
    for word in LANGUAGE_KEYWORDS
        .iter()
        .chain(PYTHON_RESERVED.iter())
        .chain(ATOMIC_TYPES.iter())
    {
        assert!(
            source.contains(&format!("= \"{}\";", word)),
            "`{}` is no longer a keyword in constants::common -- \
             update the lists in this test and the TextMate grammar",
            word
        );
    }
}
