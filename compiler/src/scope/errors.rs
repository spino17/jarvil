use text_size::TextRange;

#[derive(Debug)]
pub enum GenericTypeArgsCheckError {
    GenericTypeArgsIncorrectlyBounded(Vec<(TextRange, String)>),
    GenericTypeArgsNotExpected,
    GenericTypeArgsExpected,
    GenericTypeArgsCountMismatched(usize, usize), // (received, expected)
}
