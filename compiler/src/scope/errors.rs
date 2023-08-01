#[derive(Debug)]
pub enum GenericTypeArgsCheckError {
    GenericTypeArgsNotExpected,
    GenericTypeArgsExpected,
    GenericTypeArgsCountMismatched(usize, usize), // (received, expected)
}
