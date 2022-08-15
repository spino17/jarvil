pub struct CoreError {
    start_index: usize,
    end_index: usize,
    message: String,
}

pub struct SyntaxError {
    core: CoreError,
    start_line_number: usize,
    end_line_number: usize,
}

pub struct SemanticError {
    core: CoreError,
}

pub enum JarvilError {
    IO(std::io::Error),
    SYNTAX(SyntaxError),
    SEMANTIC(SemanticError),
}
