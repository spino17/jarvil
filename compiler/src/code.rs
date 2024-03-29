use text_size::TextRange;

pub struct JarvilCodeHandler<'a> {
    pub code: &'a JarvilCode,
    code_lines: Vec<usize>,
}

impl<'a> JarvilCodeHandler<'a> {
    pub fn new(code: &'a JarvilCode, code_lines: Vec<usize>) -> Self {
        JarvilCodeHandler { code, code_lines }
    }

    pub fn line_start_index(&self, line_number: usize) -> usize {
        self.code_lines[line_number - 1]
    }
}

#[derive(Debug)]
pub struct JarvilCode(Vec<char>);

impl JarvilCode {
    pub fn new(code: &str) -> Self {
        JarvilCode(code.chars().collect())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get_char(&self, index: usize) -> char {
        self.0[index]
    }

    pub fn token_value(&self, start_index: usize, end_index: Option<usize>) -> String {
        match end_index {
            Some(end_index) => self.0[start_index..end_index].iter().collect(),
            None => self.0[start_index..].iter().collect(),
        }
    }

    pub fn token_from_range(&self, range: TextRange) -> String {
        self.token_value(range.start().into(), Some(range.end().into()))
    }

    pub fn token_value_as_iter(
        &self,
        start_index: usize,
        end_index: Option<usize>,
    ) -> std::slice::Iter<char> {
        match end_index {
            Some(end_index) => self.0[start_index..end_index].iter(),
            None => self.0[start_index..].iter(),
        }
    }
}

impl ToString for JarvilCode {
    fn to_string(&self) -> String {
        self.0.iter().collect()
    }
}
