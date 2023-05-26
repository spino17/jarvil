use text_size::TextRange;

pub struct JarvilCode {
    code_vec: Vec<char>,
    code_lines: Option<Vec<usize>>
}

impl JarvilCode {
    pub fn new(code_vec: Vec<char>) -> Self {
        JarvilCode {
            code_vec: code_vec,
            code_lines: None,
        }
    }

    pub fn extract_code_lines(&self) -> &Vec<usize> {
        match &self.code_lines {
            Some(code_lines) => return code_lines,
            None => unreachable!(
                "this method should always be called once code_lines has been set by the lexer"
            ),
        }
    }

    pub fn len(&self) -> usize {
        self.code_vec.len()
    }

    pub fn get_char(&self, index: usize) -> char {
        self.code_vec[index]
    }

    pub fn get_line_start_index(&self, line_number: usize) -> usize {
        let code_lines = self.extract_code_lines();
        code_lines[line_number - 1]
    }

    pub fn set_code_lines(&mut self, code_lines: Vec<usize>) {
        self.code_lines = Some(code_lines);
    }

    pub fn token_value(&self, start_index: usize, end_index: Option<usize>) -> String {
        match end_index {
            Some(end_index) => self.code_vec[start_index..end_index].iter().collect(),
            None => self.code_vec[start_index..].iter().collect(),
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
            Some(end_index) => self.code_vec[start_index..end_index].iter(),
            None => self.code_vec[start_index..].iter(),
        }
    }
}

impl Clone for JarvilCode {
    fn clone(&self) -> Self {
        JarvilCode {
            code_vec: self.code_vec.clone(),
            code_lines: match &self.code_lines {
                Some(code_lines) => Some(code_lines.clone()),
                None => None,
            },
        }
    }
}
