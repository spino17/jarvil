use std::rc::Rc;
use text_size::TextRange;

pub struct Code {
    code_vec: Rc<Vec<char>>,
    code_lines: Option<Rc<Vec<usize>>>,
}

impl Code {
    pub fn new(code_vec: Vec<char>) -> Self {
        Code {
            code_vec: Rc::new(code_vec),
            code_lines: None,
        }
    }

    pub fn extract_code_lines(&self) -> Rc<Vec<usize>> {
        match &self.code_lines {
            Some(code_lines) => return code_lines.clone(),
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
        self.code_lines = Some(Rc::new(code_lines));
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

    pub fn line(&self, line_number: usize) -> String {
        let code_lines = self.extract_code_lines();
        let start_index = code_lines[line_number - 1];
        let end_index = if line_number >= code_lines.len() {
            None
        } else {
            Some(code_lines[line_number] - 1)
        };
        let mut code_line_str: String = self.token_value(start_index, end_index);
        code_line_str.push(' ');
        return code_line_str;
    }

    pub fn lines(&self, start_line_number: usize, end_line_number: usize) -> Vec<String> {
        let mut code_lines_vec: Vec<String> = vec![];
        for line_number in start_line_number..(end_line_number + 1) {
            code_lines_vec.push(self.line(line_number))
        }
        code_lines_vec
    }

    pub fn line_len(&self, line_number: usize) -> usize {
        let code_lines = self.extract_code_lines();
        let start_index = code_lines[line_number - 1];
        let end_index = if line_number >= code_lines.len() {
            self.code_vec.len()
        } else {
            code_lines[line_number] - 1
        };
        end_index - start_index
    }

    pub fn line_range_from_indexes(
        &self,
        start_index: usize,
        end_index: usize,
        curr_line_number: usize,
    ) -> (usize, usize) {
        let (start_line_number, _) = self.line_number_from_index(curr_line_number, start_index);
        let code_lines = self.extract_code_lines();
        let mut curr_line_number = start_line_number;
        loop {
            let curr_start_index = code_lines[curr_line_number - 1];
            if end_index < curr_start_index {
                break;
            }
            curr_line_number = curr_line_number + 1;
        }
        let end_line_number = curr_line_number - 1;
        (start_line_number, end_line_number)
    }

    pub fn line_number_from_index(
        &self,
        mut curr_line_number: usize,
        index: usize,
    ) -> (usize, usize) {
        let code_lines = self.extract_code_lines();
        loop {
            let line_start_index = code_lines[curr_line_number - 1];
            if index >= line_start_index {
                return (curr_line_number, line_start_index);
            }
            curr_line_number = curr_line_number - 1;
        }
    }

    pub fn line_data(
        &self,
        curr_line_number: usize,
        index: usize,
    ) -> (String, usize, usize, usize) {
        let (curr_line_number, line_start_index) =
            self.line_number_from_index(curr_line_number, index);
        let s = self.line(curr_line_number);
        (s, line_start_index, curr_line_number, index)
    }
}

impl Clone for Code {
    fn clone(&self) -> Self {
        Code {
            code_vec: self.code_vec.clone(),
            code_lines: match &self.code_lines {
                Some(code_lines) => Some(code_lines.clone()),
                None => None,
            },
        }
    }
}
