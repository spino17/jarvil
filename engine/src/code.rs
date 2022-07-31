use std::rc::Rc;

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

    pub fn line_data(
        &self,
        mut curr_line_number: usize,
        index: usize,
    ) -> (String, usize, usize, usize) {
        let code_lines = self.extract_code_lines();
        loop {
            let line_start_index = code_lines[curr_line_number - 1];
            if index >= line_start_index {
                let s = self.line(curr_line_number);
                return (s, line_start_index, curr_line_number, index);
            }
            curr_line_number = curr_line_number - 1;
        }
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
