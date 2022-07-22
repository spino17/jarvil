use std::rc::Rc;

pub struct Code {
    code_vec: Rc<Vec<char>>,
    code_lines: Rc<Vec<usize>>,
}

impl Code {
    pub fn new(code_vec: Vec<char>) -> Self {
        Code {
            code_vec: Rc::new(code_vec),
            code_lines: Rc::new(vec![]),
        }
    }
    
    pub fn get_token_value(&self, start_index: usize, end_index: Option<usize>) -> String {
        match end_index {
            Some(end_index) => self.code_vec[start_index..end_index].iter().collect(),
            None => self.code_vec[start_index..].iter().collect()
        }
    }
    
    pub fn get_code_line_str(&self, line_number: usize) -> String {
        let start_index = self.code_lines[line_number - 1];
        let end_index = if line_number == self.code_lines.len() {
            None
        } else {
            Some(self.code_lines[line_number] - 1)
        };
        let mut code_line_str: String = self.get_token_value(start_index, end_index);
        code_line_str.push(' ');
        return code_line_str
    }
    
    pub fn get_code_lines_str(&self, start_line_number: usize, end_line_number: usize) -> Vec<Rc<String>> {
        let mut code_lines_vec: Vec<Rc<String>> = vec![];
        for line_number in start_line_number..(end_line_number + 1) {
            code_lines_vec.push(Rc::new(self.get_code_line_str(line_number)))
        }
        code_lines_vec
    }
    
    pub fn get_code_line_data(&self, mut curr_line_number: usize, index: usize) -> (Rc<String>, usize, usize, usize) {
        loop {
            let line_start_index = self.code_lines[curr_line_number - 1];
            if index >= line_start_index {
                let s = Rc::new(self.get_code_line_str(curr_line_number));
                return (s.clone(), line_start_index, curr_line_number, index)
            }
            curr_line_number = curr_line_number - 1;
        }
    }
}

impl Clone for Code {
    fn clone(&self) -> Self {
        Code {
            code_vec: self.code_vec.clone(),
            code_lines: self.code_lines.clone(),
        }
    }
}