use std::rc::Rc;

pub fn get_token_value(code: &Vec<char>, start_index: usize, end_index: usize) -> String {
    code[start_index..end_index].iter().collect()
}

pub fn get_code_line_data(code_lines: &Vec<(Rc<String>, usize)>,
mut curr_line_number: usize, index: usize) -> (Rc<String>, usize, usize, usize) {
    loop {
        let (s, line_start_index) = &code_lines[curr_line_number - 1];
        if index >= *line_start_index {
            return (s.clone(), *line_start_index, curr_line_number, index)
        }
        curr_line_number = curr_line_number - 1;
    }
}