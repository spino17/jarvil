use std::rc::Rc;
/*
pub fn get_token_value(code: &Vec<char>, start_index: usize, end_index: Option<usize>) -> String {
    match end_index {
        Some(end_index) => code[start_index..end_index].iter().collect(),
        None => code[start_index..].iter().collect()
    }
}

pub fn get_code_line_str(code: &Vec<char>, code_lines: &Vec<usize>, line_number: usize) -> String {
    let start_index = code_lines[line_number - 1];
    let end_index = if line_number == code_lines.len() {
        None
    } else {
        Some(code_lines[line_number] - 1)
    };
    let mut code_line_str: String = get_token_value(code, start_index, end_index);
    code_line_str.push(' ');
    return code_line_str
}

pub fn get_code_lines_str(code: &Rc<Vec<char>>, code_lines: &Vec<usize>, 
    start_line_number: usize, end_line_number: usize) -> Vec<Rc<String>> {
    let mut code_lines_vec: Vec<Rc<String>> = vec![];
    for line_number in start_line_number..(end_line_number + 1) {
        code_lines_vec.push(Rc::new(get_code_line_str(code, code_lines, line_number)))
    }
    code_lines_vec
}

pub fn get_code_line_data(code: &Vec<char>, code_lines: &Vec<usize>, mut curr_line_number: usize, 
    index: usize) -> (Rc<String>, usize, usize, usize) {
    loop {
        let line_start_index = code_lines[curr_line_number - 1];
        if index >= line_start_index {
            let s = Rc::new(get_code_line_str(code, code_lines, curr_line_number));
            return (s.clone(), line_start_index, curr_line_number, index)
        }
        curr_line_number = curr_line_number - 1;
    }
}
 */
