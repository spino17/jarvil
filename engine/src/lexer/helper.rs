use std::rc::Rc;
use crate::{lexer::token::CoreToken, errors::LexicalError, context};
use super::token::TokenValue;
use crate::constants::common::get_token_for_identifier;

// + -> +, ++
/*
pub fn extract_plus_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<(CoreToken, String), LexicalError> {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '+' => {
                *begin_lexeme = forward_lexeme + 1;  // retract true
                return Ok((CoreToken::DOUBLE_PLUS, String::from("++")));
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;  // retract false
                return Ok((CoreToken::PLUS, String::from("+")));
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;  // retract false
        return Ok((CoreToken::PLUS, String::from("+")));
    }
}
 */

// - -> -, --, ->
pub fn extract_minus_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<(CoreToken, String), LexicalError> {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            /*
            '-' => {
                *begin_lexeme = forward_lexeme + 1;
                return Ok((CoreToken::DOUBLE_MINUS, String::from("--")));
            },
             */
            '>' => {
                *begin_lexeme = forward_lexeme + 1;
                return Ok((CoreToken::RIGHT_ARROW, String::from("->")));
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return Ok((CoreToken::MINUS, String::from("-")));
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return Ok((CoreToken::MINUS, String::from("-")));
    }
}

// * -> *, **
pub fn extract_star_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<(CoreToken, String), LexicalError> {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '*' => {
                *begin_lexeme = forward_lexeme + 1;
                return Ok((CoreToken::DOUBLE_STAR, String::from("**")));
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return Ok((CoreToken::STAR, String::from("*")));
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return Ok((CoreToken::STAR, String::from("*")));
    }
}

// / -> /, /*, //
pub fn extract_slash_prefix_lexeme(begin_lexeme: &mut usize, 
    line_number: &mut usize, code: &Vec<char>, 
    code_lines: &mut Vec<(Rc<String>, usize)>, line_start_index: &mut usize) -> Result<(CoreToken, String), LexicalError> {
    let mut forward_lexeme = *begin_lexeme + 1;
    let mut state: usize = 0;
    while forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        if next_char == '\n' {
            code_lines.push((Rc::new(code[*line_start_index..forward_lexeme].iter().collect()), *line_start_index));
            *begin_lexeme = *begin_lexeme + 1;
            *line_number = *line_number + 1;
            *line_start_index = forward_lexeme + 1;
        }
        match state {
            0 => {
                match next_char {
                    '/' => {
                        state = 1;
                    },
                    '*' => {
                        state = 2;
                    },
                    _ => {
                        *begin_lexeme = *begin_lexeme + 1;
                        return Ok((CoreToken::SLASH, String::from("/")));
                    }
                }
            },
            1 => {
                match next_char {
                    '\n' => {
                        *begin_lexeme = forward_lexeme + 1;
                        return Ok((CoreToken::SINGLE_LINE_COMMENT, String::from("single_comment")));
                    },
                    _ => {}
                }
            },
            2 => {
                match next_char {
                    '*' => {
                        state = 3;
                    },
                    _ => {}
                }
            },
            3 => {
                match next_char {
                    '/' => {
                        *begin_lexeme = forward_lexeme + 1;
                        return Ok((CoreToken::BLOCK_COMMENT, String::from("block_comment")));
                    },
                    _ => {
                        state = 2;
                    }
                }
            }
            _ => {
                unreachable!("any state other than 0, 1, 2 and 3 is not reachable")
            }
        }
        forward_lexeme = forward_lexeme + 1;
    }
    match state {
        0 => {
            *begin_lexeme = *begin_lexeme + 1;
            return Ok((CoreToken::SLASH, String::from("/")));
        },
        1 => {
            *begin_lexeme = forward_lexeme;
            return Ok((CoreToken::SINGLE_LINE_COMMENT, String::from("single_comment")));
            // Err(LexicalError::new(*line_number, String::from("no newline terminal found for line comment")))
        },
        2 => {
            Err(LexicalError::new(*line_number, String::from("no closing tag found for block comment")))
        },
        3 => unreachable!("found state 3 which is not possible as state 3 either returns or always transition to state 2"),
        _ => unreachable!("any state other than 0, 1, 2 and 3 is not reachable")
    }
}

// # -> #......\n
pub fn extract_hash_prefix_lexeme(begin_lexeme: &mut usize, 
    line_number: &mut usize, code: &Vec<char>, 
    code_lines: &mut Vec<(Rc<String>, usize)>, line_start_index: &mut usize) -> Result<(CoreToken, String), LexicalError> {
    let mut forward_lexeme = *begin_lexeme + 1;
    while forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        if next_char == '\n' {
            code_lines.push((Rc::new(code[*line_start_index..forward_lexeme].iter().collect()), *line_start_index));
            *begin_lexeme = *begin_lexeme + 1;
            *line_number = *line_number + 1;
            *line_start_index = forward_lexeme + 1;
        }
        match next_char {
            '\n' => {
                *begin_lexeme = forward_lexeme + 1;
                return Ok((CoreToken::SINGLE_LINE_COMMENT, String::from("single_comment")))
            },
            _ => {}
        }
        forward_lexeme = forward_lexeme + 1;
    }
    *begin_lexeme = forward_lexeme;
    return Ok((CoreToken::SINGLE_LINE_COMMENT, String::from("single_comment")))
    // Err(LexicalError::new(*line_number, String::from("no newline terminal found for line comment")))
}

// = -> =, ==
pub fn extract_equal_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<(CoreToken, String), LexicalError> {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return Ok((CoreToken::DOUBLE_EQUAL, String::from("==")));
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return Ok((CoreToken::EQUAL, String::from("=")));
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return Ok((CoreToken::EQUAL, String::from("=")));
    }
}

// > -> >, >=
pub fn extract_greater_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<(CoreToken, String), LexicalError> {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return Ok((CoreToken::GREATER_EQUAL, String::from(">=")));
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return Ok((CoreToken::GREATER, String::from(">")));
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return Ok((CoreToken::GREATER, String::from(">")));
    }
}

// < -> <, <=
pub fn extract_less_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<(CoreToken, String), LexicalError> {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return Ok((CoreToken::LESS_EQUAL, String::from("<=")));
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return Ok((CoreToken::LESS, String::from("<")));
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return Ok((CoreToken::LESS, String::from("<")));
    }
}

// " -> "......"
pub fn extract_literal_prefix_lexeme(begin_lexeme: &mut usize, 
    line_number: &mut usize, code: &Vec<char>) -> Result<(CoreToken, String), LexicalError> {
    let mut forward_lexeme = *begin_lexeme + 1;
    while forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        if next_char == '\n' {
            *line_number = *line_number + 1;
        }
        match next_char {
            '"' => {
                let literal_value: String = code[(*begin_lexeme + 1)..(forward_lexeme)].iter().collect();
                *begin_lexeme = forward_lexeme + 1;
                return Ok((CoreToken::LITERAL(TokenValue(Rc::new(literal_value))), String::from("literal")))
            },
            _ => {}
        }
        forward_lexeme = forward_lexeme + 1;
    }
    Err(LexicalError::new(*line_number, String::from(r#"no closing " found for literal"#)))
}

// letter -> letter((letter|digit|_)*) or keyword or type
pub fn extract_letter_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<(CoreToken, String), LexicalError> {
    let mut forward_lexeme = *begin_lexeme + 1;
    while forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        if context::is_letter(&next_char) || context::is_digit(&next_char) {
            // do nothing
        } else {
            let value: String = code[(*begin_lexeme)..(forward_lexeme)].iter().collect();
            *begin_lexeme = forward_lexeme;
            return Ok(get_token_for_identifier(value));
        }
        forward_lexeme = forward_lexeme + 1;
    }
    let value: String = code[(*begin_lexeme)..(forward_lexeme)].iter().collect();
    *begin_lexeme = forward_lexeme;
    return Ok(get_token_for_identifier(value));
}

// digit -> digit((digit)*(.digit(digit*)|empty))
pub fn extract_digit_prefix_lexeme(begin_lexeme: &mut usize, 
    line_number: &mut usize, code: &Vec<char>) -> Result<(CoreToken, String), LexicalError> {
    let mut forward_lexeme = *begin_lexeme + 1;
    let mut state: usize = 0;
    while forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match state {
            0 => {
                if context::is_digit(&next_char) {
                    // do nothing
                } else if next_char == '.' {
                    state = 1;
                } else {
                    let value: String = code[*begin_lexeme..(forward_lexeme)].iter().collect();
                    *begin_lexeme = forward_lexeme;
                    return Ok((CoreToken::INTEGER(TokenValue(Rc::new(value))), String::from("int")))
                }
            },
            1 => {
                if context::is_digit(&next_char) {
                    state = 2;
                } else {
                    return Err(LexicalError::new(*line_number, String::from("expected at least one digit after '.'")))
                }
            },
            2 => {
                if context::is_digit(&next_char) {
                    // do nothing
                } else {
                    let value: String = code[*begin_lexeme..(forward_lexeme)].iter().collect();
                    *begin_lexeme = forward_lexeme;
                    return Ok((CoreToken::FLOAT(TokenValue(Rc::new(value))), String::from("float")))
                }
            },
            _ => {
                unreachable!("any state other than 0, 1, 2 and 3 is not reachable")
            }
        }
        forward_lexeme = forward_lexeme + 1;
    }
    match state {
        0 => {
            let value: String = code[*begin_lexeme..(forward_lexeme)].iter().collect();
            *begin_lexeme = forward_lexeme;
            return Ok((CoreToken::INTEGER(TokenValue(Rc::new(value))), String::from("int")))
        },
        1 => {
            return Err(LexicalError::new(*line_number, String::from("expected at least one digit after '.'")))
        },
        2 => {
            let value: String = code[*begin_lexeme..(forward_lexeme)].iter().collect();
            *begin_lexeme = forward_lexeme;
            return Ok((CoreToken::FLOAT(TokenValue(Rc::new(value))), String::from("float")))
        },
        _ => unreachable!("any state other than 0, 1, 2 and 3 is not reachable")
    }
}