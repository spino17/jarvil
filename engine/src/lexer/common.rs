use crate::{lexer::token::CoreToken, errors::LexicalError};

// + -> +, ++
pub fn extract_plus_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> CoreToken {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '+' => {
                *begin_lexeme = forward_lexeme + 1;  // retract true
                return CoreToken::DOUBLE_PLUS;
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;  // retract false
                return CoreToken::PLUS;
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;  // retract false
        return CoreToken::PLUS;
    }
}

// - -> -, --
pub fn extract_minus_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> CoreToken {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '-' => {
                *begin_lexeme = forward_lexeme + 1;
                return CoreToken::DOUBLE_MINUS;
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return CoreToken::MINUS;
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return CoreToken::MINUS;
    }
}

// * -> *, **
pub fn extract_star_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> CoreToken {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '*' => {
                *begin_lexeme = forward_lexeme + 1;
                return CoreToken::DOUBLE_STAR;
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return CoreToken::STAR;
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return CoreToken::STAR;
    }
}

// / -> /, /*, //
pub fn extract_slash_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> CoreToken {
    let mut forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme == code.len() {
        *begin_lexeme = *begin_lexeme + 1;
        return CoreToken::SLASH;
    }
    let mut state: usize = 0;
    while forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
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
                        return CoreToken::SLASH;
                    }
                }
            },
            1 => {
                match next_char {
                    '\n' => {
                        *begin_lexeme = forward_lexeme + 1;
                        return CoreToken::SINGLE_LINE_COMMENT;
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
                        return CoreToken::BLOCK_COMMENT;
                    },
                    _ => {
                        state = 2;
                    }
                }
            }
            _ => {
                unreachable!()  // if we got unknown state then it's a bug
            }
        }
        forward_lexeme = forward_lexeme + 1;
    }
    match state {
        0 => {
            unreachable!()  // if we get to this point in state 0, it's a bug
        },
        1 => {
            Err(LexicalError{})  // TODO - did not found any newline
        },
        2 => {
            Err(LexicalError{})  // TODO - did not found closing tag for block comment
        },
        3 => {
            unreachable!()
        }
    }
}

// = -> =, ==
pub fn extract_equal_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> CoreToken {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return CoreToken::EQUAL
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return CoreToken::DOUBLE_EQUAL
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return CoreToken::EQUAL;
    }
}

// > -> >, >=
pub fn extract_greater_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> CoreToken {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return CoreToken::GREATER_EQUAL
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return CoreToken::GREATER
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return CoreToken::GREATER;
    }
}

// < -> <, <=
pub fn extract_less_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> CoreToken {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return CoreToken::LESS_EQUAL
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return CoreToken::LESS
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return CoreToken::LESS;
    }
}