use crate::{lexer::token::CoreToken, errors::LexicalError};

// + -> +, ++
pub fn extract_plus_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<CoreToken, LexicalError> {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '+' => {
                *begin_lexeme = forward_lexeme + 1;  // retract true
                return Ok(CoreToken::DOUBLE_PLUS);
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;  // retract false
                return Ok(CoreToken::PLUS);
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;  // retract false
        return Ok(CoreToken::PLUS);
    }
}

// - -> -, --
pub fn extract_minus_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<CoreToken, LexicalError> {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '-' => {
                *begin_lexeme = forward_lexeme + 1;
                return Ok(CoreToken::DOUBLE_MINUS);
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return Ok(CoreToken::MINUS);
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return Ok(CoreToken::MINUS);
    }
}

// * -> *, **
pub fn extract_star_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<CoreToken, LexicalError> {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '*' => {
                *begin_lexeme = forward_lexeme + 1;
                return Ok(CoreToken::DOUBLE_STAR);
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return Ok(CoreToken::STAR);
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return Ok(CoreToken::STAR);
    }
}

// / -> /, /*, //
pub fn extract_slash_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<CoreToken, LexicalError> {
    let mut forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme == code.len() {
        *begin_lexeme = *begin_lexeme + 1;
        return Ok(CoreToken::SLASH);
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
                        return Ok(CoreToken::SLASH);
                    }
                }
            },
            1 => {
                match next_char {
                    '\n' => {
                        *begin_lexeme = forward_lexeme + 1;
                        return Ok(CoreToken::SINGLE_LINE_COMMENT);
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
                        return Ok(CoreToken::BLOCK_COMMENT);
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
        0 => unreachable!(),  // if we get to this point in state 0, it's a bug
        1 => {
            Err(LexicalError::new("no newline terminal found for line comment"))
        },
        2 => {
            Err(LexicalError::new("no closing tag found for block comment"))
        },
        3 => unreachable!(),
        _ => unreachable!()
    }
}

// = -> =, ==
pub fn extract_equal_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<CoreToken, LexicalError> {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return Ok(CoreToken::EQUAL);
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return Ok(CoreToken::DOUBLE_EQUAL);
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return Ok(CoreToken::EQUAL);
    }
}

// > -> >, >=
pub fn extract_greater_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<CoreToken, LexicalError> {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return Ok(CoreToken::GREATER_EQUAL);
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return Ok(CoreToken::GREATER);
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return Ok(CoreToken::GREATER);
    }
}

// < -> <, <=
pub fn extract_less_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<CoreToken, LexicalError> {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return Ok(CoreToken::LESS_EQUAL);
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return Ok(CoreToken::LESS);
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return Ok(CoreToken::LESS);
    }
}