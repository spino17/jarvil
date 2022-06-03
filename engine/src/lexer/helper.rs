use std::rc::Rc;
use crate::{lexer::token::CoreToken, errors::LexicalError, context};
use super::token::TokenValue;
use crate::constants::common::get_token_for_identifier;

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
                unreachable!("any state other than 0, 1, 2 and 3 is not reachable")
            }
        }
        forward_lexeme = forward_lexeme + 1;
    }
    match state {
        0 => {
            *begin_lexeme = *begin_lexeme + 1;
            return Ok(CoreToken::SLASH);
        },
        1 => {
            Err(LexicalError::new("no newline terminal found for line comment"))
        },
        2 => {
            Err(LexicalError::new("no closing tag found for block comment"))
        },
        3 => unreachable!("found state 3 which is not possible as state 3 either returns or always transition to state 2"),
        _ => unreachable!("any state other than 0, 1, 2 and 3 is not reachable")
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
                return Ok(CoreToken::DOUBLE_EQUAL);
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return Ok(CoreToken::EQUAL);
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

// " -> "......"
pub fn extract_literal_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<CoreToken, LexicalError> {
    let mut forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme == code.len() {
        return Err(LexicalError::new(r#"no closing " found for literal"#))
    }
    while forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '"' => {
                let literal_value: String = code[(*begin_lexeme + 1)..(forward_lexeme)].iter().collect();
                *begin_lexeme = forward_lexeme + 1;
                return Ok(CoreToken::LITERAL(TokenValue(Rc::new(literal_value))))
            },
            _ => {}
        }
        forward_lexeme = forward_lexeme + 1;
    }
    Err(LexicalError::new(r#"no closing " found for literal"#))
}

// letter -> letter((letter|digit|_)*) or keyword or type
pub fn extract_letter_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<CoreToken, LexicalError> {
    // at the end check whether value is keyword, type or identifier
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
    return Ok(get_token_for_identifier(value));
}

// digit -> digit((digit)*(.digit(digit*)|empty))
pub fn extract_digit_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> Result<CoreToken, LexicalError> {
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
                    return Ok(CoreToken::INTEGER(TokenValue(Rc::new(value))))
                }
            },
            1 => {
                if context::is_digit(&next_char) {
                    state = 2;
                } else {
                    return Err(LexicalError::new("expected at least one digit after '.'"))
                }
            },
            2 => {
                if context::is_digit(&next_char) {
                    // do nothing
                } else {
                    let value: String = code[*begin_lexeme..(forward_lexeme)].iter().collect();
                    *begin_lexeme = forward_lexeme;
                    return Ok(CoreToken::FLOAT(TokenValue(Rc::new(value))))
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
            return Ok(CoreToken::NUMBER(TokenValue(Rc::new(value))))
        },
        1 => {
            unreachable!("found state 1 which is not possible as state 1 either returns or always transition to state 2")
        },
        2 => {
            let value: String = code[*begin_lexeme..(forward_lexeme)].iter().collect();
            *begin_lexeme = forward_lexeme;
            return Ok(CoreToken::FLOAT(TokenValue(Rc::new(value))))
        },
        _ => unreachable!("any state other than 0, 1, 2 and 3 is not reachable")
    }
}