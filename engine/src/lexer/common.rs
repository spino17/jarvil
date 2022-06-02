use crate::lexer::token::CoreToken;

// + -> +, ++
pub fn extract_plus_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> CoreToken {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '+' => {
                *begin_lexeme = forward_lexeme + 1;  // retract true
                return CoreToken::DOUBLE_PLUS
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;  // retract false
                return CoreToken::PLUS
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
                return CoreToken::DOUBLE_MINUS
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return CoreToken::MINUS
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
                return CoreToken::DOUBLE_STAR
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return CoreToken::STAR
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return CoreToken::STAR;
    }
}

// / -> /, /*, //
pub fn extract_slash_prefix_lexeme(begin_lexeme: &mut usize, code: &Vec<char>) -> CoreToken {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code[forward_lexeme];
        match next_char {
            '/' => {
                *begin_lexeme = forward_lexeme + 1;
                return CoreToken::DOUBLE_SLASH
            },
            '*' => {
                *begin_lexeme = forward_lexeme + 1;
                return CoreToken::LCOMMENT
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return CoreToken::STAR
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return CoreToken::STAR;
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