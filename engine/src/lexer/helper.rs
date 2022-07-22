use std::rc::Rc;
use crate::{lexer::token::CoreToken, context, constants::common::{LEXICAL_ERROR, BLOCK_COMMENT, SINGLE_LINE_COMMENT, BLANK}, code::Code};
use super::token::{LexicalErrorKind};
use crate::constants::common::{get_token_for_identifier, LITERAL, INTEGER, FLOATING_POINT_NUMBER};

// ' ' -> '...'
pub fn extract_blank_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> (CoreToken, String) {
    let mut forward_lexeme = *begin_lexeme + 1;
    while forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        if next_char != ' ' {
            *begin_lexeme = forward_lexeme;
            return (CoreToken::BLANK, String::from(BLANK));
        }
        forward_lexeme = forward_lexeme + 1;
    }
    *begin_lexeme = forward_lexeme;
    return (CoreToken::BLANK, String::from(BLANK));
}

// - -> -, ->
pub fn extract_dash_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> (CoreToken, String) {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '>' => {
                *begin_lexeme = forward_lexeme + 1;
                return (CoreToken::RIGHT_ARROW, String::from("->"));
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return (CoreToken::DASH, String::from("-"));
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return (CoreToken::DASH, String::from("-"));
    }
}

// * -> *, **
pub fn extract_star_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> (CoreToken, String) {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '*' => {
                *begin_lexeme = forward_lexeme + 1;
                return (CoreToken::DOUBLE_STAR, String::from("**"));
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return (CoreToken::STAR, String::from("*"));
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return (CoreToken::STAR, String::from("*"));
    }
}

// / -> /, /*, //
pub fn extract_slash_prefix_lexeme(begin_lexeme: &mut usize, 
    line_number: &mut usize, code: &Code,
    code_lines: &mut Vec<usize>, line_start_index: &mut usize) -> (CoreToken, String) {
    let mut forward_lexeme = *begin_lexeme + 1;
    let mut state: usize = 0;
    while forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
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
                        return (CoreToken::SLASH, String::from("/"));
                    }
                }
            },
            1 => {
                match next_char {
                    '\n' => {
                        *begin_lexeme = forward_lexeme;
                        return (CoreToken::SINGLE_LINE_COMMENT, String::from(SINGLE_LINE_COMMENT));
                    },
                    _ => {}
                }
            },
            2 => {
                match next_char {
                    '*' => {
                        state = 3;
                    },
                    '\n' => {
                        code_lines.push(*line_start_index);
                        *line_number = *line_number + 1;
                        *line_start_index = forward_lexeme + 1;
                    },
                    _ => {}
                }
            },
            3 => {
                match next_char {
                    '/' => {
                        *begin_lexeme = forward_lexeme + 1;
                        return (CoreToken::BLOCK_COMMENT, String::from(BLOCK_COMMENT));
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
            return (CoreToken::SLASH, String::from("/"));
        },
        1 => {
            *begin_lexeme = forward_lexeme;
            return (CoreToken::SINGLE_LINE_COMMENT, String::from(SINGLE_LINE_COMMENT));
        },
        2 => {
            *begin_lexeme = forward_lexeme;
            let err_str = Rc::new(String::from("missing trailing symbol `*/` for block comment"));
            return (CoreToken::LEXICAL_ERROR((LexicalErrorKind::NO_CLOSING_SYMBOLS, err_str)), String::from(LEXICAL_ERROR))
        },
        3 => {
            *begin_lexeme = forward_lexeme;
            let err_str = Rc::new(String::from("missing trailing symbol `*/` for block comment"));
            return (CoreToken::LEXICAL_ERROR((LexicalErrorKind::NO_CLOSING_SYMBOLS, err_str)), String::from(LEXICAL_ERROR))
        },
        _ => unreachable!("any state other than 0, 1, 2 and 3 is not reachable")
    }
}

// # -> #...\n
pub fn extract_hash_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> (CoreToken, String) {
    let mut forward_lexeme = *begin_lexeme + 1;
    while forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '\n' => {
                *begin_lexeme = forward_lexeme;
                return (CoreToken::SINGLE_LINE_COMMENT, String::from(SINGLE_LINE_COMMENT))
            },
            _ => {}
        }
        forward_lexeme = forward_lexeme + 1;
    }
    *begin_lexeme = forward_lexeme;
    return (CoreToken::SINGLE_LINE_COMMENT, String::from(SINGLE_LINE_COMMENT))
}

// = -> =, ==
pub fn extract_equal_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> (CoreToken, String) {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return (CoreToken::DOUBLE_EQUAL, String::from("=="));
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return (CoreToken::EQUAL, String::from("="));
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return (CoreToken::EQUAL, String::from("="));
    }
}

// > -> >, >=
pub fn extract_rbracket_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> (CoreToken, String) {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return (CoreToken::GREATER_EQUAL, String::from(">="));
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return (CoreToken::RBRACKET, String::from(">"));
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return (CoreToken::RBRACKET, String::from(">"));
    }
}

// < -> <, <=
pub fn extract_lbracket_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> (CoreToken, String) {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return (CoreToken::LESS_EQUAL, String::from("<="));
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return (CoreToken::LBRACKET, String::from("<"));
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return (CoreToken::LBRACKET, String::from("<"));
    }
}

// ! -> !=
pub fn extract_exclaimation_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> (CoreToken, String) {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return (CoreToken::NOT_EQUAL, String::from("!="));
            },
            _ => {
                let error_str = Rc::new(String::from("invalid character `!` found"));
                *begin_lexeme = *begin_lexeme + 1;
                (
                    CoreToken::LEXICAL_ERROR((LexicalErrorKind::INVALID_CHAR, error_str.clone())),
                    String::from(LEXICAL_ERROR)
                )
            }
        }
    } else {
        let error_str = Rc::new(String::from("invalid character `!` found"));
        *begin_lexeme = *begin_lexeme + 1;
        (
            CoreToken::LEXICAL_ERROR((LexicalErrorKind::INVALID_CHAR, error_str.clone())),
            String::from(LEXICAL_ERROR)
        )
    }
}

// ' -> '...'
pub fn extract_single_quote_prefix_lexeme(begin_lexeme: &mut usize, 
    line_number: &mut usize, code: &Code, code_lines: &mut Vec<usize>, 
    line_start_index: &mut usize) -> (CoreToken, String) {
    let mut forward_lexeme = *begin_lexeme + 1;
    while forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '\'' => {
                *begin_lexeme = forward_lexeme + 1;
                return (CoreToken::LITERAL, String::from(LITERAL))
            },
            '\n' => {
                code_lines.push(*line_start_index);
                *line_number = *line_number + 1;
                *line_start_index = forward_lexeme + 1;
            },
            _ => {}
        }
        forward_lexeme = forward_lexeme + 1;
    }
    *begin_lexeme = forward_lexeme;
    let err_str = Rc::new(String::from(r#"no closing `'` found for literal"#));
    return (
        CoreToken::LEXICAL_ERROR((LexicalErrorKind::NO_CLOSING_SYMBOLS, err_str)),
        String::from(LEXICAL_ERROR)
    )
}

// " -> "..."
pub fn extract_double_quote_prefix_lexeme(begin_lexeme: &mut usize, 
    line_number: &mut usize, code: &Code, code_lines: &mut Vec<usize>,
    line_start_index: &mut usize) -> (CoreToken, String) {
    let mut forward_lexeme = *begin_lexeme + 1;
    while forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '"' => {
                *begin_lexeme = forward_lexeme + 1;
                return (CoreToken::LITERAL, String::from(LITERAL))
            },
            '\n' => {
                code_lines.push(*line_start_index);
                *line_number = *line_number + 1;
                *line_start_index = forward_lexeme + 1;
            },
            _ => {}
        }
        forward_lexeme = forward_lexeme + 1;
    }
    *begin_lexeme = forward_lexeme;
    let err_str = Rc::new(String::from(r#"no closing `"` found for literal"#));
    return (
        CoreToken::LEXICAL_ERROR((LexicalErrorKind::NO_CLOSING_SYMBOLS, err_str)),
        String::from(LEXICAL_ERROR)
    )
}

// letter -> letter((letter|digit|_)*) or keyword or type
pub fn extract_letter_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> (CoreToken, String) {
    let mut forward_lexeme = *begin_lexeme + 1;
    while forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        if context::is_letter(&next_char) || context::is_digit(&next_char) {
            // do nothing
        } else {
            let value: String = code.token_value(*begin_lexeme, Some(forward_lexeme));
            *begin_lexeme = forward_lexeme;
            return get_token_for_identifier(value);
        }
        forward_lexeme = forward_lexeme + 1;
    }
    let value: String = code.token_value(*begin_lexeme, Some(forward_lexeme));
    *begin_lexeme = forward_lexeme;
    return get_token_for_identifier(value);
}

// digit -> digit((digit)*(.digit(digit*)|empty))
pub fn extract_digit_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> (CoreToken, String) {
    let mut forward_lexeme = *begin_lexeme + 1;
    let mut state: usize = 0;
    while forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match state {
            0 => {
                if context::is_digit(&next_char) {
                    // do nothing
                } else if next_char == '.' {
                    state = 1;
                } else {
                    *begin_lexeme = forward_lexeme;
                    return (CoreToken::INTEGER, String::from(INTEGER))
                }
            },
            1 => {
                if context::is_digit(&next_char) {
                    state = 2;
                } else {
                    *begin_lexeme = forward_lexeme - 1;
                    return (CoreToken::INTEGER, String::from(INTEGER))
                }
            },
            2 => {
                if context::is_digit(&next_char) {
                    // do nothing
                } else {
                    *begin_lexeme = forward_lexeme;
                    return (CoreToken::FLOATING_POINT_NUMBER, String::from(FLOATING_POINT_NUMBER))
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
            *begin_lexeme = forward_lexeme;
            return (CoreToken::INTEGER, String::from(INTEGER))
        },
        1 => {
            *begin_lexeme = forward_lexeme - 1;
            return (CoreToken::INTEGER, String::from(INTEGER))
        },
        2 => {
            *begin_lexeme = forward_lexeme;
            return (CoreToken::FLOATING_POINT_NUMBER, String::from(FLOATING_POINT_NUMBER))
        },
        _ => unreachable!("any state other than 0, 1, and 2 is not reachable")
    }
}

// : -> :, ::
pub fn extract_colon_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> (CoreToken, String) {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            ':' => {
                *begin_lexeme = forward_lexeme + 1;
                return (CoreToken::DOUBLE_COLON, String::from("::"));
            },
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return (CoreToken::COLON, String::from(":"));
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return (CoreToken::COLON, String::from(":"));
    }
}