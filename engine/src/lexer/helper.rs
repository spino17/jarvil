use super::token::LexicalErrorKind;
use crate::{code::Code, lexer::token::CoreToken};

pub fn is_letter(c: &char) -> bool {
    if c.is_ascii_alphabetic() || (*c == '_') {
        true
    } else {
        false
    }
}

// ' ' -> '...'
pub fn extract_blank_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> CoreToken {
    let mut forward_lexeme = *begin_lexeme + 1;
    while forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        if next_char != ' ' {
            *begin_lexeme = forward_lexeme;
            return CoreToken::BLANK;
        }
        forward_lexeme = forward_lexeme + 1;
    }
    *begin_lexeme = forward_lexeme;
    return CoreToken::BLANK;
}

// - -> -, ->
pub fn extract_dash_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> CoreToken {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '>' => {
                *begin_lexeme = forward_lexeme + 1;
                return CoreToken::RIGHT_ARROW;
            }
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return CoreToken::DASH;
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return CoreToken::DASH;
    }
}

// * -> *, **
pub fn extract_star_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> CoreToken {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '*' => {
                *begin_lexeme = forward_lexeme + 1;
                return CoreToken::DOUBLE_STAR;
            }
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
pub fn extract_slash_prefix_lexeme(
    begin_lexeme: &mut usize,
    line_number: &mut usize,
    code: &Code,
    code_lines: &mut Vec<usize>,
    line_start_index: &mut usize,
) -> CoreToken {
    let mut forward_lexeme = *begin_lexeme + 1;
    let mut state: usize = 0;
    while forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match state {
            0 => match next_char {
                '/' => {
                    state = 1;
                }
                '*' => {
                    state = 2;
                }
                _ => {
                    *begin_lexeme = *begin_lexeme + 1;
                    return CoreToken::SLASH;
                }
            },
            1 => match next_char {
                '\n' => {
                    *begin_lexeme = forward_lexeme;
                    return CoreToken::SINGLE_LINE_COMMENT;
                }
                _ => {}
            },
            2 => match next_char {
                '*' => {
                    state = 3;
                }
                '\n' => {
                    code_lines.push(*line_start_index);
                    *line_number = *line_number + 1;
                    *line_start_index = forward_lexeme + 1;
                }
                _ => {}
            },
            3 => match next_char {
                '/' => {
                    *begin_lexeme = forward_lexeme + 1;
                    return CoreToken::BLOCK_COMMENT;
                }
                _ => {
                    state = 2;
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
            *begin_lexeme = *begin_lexeme + 1;
            return CoreToken::SLASH;
        }
        1 => {
            *begin_lexeme = forward_lexeme;
            return CoreToken::SINGLE_LINE_COMMENT;
        }
        2 => {
            *begin_lexeme = forward_lexeme;
            return CoreToken::LEXICAL_ERROR(LexicalErrorKind::NO_CLOSING_SYMBOLS("*/"));
        }
        3 => {
            *begin_lexeme = forward_lexeme;
            return CoreToken::LEXICAL_ERROR(LexicalErrorKind::NO_CLOSING_SYMBOLS("*/"));
        }
        _ => unreachable!("any state other than 0, 1, 2 and 3 is not reachable"),
    }
}

// # -> #...\n
pub fn extract_hash_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> CoreToken {
    let mut forward_lexeme = *begin_lexeme + 1;
    while forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '\n' => {
                *begin_lexeme = forward_lexeme;
                return CoreToken::SINGLE_LINE_COMMENT;
            }
            _ => {}
        }
        forward_lexeme = forward_lexeme + 1;
    }
    *begin_lexeme = forward_lexeme;
    return CoreToken::SINGLE_LINE_COMMENT;
}

// = -> =, ==
pub fn extract_equal_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> CoreToken {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return CoreToken::DOUBLE_EQUAL;
            }
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return CoreToken::EQUAL;
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return CoreToken::EQUAL;
    }
}

// > -> >, >=
pub fn extract_rbracket_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> CoreToken {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return CoreToken::GREATER_EQUAL;
            }
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return CoreToken::RBRACKET;
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return CoreToken::RBRACKET;
    }
}

// < -> <, <=
pub fn extract_lbracket_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> CoreToken {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return CoreToken::LESS_EQUAL;
            }
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return CoreToken::LBRACKET;
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return CoreToken::LBRACKET;
    }
}

// ! -> !=
pub fn extract_exclaimation_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> CoreToken {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '=' => {
                *begin_lexeme = forward_lexeme + 1;
                return CoreToken::NOT_EQUAL;
            }
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                CoreToken::LEXICAL_ERROR(LexicalErrorKind::INVALID_CHAR)
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        CoreToken::LEXICAL_ERROR(LexicalErrorKind::INVALID_CHAR)
    }
}

// ' -> '...'
pub fn extract_single_quote_prefix_lexeme(
    begin_lexeme: &mut usize,
    line_number: &mut usize,
    code: &Code,
    code_lines: &mut Vec<usize>,
    line_start_index: &mut usize,
) -> CoreToken {
    let mut forward_lexeme = *begin_lexeme + 1;
    while forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '\'' => {
                *begin_lexeme = forward_lexeme + 1;
                return CoreToken::LITERAL;
            }
            '\n' => {
                code_lines.push(*line_start_index);
                *line_number = *line_number + 1;
                *line_start_index = forward_lexeme + 1;
            }
            _ => {}
        }
        forward_lexeme = forward_lexeme + 1;
    }
    *begin_lexeme = forward_lexeme;
    return CoreToken::LEXICAL_ERROR(LexicalErrorKind::NO_CLOSING_SYMBOLS("'"));
}

// " -> "..."
pub fn extract_double_quote_prefix_lexeme(
    begin_lexeme: &mut usize,
    line_number: &mut usize,
    code: &Code,
    code_lines: &mut Vec<usize>,
    line_start_index: &mut usize,
) -> CoreToken {
    let mut forward_lexeme = *begin_lexeme + 1;
    while forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            '"' => {
                *begin_lexeme = forward_lexeme + 1;
                return CoreToken::LITERAL;
            }
            '\n' => {
                code_lines.push(*line_start_index);
                *line_number = *line_number + 1;
                *line_start_index = forward_lexeme + 1;
            }
            _ => {}
        }
        forward_lexeme = forward_lexeme + 1;
    }
    *begin_lexeme = forward_lexeme;
    return CoreToken::LEXICAL_ERROR(LexicalErrorKind::NO_CLOSING_SYMBOLS(r#"""#));
}

// letter -> letter((letter|digit|_)*) or keyword or type
pub fn extract_letter_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> CoreToken {
    let mut forward_lexeme = *begin_lexeme + 1;
    while forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        if is_letter(&next_char) || next_char.is_digit(10) {
            // do nothing
        } else {
            let value_iter = code.token_value_as_iter(*begin_lexeme, Some(forward_lexeme));
            *begin_lexeme = forward_lexeme;
            return token_for_identifier(value_iter);
        }
        forward_lexeme = forward_lexeme + 1;
    }
    let value_iter = code.token_value_as_iter(*begin_lexeme, Some(forward_lexeme));
    *begin_lexeme = forward_lexeme;
    return token_for_identifier(value_iter);
}

// digit -> digit((digit)*(.digit(digit*)|empty))
pub fn extract_digit_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> CoreToken {
    let mut forward_lexeme = *begin_lexeme + 1;
    let mut state: usize = 0;
    while forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match state {
            0 => {
                if next_char.is_digit(10) {
                    // do nothing
                } else if next_char == '.' {
                    state = 1;
                } else {
                    *begin_lexeme = forward_lexeme;
                    return CoreToken::INTEGER;
                }
            }
            1 => {
                if next_char.is_digit(10) {
                    state = 2;
                } else {
                    *begin_lexeme = forward_lexeme - 1;
                    return CoreToken::INTEGER;
                }
            }
            2 => {
                if next_char.is_digit(10) {
                    // do nothing
                } else {
                    *begin_lexeme = forward_lexeme;
                    return CoreToken::FLOATING_POINT_NUMBER;
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
            *begin_lexeme = forward_lexeme;
            return CoreToken::INTEGER;
        }
        1 => {
            *begin_lexeme = forward_lexeme - 1;
            return CoreToken::INTEGER;
        }
        2 => {
            *begin_lexeme = forward_lexeme;
            return CoreToken::FLOATING_POINT_NUMBER;
        }
        _ => unreachable!("any state other than 0, 1, and 2 is not reachable"),
    }
}

// : -> :, ::
pub fn extract_colon_prefix_lexeme(begin_lexeme: &mut usize, code: &Code) -> CoreToken {
    let forward_lexeme = *begin_lexeme + 1;
    if forward_lexeme < code.len() {
        let next_char = code.get_char(forward_lexeme);
        match next_char {
            ':' => {
                *begin_lexeme = forward_lexeme + 1;
                return CoreToken::DOUBLE_COLON;
            }
            _ => {
                *begin_lexeme = *begin_lexeme + 1;
                return CoreToken::COLON;
            }
        }
    } else {
        *begin_lexeme = *begin_lexeme + 1;
        return CoreToken::COLON;
    }
}

// This method is taken from the amazing book `Crafting Interpreters` by `Bob Nystrom`
fn check_keyword(
    remaining_str: &str,
    value: std::slice::Iter<char>,
    token_type: CoreToken,
) -> CoreToken {
    let value: String = value.collect();
    if value.len() == remaining_str.len() && value.eq(remaining_str) {
        token_type
    } else {
        CoreToken::IDENTIFIER
    }
}

// Trie based implementation for efficient reserved words matching
pub fn token_for_identifier(mut value_iter: std::slice::Iter<char>) -> CoreToken {
    match value_iter.next() {
        Some(c) => {
            match c {
                'f' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => match next_c {
                            'o' => check_keyword("r", value_iter, CoreToken::FOR),
                            'l' => check_keyword("oat", value_iter, CoreToken::ATOMIC_TYPE),
                            'i' => check_keyword("nally", value_iter, CoreToken::FINALLY_KEYWORD),
                            'r' => check_keyword("om", value_iter, CoreToken::FROM_KEYWORD),
                            _ => return CoreToken::IDENTIFIER,
                        },
                        None => return CoreToken::IDENTIFIER,
                    }
                } // for, float, finally, from
                'w' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => match next_c {
                            'h' => check_keyword("ile", value_iter, CoreToken::WHILE),
                            'i' => check_keyword("th", value_iter, CoreToken::WITH_KEYWORD),
                            _ => return CoreToken::IDENTIFIER,
                        },
                        None => return CoreToken::IDENTIFIER,
                    }
                } // with, while
                'c' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => match next_c {
                            'o' => check_keyword("ntinue", value_iter, CoreToken::CONTINUE),
                            'l' => check_keyword("ass", value_iter, CoreToken::CLASS_KEYWORD),
                            _ => return CoreToken::IDENTIFIER,
                        },
                        None => return CoreToken::IDENTIFIER,
                    }
                } // continue, class
                'b' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => match next_c {
                            'r' => check_keyword("eak", value_iter, CoreToken::BREAK),
                            'o' => check_keyword("ol", value_iter, CoreToken::ATOMIC_TYPE),
                            _ => return CoreToken::IDENTIFIER,
                        },
                        None => return CoreToken::IDENTIFIER,
                    }
                } // break, bool
                'i' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => match next_c {
                            's' => check_keyword("", value_iter, CoreToken::IS),
                            'f' => check_keyword("", value_iter, CoreToken::IF),
                            'n' => {
                                let next_next_c = value_iter.next();
                                match next_next_c {
                                    Some(next_next_c) => match next_next_c {
                                        't' => {
                                            let next_next_next_c = value_iter.next();
                                            match next_next_next_c {
                                                Some(next_next_next_c) => match next_next_next_c {
                                                    'e' => check_keyword(
                                                        "rface",
                                                        value_iter,
                                                        CoreToken::INTERFACE_KEYWORD,
                                                    ),
                                                    _ => return CoreToken::IDENTIFIER,
                                                },
                                                None => return CoreToken::ATOMIC_TYPE,
                                            }
                                        }
                                        _ => return CoreToken::IDENTIFIER,
                                    },
                                    None => return CoreToken::IN,
                                }
                            }
                            'm' => {
                                let next_next_c = value_iter.next();
                                match next_next_c {
                                    Some(next_next_c) => match next_next_c {
                                        'p' => {
                                            let next_next_next_c = value_iter.next();
                                            match next_next_next_c {
                                                Some(next_next_next_c) => match next_next_next_c {
                                                    'l' => check_keyword(
                                                        "",
                                                        value_iter,
                                                        CoreToken::IMPL,
                                                    ),
                                                    'o' => check_keyword(
                                                        "rt",
                                                        value_iter,
                                                        CoreToken::IMPORT_KEYWORD,
                                                    ),
                                                    _ => return CoreToken::IDENTIFIER,
                                                },
                                                None => return CoreToken::IDENTIFIER,
                                            }
                                        }
                                        _ => return CoreToken::IDENTIFIER,
                                    },
                                    None => return CoreToken::IDENTIFIER,
                                }
                            }
                            _ => return CoreToken::IDENTIFIER,
                        },
                        None => return CoreToken::IDENTIFIER,
                    }
                } // if, interface, in, impl, int, import, is
                'e' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => match next_c {
                            'l' => {
                                let next_next_c = value_iter.next();
                                match next_next_c {
                                    Some(next_next_c) => match next_next_c {
                                        's' => check_keyword("e", value_iter, CoreToken::ELSE),
                                        'i' => check_keyword("f", value_iter, CoreToken::ELIF),
                                        _ => return CoreToken::IDENTIFIER,
                                    },
                                    None => return CoreToken::IDENTIFIER,
                                }
                            }
                            'x' => check_keyword("cept", value_iter, CoreToken::EXCEPT_KEYWORD),
                            _ => return CoreToken::IDENTIFIER,
                        },
                        None => return CoreToken::IDENTIFIER,
                    }
                } // else, elif, except
                't' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => match next_c {
                            'y' => check_keyword("pe", value_iter, CoreToken::TYPE_KEYWORD),
                            'r' => check_keyword("y", value_iter, CoreToken::TRY_KEYWORD),
                            _ => return CoreToken::IDENTIFIER,
                        },
                        None => return CoreToken::IDENTIFIER,
                    }
                } // type, try
                'd' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => match next_c {
                            'e' => {
                                let next_next_c = value_iter.next();
                                match next_next_c {
                                    Some(next_next_c) => match next_next_c {
                                        'f' => check_keyword("", value_iter, CoreToken::DEF),
                                        'l' => {
                                            check_keyword("", value_iter, CoreToken::DEL_KEYWORD)
                                        }
                                        _ => return CoreToken::IDENTIFIER,
                                    },
                                    None => return CoreToken::IDENTIFIER,
                                }
                            }
                            _ => return CoreToken::IDENTIFIER,
                        },
                        None => return CoreToken::IDENTIFIER,
                    }
                } // del, def
                'l' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => match next_c {
                            'e' => check_keyword("t", value_iter, CoreToken::LET),
                            'a' => check_keyword("mbda", value_iter, CoreToken::LAMBDA_KEYWORD),
                            _ => return CoreToken::IDENTIFIER,
                        },
                        None => return CoreToken::IDENTIFIER,
                    }
                } // let, lambda
                's' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => match next_c {
                            'e' => check_keyword("lf", value_iter, CoreToken::SELF),
                            't' => {
                                let next_next_c = value_iter.next();
                                match next_next_c {
                                    Some(next_next_c) => match next_next_c {
                                        'r' => {
                                            let next_next_next_c = value_iter.next();
                                            match next_next_next_c {
                                                Some(next_next_next_c) => match next_next_next_c {
                                                    'u' => check_keyword(
                                                        "ct",
                                                        value_iter,
                                                        CoreToken::STRUCT_KEYWORD,
                                                    ),
                                                    _ => CoreToken::IDENTIFIER,
                                                },
                                                None => return CoreToken::ATOMIC_TYPE,
                                            }
                                        }
                                        _ => return CoreToken::IDENTIFIER,
                                    },
                                    None => return CoreToken::IDENTIFIER,
                                }
                            }
                            _ => return CoreToken::IDENTIFIER,
                        },
                        None => return CoreToken::IDENTIFIER,
                    }
                } // self, str, struct
                'a' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => match next_c {
                            'n' => check_keyword("d", value_iter, CoreToken::AND),
                            's' => {
                                let next_next_c = value_iter.next();
                                match next_next_c {
                                    Some(next_next_c) => match next_next_c {
                                        's' => check_keyword(
                                            "ert",
                                            value_iter,
                                            CoreToken::ASSERT_KEYWORD,
                                        ),
                                        'y' => check_keyword(
                                            "nc",
                                            value_iter,
                                            CoreToken::ASYNC_KEYWORD,
                                        ),
                                        _ => return CoreToken::IDENTIFIER,
                                    },
                                    None => return CoreToken::AS,
                                }
                            }
                            'w' => check_keyword("ait", value_iter, CoreToken::AWAIT_KEYWORD),
                            _ => return CoreToken::IDENTIFIER,
                        },
                        None => return CoreToken::IDENTIFIER,
                    }
                } // and, as, assert, async, await
                'n' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => match next_c {
                            'o' => {
                                let next_next_c = value_iter.next();
                                match next_next_c {
                                    Some(next_next_c) => match next_next_c {
                                        't' => check_keyword("", value_iter, CoreToken::NOT),
                                        'n' => check_keyword(
                                            "local",
                                            value_iter,
                                            CoreToken::NONLOCAL_KEYWORD,
                                        ),
                                        _ => return CoreToken::IDENTIFIER,
                                    },
                                    None => return CoreToken::IDENTIFIER,
                                }
                            }
                            _ => return CoreToken::IDENTIFIER,
                        },
                        None => return CoreToken::IDENTIFIER,
                    }
                } // not, nonlocal
                'o' => check_keyword("r", value_iter, CoreToken::OR), // or
                'T' => check_keyword("rue", value_iter, CoreToken::TRUE), // True
                'F' => check_keyword("alse", value_iter, CoreToken::FALSE), // False
                'r' => {
                    let next_c = value_iter.next();
                    match next_c {
                        Some(next_c) => match next_c {
                            'e' => check_keyword("turn", value_iter, CoreToken::RETURN),
                            'a' => check_keyword("ise", value_iter, CoreToken::RAISE_KEYWORD),
                            _ => return CoreToken::IDENTIFIER,
                        },
                        None => return CoreToken::IDENTIFIER,
                    }
                } // raise, return
                'N' => check_keyword("one", value_iter, CoreToken::NONE), // None
                'g' => check_keyword("lobal", value_iter, CoreToken::GLOBAL_KEYWORD), // global
                'p' => check_keyword("ass", value_iter, CoreToken::PASS_KEYWORD), // pass
                'y' => check_keyword("ield", value_iter, CoreToken::YIELD_KEYWORD), // yield
                '_' => check_keyword("_peg_parser__", value_iter, CoreToken::PEG_PARSER), // __peg_parser__
                _ => CoreToken::IDENTIFIER,
            }
        }
        None => unreachable!("identifer value should have alteast one character"),
    }
}
