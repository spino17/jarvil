use rustc_hash::FxHashMap;
use std::{collections::HashMap, f32::consts::E};
use std::cell::RefCell;
use crate::{constants::common::{KEYWORDS, TYPES, LETTERS, DIGITS}, errors::ParseError};

thread_local! {
    static CONTEXT: RefCell<Context> = RefCell::new(Context::new())
}

// TODO - indent_spaces and max_error_lines take default values while initialization of thread local but should provide
// overriding mechanism to set custom values through command line or IDE settings.

struct Context {
    keywords: FxHashMap<String, ()>,
    types: FxHashMap<String, ()>,
    letters: FxHashMap<char, ()>,
    digits: FxHashMap<char, ()>,
    parse_errors: Vec<ParseError>,
    indent_spaces: i64,
    max_error_lines: usize,
}

impl Context {
    fn new() -> Self {
        let mut keywords: FxHashMap<String, ()> = HashMap::default();
        let mut types: FxHashMap<String, ()> = HashMap::default();
        let mut letters: FxHashMap<char, ()> = HashMap::default();
        let mut digits: FxHashMap<char, ()> = HashMap::default();

        for &keyword in KEYWORDS.iter() {
            keywords.insert(String::from(keyword), ());
        }
        for &data_type in TYPES.iter() {
            types.insert(String::from(data_type), ());
        }
        for &letter in LETTERS.iter() {
            letters.insert(letter, ());
        }
        for &digit in DIGITS.iter() {
            digits.insert(digit, ());
        }

        Context {
            keywords,
            types,
            letters,
            digits,
            parse_errors: vec![],
            indent_spaces: 4,       // default indentation is 4 spaces
            max_error_lines: 10,    // default max lines shown in error messages
        }
    }

    fn is_keyword(&self, name: &str) -> bool {
        match self.keywords.get(name) {
            Some(_) => true,
            None => false
        }
    }

    fn is_type(&self, name: &str) -> bool {
        match self.types.get(name) {
            Some(_) => true,
            None => false
        }
    }

    fn is_letter(&self, c: &char) -> bool {
        match self.letters.get(c) {
            Some(_) => true,
            None => false
        }
    }

    fn is_digit(&self, c: &char) -> bool {
        match self.digits.get(c) {
            Some(_) => true,
            None => false
        }
    }

    fn set_errors(&mut self, err: Vec<ParseError>) {
        self.parse_errors = err;
    }

    pub fn first_error(&self) -> Option<ParseError> {
        let errors_len = self.parse_errors.len();
        if errors_len == 0 {
            None
        } else {
            Some(self.parse_errors[0].clone())
        }
    }

    fn errors_len(&self) -> usize {
        self.parse_errors.len()
    }

    fn set_indent(&mut self, indent_spaces: i64) {
        self.indent_spaces = indent_spaces;
    }

    fn indent_spaces(&self) -> i64 {
        self.indent_spaces
    }

    fn set_max_error_lines(&mut self, max_error_lines: usize) {
        self.max_error_lines = max_error_lines;
    }

    fn max_error_lines(&self) -> usize {
        self.max_error_lines
    }
}

pub fn is_keyword(name: &str) -> bool {
    match CONTEXT.try_with(|ctx| {
        ctx.borrow().is_keyword(name)
    }) {
        Ok(value) => value,
        Err(err) => {
            panic!("{}", err)
        }
    }
}

pub fn is_type(name: &str) -> bool {
    match CONTEXT.try_with(|ctx| {
        ctx.borrow().is_type(name)
    }) {
        Ok(value) => value,
        Err(err) => {
            panic!("{}", err)
        }
    }
}

pub fn is_letter(c: &char) -> bool {
    match CONTEXT.try_with(|ctx| {
        ctx.borrow().is_letter(c)
    }) {
        Ok(value) => value,
        Err(err) => {
            panic!("{}", err)
        }
    }
}

pub fn is_digit(c: &char) -> bool {
    match CONTEXT.try_with(|ctx| {
        ctx.borrow().is_digit(c)
    }) {
        Ok(value) => value,
        Err(err) => {
            panic!("{}", err)
        }
    }
}

pub fn set_errors(err: Vec<ParseError>) {
    match CONTEXT.try_with(|ctx| {
        ctx.borrow_mut().set_errors(err)
    }) {
        Err(err) => {
            panic!("{}", err)
        }
        _ => {}
    }
}

pub fn first_error() -> Option<ParseError> {
    match CONTEXT.try_with(|ctx| {
        ctx.borrow().first_error()
    }) {
        Ok(val) => val,
        Err(err) => {
            panic!("{}", err)
        }
    }
}

pub fn errors_len() -> usize {
    match CONTEXT.try_with(|ctx| {
        ctx.borrow_mut().errors_len()
    }) {
        Ok(val) => val,
        Err(err) => {
            panic!("{}", err)
        }
    }
}

pub fn set_indent(indent_spaces: i64) {
    match CONTEXT.try_with(|ctx| {
        ctx.borrow_mut().set_indent(indent_spaces)
    }) {
        Err(err) => {
            panic!("{}", err)
        }
        _ => {}
    }
}

pub fn indent_spaces() -> i64 {
    match CONTEXT.try_with(|ctx| {
        ctx.borrow().indent_spaces()
    }) {
        Ok(val) => val,
        Err(err) => {
            panic!("{}", err)
        }
    }
}

pub fn set_max_error_lines(max_error_lines: usize) {
    match CONTEXT.try_with(|ctx| {
        ctx.borrow_mut().set_max_error_lines(max_error_lines)
    }) {
        Err(err) => {
            panic!("{}", err)
        }
        _ => {}
    }
}

pub fn max_error_lines() -> usize {
    match CONTEXT.try_with(|ctx| {
        ctx.borrow().max_error_lines()
    }) {
        Ok(val) => val,
        Err(err) => {
            panic!("{}", err)
        }
    }
}