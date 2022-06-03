use rustc_hash::FxHashMap;
use std::collections::HashMap;
use std::cell::RefCell;
use crate::constants::common::{KEYWORDS, TYPES, LETTERS, DIGITS};

thread_local! {
    static CONTEXT: RefCell<Context> = RefCell::new(Context::new())
}

struct Context {
    keywords: FxHashMap<String, ()>,
    types: FxHashMap<String, ()>,
    letters: FxHashMap<char, ()>,
    digits: FxHashMap<char, ()>,
    // TODO - add Env (symbol table) so that internal methods just access context value and Env is updated while entry and exit
    // of block
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