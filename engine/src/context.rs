use std::{collections::HashMap};
use crate::env::{SymbolData, MetaData};
use std::cell::RefCell;
use crate::constants::{keywords::KEYWORDS, types::TYPES};

thread_local! {
    static CONTEXT: RefCell<Context> = RefCell::new(Context::new())
}

struct Context {
    keywords: HashMap<String, SymbolData>,
    types: HashMap<String, SymbolData>,
}

impl Context {
    fn new() -> Self {
        let mut keywords = HashMap::new();
        let mut types = HashMap::new();
        for &keyword in KEYWORDS.iter() {
            keywords.insert(String::from(keyword), SymbolData::new_with_keyword());
        }
        for &data_type in TYPES.iter() {
            types.insert(String::from(data_type), SymbolData::new_with_type());
        }
        Context {
            keywords,
            types,
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