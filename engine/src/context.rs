use std::{collections::HashMap, thread::panicking};
use crate::env::{SymbolData, MetaData};
use std::cell::RefCell;
use crate::constants::keywords::KEYWORDS;

thread_local! {
    static CONTEXT: RefCell<Context> = RefCell::new(Context::new())
}

struct Context {
    keywords: HashMap<String, SymbolData>
}

impl Context {
    fn new() -> Self {
        let mut keywords = HashMap::new();
        for &keyword in KEYWORDS.iter() {
            keywords.insert(String::from(keyword), SymbolData::new_keyword());
        }
        Context {
            keywords,
        }
    }

    fn is_keyword(&self, name: &str) -> bool {
        match self.keywords.get(name) {
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