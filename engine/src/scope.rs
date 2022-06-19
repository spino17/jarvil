// Symbol Table - HashMap for (token_name, meta data like datatypes etc.). Each scope bounded by {...} is represented by
// individual symbol table. Each symbol table points to the symbol table belonging to it's enclosing scope.
// Point of interactions:
// 1. Before lexical analysis phase to initialize the map with keywords.
// 2. During lexical analysis phase to distringuish between pattern matched for identifiers and keywords (if we found lexeme in
// symbol_table then it's a keyword so form token accordingly).
// 3. During parsing phase, we add entries in the table for identifiers when we find declartion statements in the current scope.
// 4. During parsing phase, when we encounter any statement involving identifiers then we access it for checking declartions, 
// datatypes etc.

use std::cell::RefCell;
use rustc_hash::FxHashMap;
use std::rc::Rc;
use crate::lexer::token::TokenValue;

#[derive(Debug)]
struct IdentifierData {
    data_type: Rc<String>,
    is_init: bool,
}

#[derive(Debug)]
struct UserDefinedTypeData {
    fields: Rc<FxHashMap<Rc<String>, Rc<String>>>,
}

#[derive(Debug)]
struct FunctionData {
    params: Rc<Vec<(Rc<String>, Rc<String>)>>,
    return_type: Option<Rc<String>>,
}

#[derive(Debug)]
enum MetaData {
    IDENTIFIER(IdentifierData),
    USER_DEFINED_TYPE(UserDefinedTypeData),
    FUNCTION(FunctionData),
}

#[derive(Debug)]
pub struct SymbolData(Rc<RefCell<MetaData>>);

impl SymbolData {
    // identifier specific methods
    pub fn is_id(&self) -> bool {
        match *self.0.borrow() {
            MetaData::IDENTIFIER(_) => true,
            _ => false,
        }
    }

    pub fn set_init(&self, is_init: bool) {
        // .is_init = is_init;
        match &mut *self.0.borrow_mut() {
            MetaData::IDENTIFIER(data) => data.is_init = is_init,
            _ => {
                // do nothing for other branches
            }
        }
    }

    pub fn is_init(&self) -> bool {
        // .is_init
        match &*self.0.borrow() {
            MetaData::IDENTIFIER(data) => data.is_init,
            _ => {
                true
            }
        }
    }

    pub fn get_type(&self) -> Rc<String> {
        // .data_type.clone()
        match &*self.0.borrow() {
            MetaData::IDENTIFIER(data) => data.data_type.clone(),
            _ => {
                Rc::new(String::from("non-typed"))
            }
        }
    }

    pub fn type_eq(&self, data_type: &str) -> bool {
        // .data_type.as_ref().eq(data_type)
        match &*self.0.borrow() {
            MetaData::IDENTIFIER(data) => data.data_type.to_string().eq(data_type),
            _ => false
        }
    }

    pub fn get_id_data(&self) -> (Rc<String>, bool) {
        match &*self.0.borrow() {
            MetaData::IDENTIFIER(data) => {
                (data.data_type.clone(), data.is_init)
            },
            _ => {
                unreachable!("use this method only for purely identifier tokens")
            }
        }
    }

    // type specific methods
    pub fn is_type(&self) -> bool {
        match *self.0.borrow() {
            MetaData::USER_DEFINED_TYPE(_) => true,
            _ => false,
        }
    }

    pub fn get_user_defined_type_data(&self) -> Rc<FxHashMap<Rc<String>, Rc<String>>> {
        match &*self.0.borrow() {
            MetaData::USER_DEFINED_TYPE(data) => {
                data.fields.clone()
            },
            _ => {
                unreachable!("use this method only for purely identifier tokens")
            }
        }
    }

    // function specific methods
    pub fn is_function(&self) -> bool {
        match *self.0.borrow() {
            MetaData::FUNCTION(_) => true,
            _ => false,
        }
    }

    pub fn get_function_data(&self) -> (Rc<Vec<(Rc<String>, Rc<String>)>>, Option<Rc<String>>) {
        match &*self.0.borrow() {
            MetaData::FUNCTION(data) => {
                (data.params.clone(), data.return_type.clone())
            },
            _ => {
                unreachable!("use this method only for purely identifier tokens")
            }
        }
    }

    // general methods
    pub fn get_type_of_identifier(&self) -> &str {
        match &*self.0.borrow() {
            MetaData::IDENTIFIER(_) => "identifier",
            MetaData::USER_DEFINED_TYPE(_) => "user-defined type",
            MetaData::FUNCTION(_) => "function",
        }
    }
}

#[derive(Debug)]
pub struct Scope {
    symbol_table: FxHashMap<Rc<String>, SymbolData>,
    parent_env: Option<Env>,
    return_type: Option<Rc<String>>,  // for functional scope - match return type in nested sub blocks checking this global field
}

impl Scope {
    fn set(&mut self, name: Rc<String>, meta_data: MetaData) {
        self.symbol_table.insert(name, SymbolData(Rc::new(RefCell::new(meta_data))));
    }

    fn get(&self, name: &Rc<String>) -> Option<&SymbolData> {
        self.symbol_table.get(name)
    }

    fn set_init(&self, name: &Rc<String>) {
        if let Some(symbol_data) = self.get(name) {
            symbol_data.set_init(true);
        }
    }
}

#[derive(Debug)]
pub struct Env(pub Rc<RefCell<Scope>>);

impl Env {
    pub fn new() -> Self {
        Env(Rc::new(RefCell::new(Scope {
            symbol_table: FxHashMap::default(),
            parent_env: None,
            return_type: None,
        })))
    }

    pub fn new_with_parent_env(parent_env: &Env) -> Self {
        let env = parent_env.0.clone();
        Env(Rc::new(RefCell::new(Scope {
            symbol_table: FxHashMap::default(),
            parent_env: Some(Env(env)),
            return_type: None,
        })))
    }

    pub fn set_identifier(&self, token_value: &TokenValue, data_type: &Rc<String>, is_init: bool) {
        let meta_data = MetaData::IDENTIFIER(IdentifierData{
            data_type: data_type.clone(),
            is_init,
        });
        self.0.borrow_mut().set(token_value.0.clone(), meta_data);
    }

    pub fn set_identifier_init(&self, token_value: &TokenValue) {
        self.0.borrow_mut().set_init(&token_value.0.clone());
    }

    pub fn set_user_defined_type(&self, token_value: &TokenValue, fields: &Rc<FxHashMap<Rc<String>, Rc<String>>>) {
        let meta_data = MetaData::USER_DEFINED_TYPE(UserDefinedTypeData{
            fields: fields.clone(),
        });
        self.0.borrow_mut().set(token_value.0.clone(), meta_data);
    }

    pub fn set_function(&self, token_value: &TokenValue, 
        params: &Rc<Vec<(Rc<String>, Rc<String>)>>, return_type: Option<Rc<String>>) {
        let meta_data = MetaData::FUNCTION(FunctionData{
            params: params.clone(),
            return_type: return_type,
        });
        self.0.borrow_mut().set(token_value.0.clone(), meta_data);
    }

    pub fn get(&self, token_value: &TokenValue) -> Option<SymbolData> {
        let scope_ref = self.0.borrow();

        // check the identifier name in current scope
        match scope_ref.get(&token_value.0) {
            Some(value) => {
                Some(SymbolData(value.0.clone()))
            },
            None => {

                // if not found in current scope, check recursively in parent scopes
                if let Some(parent_env) = &scope_ref.parent_env {

                    // return from the nearest scope which found the identifier name
                    parent_env.get(token_value)
                } else {
                    None
                }
            }
        }
    }
}