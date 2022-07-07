use std::rc::Rc;
use crate::constants::common::{INT, FLOAT, STRING, BOOL};
use crate::types::core::{AbstractType, Type, CoreType};

#[derive(Debug)]
pub enum Atomic {
    INT,
    FLOAT,
    STRING,
    BOOL,
}

impl Atomic {
    pub fn get_atomic_type(&self) -> &str {
        match self {
            Atomic::INT     =>  INT,
            Atomic::FLOAT   =>  FLOAT,
            Atomic::STRING  =>  STRING,
            Atomic::BOOL    =>  BOOL,
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Atomic::INT => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Atomic::FLOAT => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Atomic::STRING => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Atomic::BOOL => true,
            _ => false,
        }
    }
}

impl AbstractType for Atomic {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::ATOMIC(atomic_data) => {
                match atomic_data {
                    Atomic::INT     =>  self.is_int(),
                    Atomic::FLOAT   =>  self.is_float(),
                    Atomic::STRING  =>  self.is_string(),
                    Atomic::BOOL    =>  self.is_bool(),
                }
            },
            _ => false
        }
    }
    
    fn to_string(&self) -> Rc<String> {
        match self {
            Atomic::INT     =>  Rc::new(String::from("int")),
            Atomic::FLOAT   =>  Rc::new(String::from("float")),
            Atomic::STRING  =>  Rc::new(String::from("string")),
            Atomic::BOOL    =>  Rc::new(String::from("bool")),
        }
    }
}