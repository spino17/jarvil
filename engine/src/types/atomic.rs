use crate::constants::common::{BOOL, FLOAT, INT, STRING};
use crate::types::core::{AbstractType, CoreType, Type};
use std::rc::Rc;

#[derive(Debug)]
pub enum Atomic {
    INT,
    FLOAT,
    STRING,
    BOOL,
}

impl Atomic {
    pub fn new_with_type_str(type_str: &str) -> Option<Type> {
        let type_obj = match type_str {
            INT => Type::new_with_atomic(Atomic::INT),
            FLOAT => Type::new_with_atomic(Atomic::FLOAT),
            STRING => Type::new_with_atomic(Atomic::STRING),
            BOOL => Type::new_with_atomic(Atomic::BOOL),
            _ => return None,
        };
        Some(type_obj)
    }

    pub fn get_atomic_type(&self) -> &str {
        match self {
            Atomic::INT => INT,
            Atomic::FLOAT => FLOAT,
            Atomic::STRING => STRING,
            Atomic::BOOL => BOOL,
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
            CoreType::ATOMIC(atomic_data) => match atomic_data {
                Atomic::INT => self.is_int(),
                Atomic::FLOAT => self.is_float(),
                Atomic::STRING => self.is_string(),
                Atomic::BOOL => self.is_bool(),
            },
            _ => false,
        }
    }

    fn string(&self) -> String {
        match self {
            Atomic::INT => String::from(INT),
            Atomic::FLOAT => String::from(FLOAT),
            Atomic::STRING => String::from(STRING),
            Atomic::BOOL => String::from(BOOL),
        }
    }
}
