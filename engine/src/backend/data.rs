use crate::{backend::object::core::Object, error::constants::CASTING_DATA_ERROR_MSG};
use core::panic;
use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum Data {
    INT(i32),
    FLOAT(f64),
    OBJ(Object),
    BOOL(bool),
    NIL,
}

impl Data {
    // NOTE: Below casting functions panics instead of safe returning Option<...> to have runtime performance.
    // If there is a panic that means there is a bug in type-checker!
    pub fn as_int(&self) -> i32 {
        match self {
            Data::INT(val) => *val,
            _ => panic!("{}", CASTING_DATA_ERROR_MSG),
        }
    }

    pub fn as_float(&self) -> f64 {
        match self {
            Data::FLOAT(val) => *val,
            _ => panic!("{}", CASTING_DATA_ERROR_MSG),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Data::BOOL(val) => *val,
            _ => panic!("{}", CASTING_DATA_ERROR_MSG),
        }
    }

    pub fn as_object(&self) -> Object {
        match self {
            Data::OBJ(obj) => obj.clone(),
            _ => panic!("{}", CASTING_DATA_ERROR_MSG),
        }
    }

    pub fn has_eq_type(&self, data: &Data) -> bool {
        match self {
            Data::INT(_) => match data {
                Data::INT(_) => return true,
                _ => return false,
            },
            Data::FLOAT(_) => match data {
                Data::FLOAT(_) => return true,
                _ => return false,
            },
            Data::BOOL(_) => match data {
                Data::BOOL(_) => return true,
                _ => return false,
            },
            Data::OBJ(obj_1) => match data {
                Data::OBJ(obj_2) => obj_1.eq_type(obj_2),
                _ => return false,
            },
            Data::NIL => match data {
                Data::NIL => return true,
                _ => return false,
            },
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Data::INT(_) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Data::FLOAT(_) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Data::BOOL(_) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Data::OBJ(obj) => obj.is_string(),
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            Data::OBJ(obj) => obj.is_list(),
            _ => false,
        }
    }

    pub fn is_numeric(&self) -> bool {
        match self {
            Data::INT(_) | Data::FLOAT(_) => true,
            _ => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Data::NIL => true,
            _ => false,
        }
    }
}

impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Data::INT(val) => write!(f, "{}", val),
            Data::FLOAT(val) => write!(f, "{}", val),
            Data::OBJ(val) => write!(f, "{}", val),
            Data::BOOL(val) => write!(f, "{}", val),
            Data::NIL => write!(f, "<NIL>"),
        }
    }
}
