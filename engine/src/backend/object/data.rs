use std::{fmt::Display, ptr::NonNull};

use super::core::Object;

#[derive(Clone)]
pub enum Data {
    INT(i32),
    FLOAT(f64),
    OBJ(Object),
    BOOL(bool),
}

impl Data {
    pub fn eq_type(&self, data: &Data) -> bool {
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

    pub fn is_numeric(&self) -> bool {
        match self {
            Data::INT(_) | Data::FLOAT(_) => true,
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
        }
    }
}
