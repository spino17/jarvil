use std::rc::Rc;
use std::str;
use std::fmt::Formatter;
use crate::constants::common::{INT, FLOAT, STRING, BOOL};

pub trait TypeCheck {
    fn is_eq(&self, base_type: &Type) -> bool;
    // fn get_memory_width(&self) -> usize;
}

#[derive(Debug)]
pub enum CoreType {
    ATOMIC(Atomic),
    STRUCT(Struct),
    LAMBDA(Lambda),
    // ENUMERATION,
    // TUPLES,
    // ARRAY,
    // REFERENCE,
    // GENERIC(Generic)
    NONE
}

#[derive(Debug)]
pub struct Type(pub Rc<CoreType>);

impl Type {
    pub fn get_user_defined_type_name(&self) -> Option<Rc<String>> {
        match self.0.as_ref() {
            CoreType::STRUCT(struct_data) => Some(struct_data.name.clone()),
            CoreType::LAMBDA(lambda_data) => {
                match &lambda_data.name {
                    Some(name) => Some(name.clone()),
                    None => None,
                }
            },
            _ => None,
        }
    }

    pub fn is_atomic(&self, atomic_type_name: &str) -> bool {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic_type) => {
                match atomic_type_name {
                    INT       =>  atomic_type.is_int(),
                    FLOAT     =>  atomic_type.is_float(),
                    STRING    =>  atomic_type.is_string(),
                    BOOL      =>  atomic_type.is_bool(),
                    _               =>  unreachable!("atomic type name can only be 'int', 'float', 'string' or 'bool'"),
                }
            },
            _ => false
        }
    }

    pub fn get_atomic_type(atomic_type_name: &str) -> Self {
        match atomic_type_name {
            INT       =>  Type(Rc::new(CoreType::ATOMIC(Atomic::INT))),
            FLOAT     =>  Type(Rc::new(CoreType::ATOMIC(Atomic::FLOAT))),
            STRING    =>  Type(Rc::new(CoreType::ATOMIC(Atomic::STRING))),
            BOOL      =>  Type(Rc::new(CoreType::ATOMIC(Atomic::BOOL))),
            _               =>  unreachable!("atomic type name can only be 'int', 'float', 'string' or 'bool'"),
        }
    }
}

impl TypeCheck for Type {
    fn is_eq(&self, base_type: &Type) -> bool {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic_data) => {
                atomic_data.is_eq(base_type)
            },
            CoreType::STRUCT(struct_data) => {
                struct_data.is_eq(base_type)
            },
            CoreType::LAMBDA(lambda_data) => {
                lambda_data.is_eq(base_type)
            },
            CoreType::NONE => {
                match base_type.0.as_ref() {
                    CoreType::NONE => true,
                    _ => false,
                }
            }
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic_data) => write!(f, "{}", atomic_data.get_atomic_type()),
            CoreType::STRUCT(struct_data) => write!(f, "{}", struct_data.name),
            CoreType::LAMBDA(lambda_data) => {
                match &lambda_data.name {
                    Some(name) => write!(f, "{}", name),
                    None => write!(f, "lambda"),
                }
            },
            CoreType::NONE => write!(f, "None"),
        }
    }
}

#[derive(Debug)]
pub enum Atomic {
    INT,
    FLOAT,
    STRING,
    BOOL,
}

impl Atomic {
    fn get_atomic_type(&self) -> &str {
        match self {
            Atomic::INT     =>  INT,
            Atomic::FLOAT   =>  FLOAT,
            Atomic::STRING  =>  STRING,
            Atomic::BOOL    =>  BOOL,
        }
    }

    fn is_int(&self) -> bool {
        match self {
            Atomic::INT => true,
            _ => false,
        }
    }

    fn is_float(&self) -> bool {
        match self {
            Atomic::FLOAT => true,
            _ => false,
        }
    }

    fn is_string(&self) -> bool {
        match self {
            Atomic::STRING => true,
            _ => false,
        }
    }

    fn is_bool(&self) -> bool {
        match self {
            Atomic::BOOL => true,
            _ => false,
        }
    }
}

impl TypeCheck for Atomic {
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
}

#[derive(Debug)]
pub struct Struct {
    pub name: Rc<String>,
}

impl TypeCheck for Struct {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::STRUCT(struct_data) => {
                self.name.eq(&struct_data.name)
            },
            _ => false
        }
    }
}

#[derive(Debug)]
pub struct Lambda {
    pub name: Option<Rc<String>>,
}

impl TypeCheck for Lambda {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::LAMBDA(lambda_data) => {
                todo!()
            },
            _ => false
        }
    }
}