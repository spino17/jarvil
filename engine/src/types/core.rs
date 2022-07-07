use std::rc::Rc;
use std::str;
use std::fmt::Formatter;
use crate::constants::common::{INT, FLOAT, STRING, BOOL};
use crate::types::{r#struct::Struct, atomic::Atomic, lambda::Lambda};

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
    NON_TYPED,
}

#[derive(Debug)]
pub struct Type(pub Rc<CoreType>);

impl Type {
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
            CoreType::NON_TYPED => {
                match base_type.0.as_ref() {
                    CoreType::NON_TYPED => true,
                    _ => false,
                }
            }
        }
    }
}

// TODO - convert it into to_string type of method
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
            CoreType::NON_TYPED => write!(f, "non-typed"),
        }
    }
}