use crate::constants::common::{BOOL, FLOAT, INT, NON_TYPED, STRING};
use crate::types::{array::Array, atomic::Atomic};
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use std::str;

pub trait AbstractType {
    fn is_eq(&self, base_type: &Type) -> bool;
    fn string(&self) -> Rc<String>;
    // fn get_memory_width(&self) -> usize;
}

#[derive(Debug)]
pub enum CoreType {
    ATOMIC(Atomic),
    // STRUCT(Struct),
    // LAMBDA(Lambda),
    USER_DEFINED(Rc<String>),
    ARRAY(Array),
    // ENUMERATION,
    // TUPLES,
    // ARRAY,
    // REFERENCE,
    // GENERIC(Generic)
    NON_TYPED,
}

#[derive(Debug, Clone)]
pub struct Type(pub Rc<CoreType>);

impl Type {
    pub fn new_with_atomic(atomic_type: Atomic) -> Type {
        Type(Rc::new(CoreType::ATOMIC(atomic_type)))
    }

    pub fn new_with_array(array_type: Array) -> Type {
        Type(Rc::new(CoreType::ARRAY(array_type)))
    }

    pub fn new_with_user_defined(user_defined_type_str: String) -> Type {
        Type(Rc::new(CoreType::USER_DEFINED(Rc::new(
            user_defined_type_str,
        ))))
    }

    pub fn is_atomic(&self, atomic_type_name: &str) -> bool {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic_type) => match atomic_type_name {
                INT => atomic_type.is_int(),
                FLOAT => atomic_type.is_float(),
                STRING => atomic_type.is_string(),
                BOOL => atomic_type.is_bool(),
                _ => unreachable!(
                    "atomic type name can only be `{}`, `{}`, `{}` or `{}`",
                    INT, FLOAT, STRING, BOOL
                ),
            },
            _ => false,
        }
    }

    pub fn get_atomic_type(atomic_type_name: &str) -> Self {
        match atomic_type_name {
            INT => Type(Rc::new(CoreType::ATOMIC(Atomic::INT))),
            FLOAT => Type(Rc::new(CoreType::ATOMIC(Atomic::FLOAT))),
            STRING => Type(Rc::new(CoreType::ATOMIC(Atomic::STRING))),
            BOOL => Type(Rc::new(CoreType::ATOMIC(Atomic::BOOL))),
            _ => unreachable!(
                "atomic type name can only be `{}`, `{}`, `{}` or `{}`",
                INT, FLOAT, STRING, BOOL
            ),
        }
    }
}

impl AbstractType for Type {
    fn is_eq(&self, base_type: &Type) -> bool {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic_data) => atomic_data.is_eq(base_type),
            CoreType::USER_DEFINED(user_defined_data) => match base_type.0.as_ref() {
                CoreType::USER_DEFINED(base_type_data) => user_defined_data.eq(base_type_data),
                _ => false,
            },
            CoreType::ARRAY(array_data) => array_data.is_eq(base_type),
            CoreType::NON_TYPED => match base_type.0.as_ref() {
                CoreType::NON_TYPED => true,
                _ => false,
            },
        }
    }

    fn string(&self) -> Rc<String> {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic_data) => atomic_data.string(),
            CoreType::ARRAY(array_data) => array_data.string(),
            CoreType::USER_DEFINED(user_defined_data) => user_defined_data.clone(),
            CoreType::NON_TYPED => Rc::new(String::from(NON_TYPED)),
        }
    }
}

// TODO - convert it into to_string type of method
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic_data) => write!(f, "{}", atomic_data.string()),
            CoreType::USER_DEFINED(user_defined_data) => write!(f, "{}", user_defined_data),
            CoreType::ARRAY(array_data) => write!(f, "{}", array_data.string()),
            CoreType::NON_TYPED => write!(f, "{}", NON_TYPED),
        }
    }
}
