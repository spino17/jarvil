use crate::constants::common::{BOOL, FLOAT, INT, NON_TYPED, STRING};
use crate::types::{array::Array, atomic::Atomic};
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use super::lambda::Lambda;

pub trait AbstractType {
    fn is_eq(&self, base_type: &Type) -> bool;
}

#[derive(Debug)]
pub enum CoreType {
    ATOMIC(Atomic),
    STRUCT(String),
    LAMBDA(Lambda),
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
impl AbstractType for Type {
    fn is_eq(&self, base_type: &Type) -> bool {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic)    => atomic.is_eq(base_type),
            CoreType::STRUCT(name)      => {
                match base_type.0.as_ref() {
                    CoreType::STRUCT(base_name) => name.eq(base_name),
                    _ => false
                }
            },
            CoreType::LAMBDA(lambda)    => lambda.is_eq(base_type),
            CoreType::ARRAY(array)       => array.is_eq(base_type),
            CoreType::NON_TYPED                  => match base_type.0.as_ref() {
                CoreType::NON_TYPED => true,
                _ => false,
            },
        }
    }
}

// TODO - convert it into to_string type of method
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic_data)   => write!(f, "{}", atomic_data.to_string()),
            CoreType::STRUCT(name)          => write!(f, "{}", name),
            CoreType::LAMBDA(lambda)        => write!(f, "{}", lambda.to_string()),
            CoreType::ARRAY(array_data)      => write!(f, "{}", array_data.to_string()),
            CoreType::NON_TYPED                      => write!(f, "{}", NON_TYPED),
        }
    }
}
