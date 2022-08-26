use crate::constants::common::{BOOL, FLOAT, INT, NON_TYPED, STRING};
use crate::types::{array::Array, atomic::Atomic};
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use super::lambda::Lambda;
use super::r#struct::Struct;

pub trait AbstractType {
    fn is_eq(&self, base_type: &Type) -> bool;
}

#[derive(Debug)]
pub enum CoreType {
    ATOMIC(Atomic),
    STRUCT(Struct),
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
            CoreType::ATOMIC(atomic_type)   => atomic_type.is_eq(base_type),
            CoreType::STRUCT(struct_type)   => struct_type.is_eq(base_type),
            CoreType::LAMBDA(lambda_type)   => lambda_type.is_eq(base_type),
            CoreType::ARRAY(array_type)      => array_type.is_eq(base_type),
            CoreType::NON_TYPED                      => match base_type.0.as_ref() {
                CoreType::NON_TYPED => true,
                _ => false,
            },
        }
    }
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic_type)   => write!(f, "{}", atomic_type.to_string()),
            CoreType::STRUCT(struct_type)   => write!(f, "{}", struct_type.to_string()),
            CoreType::LAMBDA(lambda_type)   => write!(f, "{}", lambda_type.to_string()),
            CoreType::ARRAY(array_type)      => write!(f, "{}", array_type.to_string()),
            CoreType::NON_TYPED                      => write!(f, "{}", NON_TYPED),
        }
    }
}
