use super::lambda::Lambda;
use super::r#struct::Struct;
use crate::constants::common::{NON_TYPED, UNKNOWN};
use crate::parser::components::expression::atom;
use crate::scope::core::SymbolData;
use crate::scope::user_defined_types::UserDefinedTypeData;
use crate::types::{array::Array, atomic::Atomic};
use std::fmt::{write, Debug, Formatter};
use std::rc::Rc;

// Type is data structure for type object. Type objects are more desirable than type expressions as the later contains a lot of information
// not really useful for doing type-analysis.

pub trait AbstractType {
    fn is_eq(&self, base_type: &Type) -> bool;
}

#[derive(Debug)]
pub enum CoreType {
    ATOMIC(Atomic),
    STRUCT(Struct),
    LAMBDA(Lambda),
    ARRAY(Array),
    NON_TYPED,
    UNKNOWN,
    VOID,
    // TODO - add below types also
    // ENUMERATION,
    // TUPLES,
    // REFERENCE,
    // GENERIC
}

#[derive(Debug, Clone)]
pub struct Type(pub Rc<CoreType>);
impl Type {
    pub fn new_with_atomic(name: &str) -> Type {
        Type(Rc::new(CoreType::ATOMIC(Atomic::new(name))))
    }

    pub fn new_with_struct(name: String, symbol_data: &SymbolData<UserDefinedTypeData>) -> Type {
        Type(Rc::new(CoreType::STRUCT(Struct::new(name, symbol_data))))
    }

    pub fn new_with_lambda(
        name: Option<String>,
        symbol_data: &SymbolData<UserDefinedTypeData>,
    ) -> Type {
        Type(Rc::new(CoreType::LAMBDA(Lambda::new(name, symbol_data))))
    }

    pub fn new_with_array(element_type: &Type, size: usize) -> Type {
        Type(Rc::new(CoreType::ARRAY(Array::new(element_type, size))))
    }

    pub fn new_with_unknown() -> Type {
        Type(Rc::new(CoreType::UNKNOWN))
    }

    pub fn new_with_void() -> Type {
        Type(Rc::new(CoreType::VOID))
    }

    pub fn is_void(&self) -> bool {
        match self.0.as_ref() {
            CoreType::VOID => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic) => atomic.is_bool(),
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic) => atomic.is_int(),
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic) => atomic.is_float(),
            _ => false,
        }
    }

    pub fn is_numeric(&self) -> bool {
        if self.is_int() || self.is_float() {
            true
        } else {
            false
        }
    }

    pub fn is_lambda(&self) -> bool {
        match self.0.as_ref() {
            CoreType::LAMBDA(_) => true,
            _ => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self.0.as_ref() {
            CoreType::UNKNOWN => true,
            _ => false,
        }
    }
}
impl AbstractType for Type {
    fn is_eq(&self, base_type: &Type) -> bool {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic_type) => atomic_type.is_eq(base_type),
            CoreType::STRUCT(struct_type) => struct_type.is_eq(base_type),
            CoreType::LAMBDA(lambda_type) => lambda_type.is_eq(base_type),
            CoreType::ARRAY(array_type) => array_type.is_eq(base_type),
            CoreType::UNKNOWN => match base_type.0.as_ref() {
                CoreType::UNKNOWN => true,
                _ => false,
            },
            CoreType::NON_TYPED => match base_type.0.as_ref() {
                CoreType::NON_TYPED => true,
                _ => false,
            },
            CoreType::VOID => match base_type.0.as_ref() {
                CoreType::VOID => true,
                _ => false,
            },
        }
    }
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic_type) => write!(f, "{}", atomic_type.to_string()),
            CoreType::STRUCT(struct_type) => write!(f, "{}", struct_type.to_string()),
            CoreType::LAMBDA(lambda_type) => write!(f, "{}", lambda_type.to_string()),
            CoreType::ARRAY(array_type) => write!(f, "{}", array_type.to_string()),
            CoreType::UNKNOWN => write!(f, "{}", UNKNOWN),
            CoreType::NON_TYPED => write!(f, "{}", NON_TYPED),
            CoreType::VOID => write!(f, "()"),
        }
    }
}
