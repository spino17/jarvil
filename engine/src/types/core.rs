use super::hashmap::HashMap;
use super::lambda::Lambda;
use super::r#struct::Struct;
use crate::constants::common::{BOOL, NON_TYPED, UNKNOWN};
use crate::lexer::token::BinaryOperatorKind;
use crate::scope::core::SymbolData;
use crate::scope::user_defined_types::UserDefinedTypeData;
use crate::types::{array::Array, atomic::Atomic};
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

pub trait AbstractType {
    fn is_eq(&self, base_type: &Type) -> bool;
}

pub trait OperatorCompatiblity {
    fn check_add(&self, other: &Type) -> Option<Type>;
    fn check_subtract(&self, other: &Type) -> Option<Type>;
    fn check_multiply(&self, other: &Type) -> Option<Type>;
    fn check_divide(&self, other: &Type) -> Option<Type>;
    fn check_double_equal(&self, other: &Type) -> Option<Type>;
    fn check_greater(&self, other: &Type) -> Option<Type>;
    fn check_less(&self, other: &Type) -> Option<Type>;
    fn check_and(&self, other: &Type) -> Option<Type>;
    fn check_or(&self, other: &Type) -> Option<Type>;
    fn check_not_equal(&self, other: &Type) -> Option<Type> {
        if self.check_double_equal(other).is_some() {
            return Some(Type::new_with_atomic(BOOL));
        }
        return None;
    }
    fn check_greater_equal(&self, other: &Type) -> Option<Type> {
        if self.check_greater(other).is_some() && self.check_double_equal(other).is_some() {
            return Some(Type::new_with_atomic(BOOL));
        }
        return None;
    }
    fn check_less_equal(&self, other: &Type) -> Option<Type> {
        if self.check_less(other).is_some() && self.check_double_equal(other).is_some() {
            return Some(Type::new_with_atomic(BOOL));
        }
        return None;
    }
}

#[derive(Debug)]
pub enum CoreType {
    ATOMIC(Atomic),
    STRUCT(Struct),
    LAMBDA(Lambda),
    ARRAY(Array),
    HASHMAP(HashMap),
    NON_TYPED,
    UNKNOWN,
    VOID,
    // TODO - add below types also
    // ANY // this type can be used to denote that any variable with this type can have any valid datatypes, will be useful in things like print(...)
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

    pub fn new_with_array(element_type: &Type) -> Type {
        Type(Rc::new(CoreType::ARRAY(Array::new(element_type))))
    }

    pub fn new_with_hashmap(key_type: &Type, value_type: &Type) -> Type {
        Type(Rc::new(CoreType::HASHMAP(HashMap::new(
            key_type, value_type,
        ))))
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

    pub fn is_string(&self) -> bool {
        match self.0.as_ref() {
            CoreType::ATOMIC(atomic) => atomic.is_string(),
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self.0.as_ref() {
            CoreType::ARRAY(_) => true,
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

    // This function returns Some if operation is possible and None otherwise
    pub fn check_operator(&self, other: &Type, op_kind: &BinaryOperatorKind) -> Option<Type> {
        match op_kind {
            BinaryOperatorKind::Add => {
                impl_op_compatiblity!(check_add, self, other)
            }
            BinaryOperatorKind::Subtract => {
                impl_op_compatiblity!(check_subtract, self, other)
            }
            BinaryOperatorKind::Multiply => {
                impl_op_compatiblity!(check_multiply, self, other)
            }
            BinaryOperatorKind::Divide => {
                impl_op_compatiblity!(check_divide, self, other)
            }
            BinaryOperatorKind::Less => {
                impl_op_compatiblity!(check_less, self, other)
            }
            BinaryOperatorKind::LessEqual => {
                impl_op_compatiblity!(check_less_equal, self, other)
            }
            BinaryOperatorKind::Greater => {
                impl_op_compatiblity!(check_greater, self, other)
            }
            BinaryOperatorKind::GreaterEqual => {
                impl_op_compatiblity!(check_greater_equal, self, other)
            }
            BinaryOperatorKind::DoubleEqual => {
                impl_op_compatiblity!(check_double_equal, self, other)
            }
            BinaryOperatorKind::NotEqual => {
                impl_op_compatiblity!(check_not_equal, self, other)
            }
            BinaryOperatorKind::And => {
                impl_op_compatiblity!(check_and, self, other)
            }
            BinaryOperatorKind::Or => {
                impl_op_compatiblity!(check_or, self, other)
            }
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
            CoreType::HASHMAP(hashmap_type) => hashmap_type.is_eq(base_type),
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
            CoreType::HASHMAP(hashmap_type) => write!(f, "{}", hashmap_type.to_string()),
            CoreType::UNKNOWN => write!(f, "{}", UNKNOWN),
            CoreType::NON_TYPED => write!(f, "{}", NON_TYPED),
            CoreType::VOID => write!(f, "()"),
        }
    }
}
