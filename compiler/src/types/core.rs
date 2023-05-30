use super::hashmap::HashMap;
use super::lambda::Lambda;
use super::r#struct::Struct;
use super::tuple::Tuple;
use crate::constants::common::{ANY, BOOL, UNKNOWN, UNSET};
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
    Atomic(Atomic),
    Struct(Struct),
    Lambda(Lambda),
    Array(Array),
    Tuple(Tuple),
    HashMap(HashMap),
    Unknown,
    Void,
    Unset,
    Any,
}

#[derive(Debug, Clone)]
pub struct Type(pub Rc<CoreType>);

impl Type {
    pub fn new_with_atomic(name: &str) -> Type {
        Type(Rc::new(CoreType::Atomic(Atomic::new(name))))
    }

    pub fn new_with_struct(name: String, symbol_data: &SymbolData<UserDefinedTypeData>) -> Type {
        Type(Rc::new(CoreType::Struct(Struct::new(name, symbol_data))))
    }

    pub fn new_with_lambda(name: Option<String>, params: &Vec<Type>, return_type: &Type) -> Type {
        Type(Rc::new(CoreType::Lambda(Lambda::new(
            name,
            params,
            return_type,
        ))))
    }

    pub fn new_with_array(element_type: &Type) -> Type {
        Type(Rc::new(CoreType::Array(Array::new(element_type))))
    }

    pub fn new_with_tuple(types: Vec<Type>) -> Type {
        Type(Rc::new(CoreType::Tuple(Tuple::new(types))))
    }

    pub fn new_with_hashmap(key_type: &Type, value_type: &Type) -> Type {
        Type(Rc::new(CoreType::HashMap(HashMap::new(
            key_type, value_type,
        ))))
    }

    pub fn new_with_unknown() -> Type {
        Type(Rc::new(CoreType::Unknown))
    }

    pub fn new_with_unset() -> Type {
        Type(Rc::new(CoreType::Unset))
    }

    pub fn new_with_void() -> Type {
        Type(Rc::new(CoreType::Void))
    }

    pub fn new_with_any() -> Type {
        Type(Rc::new(CoreType::Any))
    }

    pub fn is_void(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Void => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Atomic(atomic) => atomic.is_string(),
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Array(_) => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Atomic(atomic) => atomic.is_bool(),
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Atomic(atomic) => atomic.is_int(),
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Atomic(atomic) => atomic.is_float(),
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
            CoreType::Lambda(_) => true,
            _ => false,
        }
    }

    pub fn is_hashmap(&self) -> bool {
        match self.0.as_ref() {
            CoreType::HashMap(_) => true,
            _ => false,
        }
    }

    pub fn is_immutable(&self) -> bool {
        self.is_string() || self.is_tuple()
    }

    pub fn is_tuple(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Tuple(_) => true,
            _ => false,
        }
    }

    pub fn is_hashable(&self) -> bool {
        // `int`, `float`, `str` and `tuple` with hashable sub_types are only hashable types
        match self.0.as_ref() {
            CoreType::Atomic(atomic) => {
                return atomic.is_int() || atomic.is_string() || atomic.is_float()
            }
            CoreType::Tuple(tuple) => {
                for ty in &tuple.sub_types {
                    if !ty.is_hashable() {
                        return false;
                    }
                }
                return true;
            }
            _ => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        match self.0.as_ref() {
            CoreType::Unknown => true,
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
            CoreType::Atomic(atomic_type) => atomic_type.is_eq(base_type),
            CoreType::Struct(struct_type) => struct_type.is_eq(base_type),
            CoreType::Lambda(lambda_type) => lambda_type.is_eq(base_type),
            CoreType::Array(array_type) => array_type.is_eq(base_type),
            CoreType::Tuple(tuple_type) => tuple_type.is_eq(base_type),
            CoreType::HashMap(hashmap_type) => hashmap_type.is_eq(base_type),
            CoreType::Unknown => return false,
            CoreType::Void => match base_type.0.as_ref() {
                CoreType::Void => true,
                _ => false,
            },
            CoreType::Unset => return false,
            CoreType::Any => return true,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self.0.as_ref() {
            CoreType::Atomic(atomic_type) => write!(f, "{}", atomic_type.to_string()),
            CoreType::Struct(struct_type) => write!(f, "{}", struct_type.to_string()),
            CoreType::Lambda(lambda_type) => write!(f, "{}", lambda_type.to_string()),
            CoreType::Array(array_type) => write!(f, "{}", array_type.to_string()),
            CoreType::Tuple(tuple_type) => write!(f, "{}", tuple_type.to_string()),
            CoreType::HashMap(hashmap_type) => write!(f, "{}", hashmap_type.to_string()),
            CoreType::Unknown => write!(f, "{}", UNKNOWN),
            CoreType::Void => write!(f, "()"),
            CoreType::Unset => write!(f, "{}", UNSET),
            CoreType::Any => write!(f, "{}", ANY),
        }
    }
}
