use crate::constants::common::{BOOL, FLOAT, INT, STRING};
use crate::types::core::{AbstractType, CoreType, Type};

use super::core::OperatorCompatiblity;

#[derive(Debug)]
pub enum Atomic {
    INT,
    FLOAT,
    STRING,
    BOOL,
}

impl Atomic {
    pub fn new(name: &str) -> Atomic {
        match name {
            INT => Atomic::INT,
            FLOAT => Atomic::FLOAT,
            STRING => Atomic::STRING,
            BOOL => Atomic::BOOL,
            _ => unreachable!(
                "name should be `{}`, `{}`, `{}` and `{}`",
                INT, FLOAT, STRING, BOOL
            ),
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Atomic::INT => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Atomic::FLOAT => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Atomic::STRING => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Atomic::BOOL => true,
            _ => false,
        }
    }
}

impl AbstractType for Atomic {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::ATOMIC(atomic_data) => match atomic_data {
                Atomic::INT => self.is_int(),
                Atomic::FLOAT => self.is_float(),
                Atomic::STRING => self.is_string(),
                Atomic::BOOL => self.is_bool(),
            },
            _ => false,
        }
    }
}

impl ToString for Atomic {
    fn to_string(&self) -> String {
        match self {
            Atomic::INT => String::from(INT),
            Atomic::FLOAT => String::from(FLOAT),
            Atomic::STRING => String::from(STRING),
            Atomic::BOOL => String::from(BOOL),
        }
    }
}

impl OperatorCompatiblity for Atomic {
    fn check_add(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::ATOMIC(other_atomic) => match self {
                Atomic::INT => match other_atomic {
                    Atomic::INT => return Some(Type::new_with_atomic(INT)),
                    Atomic::FLOAT => return Some(Type::new_with_atomic(FLOAT)),
                    _ => return None,
                },
                Atomic::FLOAT => match other_atomic {
                    Atomic::INT => return Some(Type::new_with_atomic(FLOAT)),
                    Atomic::FLOAT => return Some(Type::new_with_atomic(FLOAT)),
                    _ => return None,
                },
                Atomic::STRING => match other_atomic {
                    Atomic::STRING => return Some(Type::new_with_atomic(STRING)),
                    _ => return None,
                },
                _ => return None,
            },
            _ => None,
        }
    }

    fn check_subtract(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::ATOMIC(other_atomic) => match self {
                Atomic::INT => match other_atomic {
                    Atomic::INT => return Some(Type::new_with_atomic(INT)),
                    Atomic::FLOAT => return Some(Type::new_with_atomic(FLOAT)),
                    _ => return None,
                },
                Atomic::FLOAT => match other_atomic {
                    Atomic::INT => return Some(Type::new_with_atomic(FLOAT)),
                    Atomic::FLOAT => return Some(Type::new_with_atomic(FLOAT)),
                    _ => return None,
                },
                _ => return None,
            },
            _ => None,
        }
    }

    fn check_multiply(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::ATOMIC(other_atomic) => match self {
                Atomic::INT => match other_atomic {
                    Atomic::INT => return Some(Type::new_with_atomic(INT)),
                    Atomic::FLOAT => return Some(Type::new_with_atomic(FLOAT)),
                    _ => return None,
                },
                Atomic::FLOAT => match other_atomic {
                    Atomic::INT => return Some(Type::new_with_atomic(FLOAT)),
                    Atomic::FLOAT => return Some(Type::new_with_atomic(FLOAT)),
                    _ => return None,
                },
                _ => return None,
            },
            _ => None,
        }
    }

    fn check_divide(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::ATOMIC(other_atomic) => match self {
                Atomic::INT => match other_atomic {
                    Atomic::INT => return Some(Type::new_with_atomic(FLOAT)),
                    Atomic::FLOAT => return Some(Type::new_with_atomic(FLOAT)),
                    _ => return None,
                },
                Atomic::FLOAT => match other_atomic {
                    Atomic::INT => return Some(Type::new_with_atomic(FLOAT)),
                    Atomic::FLOAT => return Some(Type::new_with_atomic(FLOAT)),
                    _ => return None,
                },
                _ => return None,
            },
            _ => None,
        }
    }

    fn check_double_equal(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::ATOMIC(other_atomic) => match self {
                Atomic::INT => match other_atomic {
                    Atomic::INT => return Some(Type::new_with_atomic(BOOL)),
                    Atomic::FLOAT => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                Atomic::FLOAT => match other_atomic {
                    Atomic::INT => return Some(Type::new_with_atomic(BOOL)),
                    Atomic::FLOAT => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                Atomic::BOOL => match other_atomic {
                    Atomic::BOOL => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                Atomic::STRING => match other_atomic {
                    Atomic::STRING => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
            },
            _ => return None,
        }
    }

    fn check_greater(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::ATOMIC(other_atomic) => match self {
                Atomic::INT => match other_atomic {
                    Atomic::INT => return Some(Type::new_with_atomic(BOOL)),
                    Atomic::FLOAT => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                Atomic::FLOAT => match other_atomic {
                    Atomic::INT => return Some(Type::new_with_atomic(BOOL)),
                    Atomic::FLOAT => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                _ => return None,
            },
            _ => None,
        }
    }

    fn check_less(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::ATOMIC(other_atomic) => match self {
                Atomic::INT => match other_atomic {
                    Atomic::INT => return Some(Type::new_with_atomic(BOOL)),
                    Atomic::FLOAT => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                Atomic::FLOAT => match other_atomic {
                    Atomic::INT => return Some(Type::new_with_atomic(BOOL)),
                    Atomic::FLOAT => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                _ => return None,
            },
            _ => None,
        }
    }

    fn check_and(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::ATOMIC(other_atomic) => match self {
                Atomic::BOOL => match other_atomic {
                    Atomic::BOOL => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                _ => return None,
            },
            _ => None,
        }
    }

    fn check_or(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::ATOMIC(other_atomic) => match self {
                Atomic::BOOL => match other_atomic {
                    Atomic::BOOL => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                _ => return None,
            },
            _ => None,
        }
    }
}
