use super::core::OperatorCompatiblity;
use crate::types::core::{AbstractType, CoreType, Type};

#[derive(Debug)]
pub struct Tuple {
    pub sub_types: Vec<Type>,
}

impl Tuple {
    pub fn new(sub_types: Vec<Type>) -> Tuple {
        Tuple {
            sub_types: sub_types.clone(),
        }
    }
}

impl AbstractType for Tuple {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::TUPLE(tuple_data) => {
                if tuple_data.sub_types.len() != self.sub_types.len() {
                    return false;
                } else {
                    let len = self.sub_types.len();
                    for i in 0..len {
                        if !self.sub_types[i].is_eq(&tuple_data.sub_types[i]) {
                            return false;
                        }
                    }
                    return true;
                }
            }
            _ => false,
        }
    }
}

impl ToString for Tuple {
    fn to_string(&self) -> String {
        let mut str = self.sub_types[0].to_string();
        for i in 1..self.sub_types.len() {
            str.push_str(&format!(", {}", self.sub_types[i]));
        }
        format!("({})", str)
    }
}

impl OperatorCompatiblity for Tuple {
    fn check_add(&self, _other: &Type) -> Option<Type> {
        // TODO - add logic to type-check
        None
    }

    fn check_subtract(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_multiply(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_divide(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_double_equal(&self, _other: &Type) -> Option<Type> {
        // TODO - add logic to type-check
        None
    }

    fn check_greater(&self, _other: &Type) -> Option<Type> {
        // TODO - add logic to type-check
        None
    }

    fn check_less(&self, _other: &Type) -> Option<Type> {
        // TODO - add logic to type-check
        None
    }

    fn check_and(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_or(&self, _other: &Type) -> Option<Type> {
        None
    }
}
