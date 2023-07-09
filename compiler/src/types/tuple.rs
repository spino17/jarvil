use super::core::OperatorCompatiblity;
use crate::{
    constants::common::BOOL,
    lexer::token::BinaryOperatorKind,
    scope::concrete::core::ConcretizationContext,
    types::core::{AbstractType, CoreType, Type},
};
use std::cmp;

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

    fn check_operator_for_tuple(
        &self,
        other: &Type,
        operator_kind: &BinaryOperatorKind,
    ) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Tuple(other_tuple) => {
                let other_len = other_tuple.sub_types.len();
                let self_len = self.sub_types.len();
                let min_len = cmp::min(self_len, other_len);
                for i in 0..min_len {
                    if self.sub_types[i]
                        .check_operator(&other_tuple.sub_types[i], operator_kind)
                        .is_none()
                    {
                        return None;
                    }
                }
                return Some(Type::new_with_atomic(BOOL));
            }
            _ => None,
        }
    }
}

impl AbstractType for Tuple {
    fn is_eq(&self, other_ty: &Type) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Tuple(tuple_data) => {
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
            CoreType::Any => true,
            _ => false,
        }
    }

    fn concretize(&self, context: &ConcretizationContext) -> Type {
        let mut concrete_types = self.sub_types.clone();
        for (index, ty) in self.sub_types.iter().enumerate() {
            if ty.has_generics() {
                concrete_types[index] = ty.concretize(context);
            }
        }
        return Type::new_with_tuple(concrete_types);
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
    fn check_add(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Tuple(other_tuple) => {
                let self_sub_types = &self.sub_types;
                let other_sub_types = &other_tuple.sub_types;
                let mut combined_sub_types: Vec<Type> = vec![];
                for ty in self_sub_types {
                    combined_sub_types.push(ty.clone());
                }
                for ty in other_sub_types {
                    combined_sub_types.push(ty.clone())
                }
                return Some(Type::new_with_tuple(combined_sub_types));
            }
            _ => return None,
        }
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

    fn check_double_equal(&self, other: &Type) -> Option<Type> {
        self.check_operator_for_tuple(other, &BinaryOperatorKind::DoubleEqual)
    }

    fn check_greater(&self, other: &Type) -> Option<Type> {
        self.check_operator_for_tuple(other, &BinaryOperatorKind::Greater)
    }

    fn check_less(&self, other: &Type) -> Option<Type> {
        self.check_operator_for_tuple(other, &BinaryOperatorKind::Less)
    }

    fn check_and(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_or(&self, _other: &Type) -> Option<Type> {
        None
    }
}
