use super::core::OperatorCompatiblity;
use crate::constants::common::{BOOL, FLOAT, INT, STRING};
use crate::parser::type_checker::InferredConcreteTypesEntry;
use crate::scope::concrete::core::ConcretizationContext;
use crate::scope::interfaces::InterfaceBounds;
use crate::scope::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::types::core::{AbstractType, CoreType, Type};

#[derive(Debug, Clone)]
pub enum Atomic {
    Int,
    Float,
    String,
    Bool,
}

impl Atomic {
    pub fn new(name: &str) -> Atomic {
        match name {
            INT => Atomic::Int,
            FLOAT => Atomic::Float,
            STRING => Atomic::String,
            BOOL => Atomic::Bool,
            _ => unreachable!(
                "name should be `{}`, `{}`, `{}` and `{}`",
                INT, FLOAT, STRING, BOOL
            ),
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            Atomic::Int => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Atomic::Float => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            Atomic::String => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Atomic::Bool => true,
            _ => false,
        }
    }
}

impl AbstractType for Atomic {
    fn is_eq(&self, other_ty: &Type) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Atomic(atomic_data) => match atomic_data {
                Atomic::Int => self.is_int(),
                Atomic::Float => self.is_float(),
                Atomic::String => self.is_string(),
                Atomic::Bool => self.is_bool(),
            },
            CoreType::Any => true,
            _ => false,
        }
    }

    fn is_structurally_eq(&self, other_ty: &Type, _context: &ConcretizationContext) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Atomic(atomic_data) => match atomic_data {
                Atomic::Int => self.is_int(),
                Atomic::Float => self.is_float(),
                Atomic::String => self.is_string(),
                Atomic::Bool => self.is_bool(),
            },
            _ => false,
        }
    }

    fn concretize(&self, _context: &ConcretizationContext) -> Type {
        unreachable!()
    }

    fn is_type_bounded_by_interfaces(&self, _interface_bounds: &InterfaceBounds) -> bool {
        unreachable!()
    }

    fn has_generics(&self) -> bool {
        unreachable!()
    }

    fn try_infer_type(
        &self,
        _received_ty: &Type,
        _inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        _global_concrete_types: Option<&Vec<Type>>,
        _num_inferred_types: &mut usize,
        _has_generics: &mut bool,
        _inference_category: GenericTypeDeclarationPlaceCategory,
    ) -> Result<(), ()> {
        unreachable!()
    }
}

impl ToString for Atomic {
    fn to_string(&self) -> String {
        match self {
            Atomic::Int => String::from(INT),
            Atomic::Float => String::from(FLOAT),
            Atomic::String => String::from(STRING),
            Atomic::Bool => String::from(BOOL),
        }
    }
}

impl OperatorCompatiblity for Atomic {
    fn check_add(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Atomic(other_atomic) => match self {
                Atomic::Int => match other_atomic {
                    Atomic::Int => return Some(Type::new_with_atomic(INT)),
                    Atomic::Float => return Some(Type::new_with_atomic(FLOAT)),
                    _ => return None,
                },
                Atomic::Float => match other_atomic {
                    Atomic::Int => return Some(Type::new_with_atomic(FLOAT)),
                    Atomic::Float => return Some(Type::new_with_atomic(FLOAT)),
                    _ => return None,
                },
                Atomic::String => match other_atomic {
                    Atomic::String => return Some(Type::new_with_atomic(STRING)),
                    _ => return None,
                },
                _ => return None,
            },
            _ => None,
        }
    }

    fn check_subtract(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Atomic(other_atomic) => match self {
                Atomic::Int => match other_atomic {
                    Atomic::Int => return Some(Type::new_with_atomic(INT)),
                    Atomic::Float => return Some(Type::new_with_atomic(FLOAT)),
                    _ => return None,
                },
                Atomic::Float => match other_atomic {
                    Atomic::Int => return Some(Type::new_with_atomic(FLOAT)),
                    Atomic::Float => return Some(Type::new_with_atomic(FLOAT)),
                    _ => return None,
                },
                _ => return None,
            },
            _ => None,
        }
    }

    fn check_multiply(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Atomic(other_atomic) => match self {
                Atomic::Int => match other_atomic {
                    Atomic::Int => return Some(Type::new_with_atomic(INT)),
                    Atomic::Float => return Some(Type::new_with_atomic(FLOAT)),
                    _ => return None,
                },
                Atomic::Float => match other_atomic {
                    Atomic::Int => return Some(Type::new_with_atomic(FLOAT)),
                    Atomic::Float => return Some(Type::new_with_atomic(FLOAT)),
                    _ => return None,
                },
                _ => return None,
            },// TODO - add array to enable syntax like `2 * [1, 2]`
            _ => None,
        }
    }

    fn check_divide(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Atomic(other_atomic) => match self {
                Atomic::Int => match other_atomic {
                    Atomic::Int => return Some(Type::new_with_atomic(FLOAT)),
                    Atomic::Float => return Some(Type::new_with_atomic(FLOAT)),
                    _ => return None,
                },
                Atomic::Float => match other_atomic {
                    Atomic::Int => return Some(Type::new_with_atomic(FLOAT)),
                    Atomic::Float => return Some(Type::new_with_atomic(FLOAT)),
                    _ => return None,
                },
                _ => return None,
            },
            _ => None,
        }
    }

    fn check_double_equal(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Atomic(other_atomic) => match self {
                Atomic::Int => match other_atomic {
                    Atomic::Int => return Some(Type::new_with_atomic(BOOL)),
                    Atomic::Float => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                Atomic::Float => match other_atomic {
                    Atomic::Int => return Some(Type::new_with_atomic(BOOL)),
                    Atomic::Float => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                Atomic::Bool => match other_atomic {
                    Atomic::Bool => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                Atomic::String => match other_atomic {
                    Atomic::String => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
            },
            _ => return None,
        }
    }

    fn check_greater(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Atomic(other_atomic) => match self {
                Atomic::Int => match other_atomic {
                    Atomic::Int => return Some(Type::new_with_atomic(BOOL)),
                    Atomic::Float => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                Atomic::Float => match other_atomic {
                    Atomic::Int => return Some(Type::new_with_atomic(BOOL)),
                    Atomic::Float => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                Atomic::String => match other_atomic {
                    Atomic::String => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                Atomic::Bool => match other_atomic {
                    Atomic::Bool => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
            },
            _ => None,
        }
    }

    fn check_less(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Atomic(other_atomic) => match self {
                Atomic::Int => match other_atomic {
                    Atomic::Int => return Some(Type::new_with_atomic(BOOL)),
                    Atomic::Float => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                Atomic::Float => match other_atomic {
                    Atomic::Int => return Some(Type::new_with_atomic(BOOL)),
                    Atomic::Float => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                Atomic::String => match other_atomic {
                    Atomic::String => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                Atomic::Bool => match other_atomic {
                    Atomic::Bool => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
            },
            _ => None,
        }
    }

    fn check_and(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Atomic(other_atomic) => match self {
                Atomic::Bool => match other_atomic {
                    Atomic::Bool => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                _ => return None,
            },
            _ => None,
        }
    }

    fn check_or(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Atomic(other_atomic) => match self {
                Atomic::Bool => match other_atomic {
                    Atomic::Bool => return Some(Type::new_with_atomic(BOOL)),
                    _ => return None,
                },
                _ => return None,
            },
            _ => None,
        }
    }
}
