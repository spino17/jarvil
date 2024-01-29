use super::core::OperatorCompatiblity;
use crate::constants::common::{BOOL, FLOAT, INT, STRING};
use crate::core::string_interner::Interner;
use crate::parser::type_checker::InferredConcreteTypesEntry;
use crate::scope::concrete::{ConcreteTypesTuple, ConcretizationContext};
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
        let CoreType::Atomic(atomic_data) = other_ty.0.as_ref() else {
            return false;
        };
        match atomic_data {
            Atomic::Int => self.is_int(),
            Atomic::Float => self.is_float(),
            Atomic::String => self.is_string(),
            Atomic::Bool => self.is_bool(),
        }
    }

    fn concretize(&self, _context: &ConcretizationContext) -> Type {
        unreachable!()
    }

    fn is_type_bounded_by_interfaces(&self, _interface_bounds: &InterfaceBounds) -> bool {
        unreachable!()
    }

    fn try_infer_type_or_check_equivalence(
        &self,
        _received_ty: &Type,
        _inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        _global_concrete_types: Option<&ConcreteTypesTuple>,
        _num_inferred_types: &mut usize,
        _inference_category: GenericTypeDeclarationPlaceCategory,
    ) -> Result<(), ()> {
        unreachable!()
    }

    fn to_string(&self, _interner: &Interner) -> String {
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
        let CoreType::Atomic(other_atomic) = other.0.as_ref() else {
            return None;
        };
        match self {
            Atomic::Int => match other_atomic {
                Atomic::Int => Some(Type::new_with_atomic(INT)),
                Atomic::Float => Some(Type::new_with_atomic(FLOAT)),
                _ => None,
            },
            Atomic::Float => match other_atomic {
                Atomic::Int => Some(Type::new_with_atomic(FLOAT)),
                Atomic::Float => Some(Type::new_with_atomic(FLOAT)),
                _ => None,
            },
            Atomic::String => match other_atomic {
                Atomic::String => Some(Type::new_with_atomic(STRING)),
                _ => None,
            },
            _ => None,
        }
    }

    fn check_subtract(&self, other: &Type) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.0.as_ref() else {
            return None;
        };
        match self {
            Atomic::Int => match other_atomic {
                Atomic::Int => Some(Type::new_with_atomic(INT)),
                Atomic::Float => Some(Type::new_with_atomic(FLOAT)),
                _ => None,
            },
            Atomic::Float => match other_atomic {
                Atomic::Int => Some(Type::new_with_atomic(FLOAT)),
                Atomic::Float => Some(Type::new_with_atomic(FLOAT)),
                _ => None,
            },
            _ => None,
        }
    }

    fn check_multiply(&self, other: &Type) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.0.as_ref() else {
            return None;
        };
        match self {
            Atomic::Int => match other_atomic {
                Atomic::Int => Some(Type::new_with_atomic(INT)),
                Atomic::Float => Some(Type::new_with_atomic(FLOAT)),
                _ => None,
            },
            Atomic::Float => match other_atomic {
                Atomic::Int => Some(Type::new_with_atomic(FLOAT)),
                Atomic::Float => Some(Type::new_with_atomic(FLOAT)),
                _ => None,
            },
            _ => None,
        }
    }

    fn check_divide(&self, other: &Type) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.0.as_ref() else {
            return None;
        };
        match self {
            Atomic::Int => match other_atomic {
                Atomic::Int => Some(Type::new_with_atomic(FLOAT)),
                Atomic::Float => Some(Type::new_with_atomic(FLOAT)),
                _ => None,
            },
            Atomic::Float => match other_atomic {
                Atomic::Int => Some(Type::new_with_atomic(FLOAT)),
                Atomic::Float => Some(Type::new_with_atomic(FLOAT)),
                _ => None,
            },
            _ => None,
        }
    }

    fn check_double_equal(&self, other: &Type) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.0.as_ref() else {
            return None;
        };
        match self {
            Atomic::Int => match other_atomic {
                Atomic::Int => Some(Type::new_with_atomic(BOOL)),
                Atomic::Float => Some(Type::new_with_atomic(BOOL)),
                _ => None,
            },
            Atomic::Float => match other_atomic {
                Atomic::Int => Some(Type::new_with_atomic(BOOL)),
                Atomic::Float => Some(Type::new_with_atomic(BOOL)),
                _ => None,
            },
            Atomic::Bool => match other_atomic {
                Atomic::Bool => Some(Type::new_with_atomic(BOOL)),
                _ => None,
            },
            Atomic::String => match other_atomic {
                Atomic::String => Some(Type::new_with_atomic(BOOL)),
                _ => None,
            },
        }
    }

    fn check_greater(&self, other: &Type) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.0.as_ref() else {
            return None;
        };
        match self {
            Atomic::Int => match other_atomic {
                Atomic::Int => Some(Type::new_with_atomic(BOOL)),
                Atomic::Float => Some(Type::new_with_atomic(BOOL)),
                _ => None,
            },
            Atomic::Float => match other_atomic {
                Atomic::Int => Some(Type::new_with_atomic(BOOL)),
                Atomic::Float => Some(Type::new_with_atomic(BOOL)),
                _ => None,
            },
            Atomic::String => match other_atomic {
                Atomic::String => Some(Type::new_with_atomic(BOOL)),
                _ => None,
            },
            Atomic::Bool => match other_atomic {
                Atomic::Bool => Some(Type::new_with_atomic(BOOL)),
                _ => None,
            },
        }
    }

    fn check_less(&self, other: &Type) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.0.as_ref() else {
            return None;
        };
        match self {
            Atomic::Int => match other_atomic {
                Atomic::Int => Some(Type::new_with_atomic(BOOL)),
                Atomic::Float => Some(Type::new_with_atomic(BOOL)),
                _ => None,
            },
            Atomic::Float => match other_atomic {
                Atomic::Int => Some(Type::new_with_atomic(BOOL)),
                Atomic::Float => Some(Type::new_with_atomic(BOOL)),
                _ => None,
            },
            Atomic::String => match other_atomic {
                Atomic::String => Some(Type::new_with_atomic(BOOL)),
                _ => None,
            },
            Atomic::Bool => match other_atomic {
                Atomic::Bool => Some(Type::new_with_atomic(BOOL)),
                _ => None,
            },
        }
    }

    fn check_and(&self, other: &Type) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.0.as_ref() else {
            return None;
        };
        match self {
            Atomic::Bool => match other_atomic {
                Atomic::Bool => Some(Type::new_with_atomic(BOOL)),
                _ => None,
            },
            _ => None,
        }
    }

    fn check_or(&self, other: &Type) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.0.as_ref() else {
            return None;
        };
        match self {
            Atomic::Bool => match other_atomic {
                Atomic::Bool => Some(Type::new_with_atomic(BOOL)),
                _ => None,
            },
            _ => None,
        }
    }
}
