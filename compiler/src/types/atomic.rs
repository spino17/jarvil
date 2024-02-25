use super::traits::{OperatorCompatiblity, TypeLike};
use crate::constants::common::{BOOL, FLOAT, INT, STRING};
use crate::core::string_interner::Interner;
use crate::parser::type_checker::InferredConcreteTypesEntry;
use crate::scope::concrete::{ConcreteTypesTuple, ConcretizationContext};
use crate::scope::namespace::Namespace;
use crate::scope::symbol::interfaces::InterfaceBounds;
use crate::scope::symbol::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::types::core::{CoreType, Type};

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
        matches!(self, Atomic::Int)
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Atomic::Float)
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Atomic::String)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Atomic::Bool)
    }
}

impl TypeLike for Atomic {
    fn is_eq(&self, other_ty: &Type, _namespace: &Namespace) -> bool {
        let CoreType::Atomic(atomic_data) = other_ty.core_ty() else {
            return false;
        };
        match atomic_data {
            Atomic::Int => self.is_int(),
            Atomic::Float => self.is_float(),
            Atomic::String => self.is_string(),
            Atomic::Bool => self.is_bool(),
        }
    }

    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        _context: &ConcretizationContext,
        _namespace: &Namespace,
    ) -> bool {
        let CoreType::Atomic(atomic_data) = other_ty.core_ty() else {
            return false;
        };
        match atomic_data {
            Atomic::Int => self.is_int(),
            Atomic::Float => self.is_float(),
            Atomic::String => self.is_string(),
            Atomic::Bool => self.is_bool(),
        }
    }

    fn concretize(&self, _context: &ConcretizationContext, _namespace: &Namespace) -> Type {
        unreachable!()
    }

    fn is_type_bounded_by_interfaces(
        &self,
        _interface_bounds: &InterfaceBounds,
        _namespace: &Namespace,
    ) -> bool {
        unreachable!()
    }

    fn try_infer_type_or_check_equivalence(
        &self,
        _received_ty: &Type,
        _inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        _global_concrete_types: Option<&ConcreteTypesTuple>,
        _num_inferred_types: &mut usize,
        _inference_category: GenericTypeDeclarationPlaceCategory,
        _namespace: &Namespace,
    ) -> Result<(), ()> {
        unreachable!()
    }

    fn to_string(&self, _interner: &Interner, _namespace: &Namespace) -> String {
        match self {
            Atomic::Int => String::from(INT),
            Atomic::Float => String::from(FLOAT),
            Atomic::String => String::from(STRING),
            Atomic::Bool => String::from(BOOL),
        }
    }
}

impl OperatorCompatiblity for Atomic {
    fn check_add(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.core_ty() else {
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

    fn check_subtract(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.core_ty() else {
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

    fn check_multiply(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.core_ty() else {
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

    fn check_divide(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.core_ty() else {
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

    fn check_double_equal(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.core_ty() else {
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

    fn check_greater(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.core_ty() else {
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

    fn check_less(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.core_ty() else {
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

    fn check_and(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.core_ty() else {
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

    fn check_or(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Atomic(other_atomic) = other.core_ty() else {
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
