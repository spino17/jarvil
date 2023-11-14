use super::{core::OperatorCompatiblity, helper::try_infer_types_from_tuple};
use crate::{
    constants::common::BOOL,
    core::string_interner::Interner,
    lexer::token::BinaryOperatorKind,
    parser::type_checker::InferredConcreteTypesEntry,
    scope::{
        concrete::{ConcreteTypesTuple, ConcretizationContext},
        interfaces::InterfaceBounds,
        types::generic_type::GenericTypeDeclarationPlaceCategory,
    },
    types::core::{AbstractType, CoreType, Type},
};
use std::cmp;

#[derive(Debug, Clone)]
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
                    self.sub_types[i].check_operator(&other_tuple.sub_types[i], operator_kind)?;
                }
                Some(Type::new_with_atomic(BOOL))
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
                }
                let len = self.sub_types.len();
                for i in 0..len {
                    if !self.sub_types[i].is_eq(&tuple_data.sub_types[i]) {
                        return false;
                    }
                }
                true
            }
            CoreType::Any => true,
            _ => false,
        }
    }

    fn is_structurally_eq(&self, other_ty: &Type, context: &ConcretizationContext) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Tuple(tuple_data) => {
                if tuple_data.sub_types.len() != self.sub_types.len() {
                    return false;
                }
                let len = self.sub_types.len();
                for i in 0..len {
                    if !self.sub_types[i].is_structurally_eq(&tuple_data.sub_types[i], context) {
                        return false;
                    }
                }
                true
            }
            _ => false,
        }
    }

    fn concretize(&self, context: &ConcretizationContext) -> Type {
        let mut concrete_types = vec![];
        for ty in &self.sub_types {
            concrete_types.push(ty.concretize(context));
        }
        Type::new_with_tuple(concrete_types)
    }

    fn is_type_bounded_by_interfaces(&self, interface_bounds: &InterfaceBounds) -> bool {
        // TODO - add checks for interfaces which `Tuple` would implement like `Iterator`, `Index`
        interface_bounds.len() == 0
    }

    fn try_infer_type_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&ConcreteTypesTuple>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
    ) -> Result<(), ()> {
        match received_ty.0.as_ref() {
            CoreType::Tuple(tuple_ty) => {
                let generics_containing_types_tuple = &self.sub_types;
                let base_types_tuple = &tuple_ty.sub_types;
                try_infer_types_from_tuple(
                    base_types_tuple,
                    generics_containing_types_tuple,
                    inferred_concrete_types,
                    global_concrete_types,
                    num_inferred_types,
                    inference_category,
                )
            }
            _ => Err(()),
        }
    }

    fn to_string(&self, interner: &Interner) -> String {
        let mut str = self.sub_types[0].to_string(interner);
        for i in 1..self.sub_types.len() {
            str.push_str(&format!(", {}", self.sub_types[i].to_string(interner)));
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
                Some(Type::new_with_tuple(combined_sub_types))
            }
            _ => None,
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
