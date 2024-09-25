use super::{
    core::TypeStringifyContext,
    helper::try_infer_types_from_tuple,
    traits::{OperatorCompatiblity, TypeLike},
};
use crate::{
    constants::common::BOOL,
    lexer::token::BinaryOperatorKind,
    parser::type_checker::InferredConcreteTypesEntry,
    scope::{
        concrete::{TurbofishTypes, TypeGenericsInstantiationContext},
        namespace::Namespace,
        symbol::{
            interfaces::InterfaceBounds, types::generic_ty::GenericTypeDeclarationPlaceCategory,
        },
        traits::InstantiationContext,
    },
    types::core::{CoreType, Type},
};
use std::cmp;

#[derive(Debug, Clone)]
pub struct Tuple {
    sub_types: Vec<Type>,
}

impl Tuple {
    pub fn new(sub_types: Vec<Type>) -> Tuple {
        Tuple { sub_types }
    }

    pub fn sub_types(&self) -> &Vec<Type> {
        &self.sub_types
    }

    fn check_operator_for_tuple(
        &self,
        other: &Type,
        operator_kind: &BinaryOperatorKind,
        namespace: &Namespace,
    ) -> Option<Type> {
        let CoreType::Tuple(other_tuple) = other.core_ty() else {
            return None;
        };

        let other_len = other_tuple.sub_types.len();
        let self_len = self.sub_types.len();
        let min_len = cmp::min(self_len, other_len);

        for i in 0..min_len {
            self.sub_types[i].check_operator(
                &other_tuple.sub_types[i],
                operator_kind,
                namespace,
            )?;
        }

        Some(Type::new_with_atomic(BOOL))
    }
}

impl TypeLike for Tuple {
    fn is_eq(&self, other_ty: &Type, namespace: &Namespace) -> bool {
        let CoreType::Tuple(tuple_data) = other_ty.core_ty() else {
            return false;
        };

        if tuple_data.sub_types.len() != self.sub_types.len() {
            return false;
        }

        let len = self.sub_types.len();

        for i in 0..len {
            if !self.sub_types[i].is_eq(&tuple_data.sub_types[i], namespace) {
                return false;
            }
        }

        true
    }

    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        context: TypeGenericsInstantiationContext,
        namespace: &Namespace,
    ) -> bool {
        let CoreType::Tuple(tuple_data) = other_ty.core_ty() else {
            return false;
        };

        if tuple_data.sub_types.len() != self.sub_types.len() {
            return false;
        }

        let len = self.sub_types.len();

        for i in 0..len {
            if !self.sub_types[i].is_structurally_eq(&tuple_data.sub_types[i], context, namespace) {
                return false;
            }
        }

        true
    }

    fn concretize<'a, T: InstantiationContext<'a> + Copy>(
        &self,
        context: T,
        namespace: &Namespace,
    ) -> Type {
        let mut concrete_types = vec![];

        for ty in &self.sub_types {
            concrete_types.push(ty.concretize(context, namespace));
        }

        Type::new_with_tuple(concrete_types)
    }

    fn is_ty_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        _namespace: &Namespace,
    ) -> bool {
        // TODO - add checks for interfaces which `Tuple` would implement like `Iterator`, `Index`
        interface_bounds.len() == 0
    }

    fn try_infer_ty_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&TurbofishTypes>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        namespace: &Namespace,
    ) -> Result<(), ()> {
        let CoreType::Tuple(tuple_ty) = received_ty.core_ty() else {
            return Err(());
        };

        let generics_containing_types_tuple = &self.sub_types;
        let base_types_tuple = &tuple_ty.sub_types;

        try_infer_types_from_tuple(
            base_types_tuple,
            generics_containing_types_tuple,
            inferred_concrete_types,
            global_concrete_types,
            num_inferred_types,
            inference_category,
            namespace,
        )
    }

    fn to_string(&self, context: TypeStringifyContext) -> String {
        let mut str = self.sub_types[0].to_string(context);

        for i in 1..self.sub_types.len() {
            str.push_str(&format!(", {}", self.sub_types[i].to_string(context)));
        }

        format!("({})", str)
    }
}

impl OperatorCompatiblity for Tuple {
    fn check_add(&self, other: &Type, _namespace: &Namespace) -> Option<Type> {
        let CoreType::Tuple(other_tuple) = other.core_ty() else {
            return None;
        };

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

    fn check_subtract(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_multiply(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_divide(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_double_equal(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        self.check_operator_for_tuple(other, &BinaryOperatorKind::DoubleEqual, namespace)
    }

    fn check_greater(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        self.check_operator_for_tuple(other, &BinaryOperatorKind::Greater, namespace)
    }

    fn check_less(&self, other: &Type, namespace: &Namespace) -> Option<Type> {
        self.check_operator_for_tuple(other, &BinaryOperatorKind::Less, namespace)
    }

    fn check_and(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_or(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }
}
