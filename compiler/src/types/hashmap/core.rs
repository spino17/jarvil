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
    types::{
        core::{CoreType, Type, TypeStringifyContext},
        traits::{CollectionType, OperatorCompatiblity, TypeLike},
    },
};

#[derive(Debug, Clone)]
pub struct HashMap {
    key_ty: Type,
    value_ty: Type,
}

impl HashMap {
    pub fn new(key_ty: Type, value_ty: Type) -> HashMap {
        HashMap { key_ty, value_ty }
    }

    pub fn key_ty(&self) -> &Type {
        &self.key_ty
    }

    pub fn value_ty(&self) -> &Type {
        &self.value_ty
    }
}

impl TypeLike for HashMap {
    fn is_eq(&self, other_ty: &Type, namespace: &Namespace) -> bool {
        let CoreType::HashMap(hashmap_data) = other_ty.core_ty() else {
            return false;
        };

        self.key_ty.is_eq(&hashmap_data.key_ty, namespace)
            && self.value_ty.is_eq(&hashmap_data.value_ty, namespace)
    }

    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        context: TypeGenericsInstantiationContext,
        namespace: &Namespace,
    ) -> bool {
        let CoreType::HashMap(hashmap_data) = other_ty.core_ty() else {
            return false;
        };

        self.key_ty
            .is_structurally_eq(&hashmap_data.key_ty, context, namespace)
            && self
                .value_ty
                .is_structurally_eq(&hashmap_data.value_ty, context, namespace)
    }

    fn concretize<'a, T: InstantiationContext<'a> + Copy>(
        &self,
        context: T,
        namespace: &Namespace,
    ) -> Type {
        let concrete_key_ty = self.key_ty.concretize(context, namespace);
        let concrete_value_ty = self.value_ty.concretize(context, namespace);

        Type::new_with_hashmap(concrete_key_ty, concrete_value_ty)
    }

    fn is_ty_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        _namespace: &Namespace,
    ) -> bool {
        // TODO - add checks for interfaces which `HashMap` would implement like `Iterator`, `Index`
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
        let CoreType::HashMap(hashmap_ty) = received_ty.core_ty() else {
            return Err(());
        };

        self.key_ty.try_infer_ty_or_check_equivalence(
            &hashmap_ty.key_ty,
            inferred_concrete_types,
            global_concrete_types,
            num_inferred_types,
            inference_category,
            namespace,
        )?;
        self.value_ty.try_infer_ty_or_check_equivalence(
            &hashmap_ty.value_ty,
            inferred_concrete_types,
            global_concrete_types,
            num_inferred_types,
            inference_category,
            namespace,
        )?;

        Ok(())
    }

    fn to_string(&self, context: TypeStringifyContext) -> String {
        format!(
            "{{{} : {}}}",
            self.key_ty.to_string(context),
            self.value_ty.to_string(context)
        )
    }
}

impl OperatorCompatiblity for HashMap {
    fn check_add(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
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
        let CoreType::HashMap(other_hashmap) = other.core_ty() else {
            return None;
        };

        if self
            .key_ty
            .check_operator(
                &other_hashmap.key_ty,
                &BinaryOperatorKind::DoubleEqual,
                namespace,
            )
            .is_some()
            && self
                .value_ty
                .check_operator(
                    &other_hashmap.value_ty,
                    &BinaryOperatorKind::DoubleEqual,
                    namespace,
                )
                .is_some()
        {
            return Some(Type::new_with_atomic(BOOL));
        }
        None
    }

    fn check_greater(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_less(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_and(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }

    fn check_or(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
        None
    }
}

impl CollectionType for HashMap {
    fn concrete_types(&self) -> TurbofishTypes {
        TurbofishTypes::new(vec![self.key_ty.clone(), self.value_ty.clone()])
    }
}
