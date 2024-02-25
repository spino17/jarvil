use crate::{
    constants::common::BOOL,
    core::string_interner::Interner,
    lexer::token::BinaryOperatorKind,
    parser::type_checker::InferredConcreteTypesEntry,
    scope::{
        concrete::{ConcreteTypesTuple, ConcretizationContext},
        namespace::Namespace,
        symbol::{
            interfaces::InterfaceBounds, types::generic_type::GenericTypeDeclarationPlaceCategory,
        },
    },
    types::{
        core::{CoreType, Type},
        traits::{CollectionType, OperatorCompatiblity, TypeLike},
    },
};

#[derive(Debug, Clone)]
pub struct HashMap {
    key_type: Type,
    value_type: Type,
}

impl HashMap {
    pub fn new(key_type: Type, value_type: Type) -> HashMap {
        HashMap {
            key_type,
            value_type,
        }
    }

    pub fn key_ty(&self) -> &Type {
        &self.key_type
    }

    pub fn value_ty(&self) -> &Type {
        &self.value_type
    }
}

impl TypeLike for HashMap {
    fn is_eq(&self, other_ty: &Type, namespace: &Namespace) -> bool {
        match other_ty.core_ty() {
            CoreType::HashMap(hashmap_data) => {
                self.key_type.is_eq(&hashmap_data.key_type, namespace)
                    && self.value_type.is_eq(&hashmap_data.value_type, namespace)
            }
            CoreType::Any => true,
            _ => false,
        }
    }

    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        context: &ConcretizationContext,
        namespace: &Namespace,
    ) -> bool {
        let CoreType::HashMap(hashmap_data) = other_ty.core_ty() else {
            return false;
        };
        self.key_type
            .is_structurally_eq(&hashmap_data.key_type, context, namespace)
            && self
                .value_type
                .is_structurally_eq(&hashmap_data.value_type, context, namespace)
    }

    fn concretize(&self, context: &ConcretizationContext, namespace: &Namespace) -> Type {
        let concrete_key_ty = self.key_type.concretize(context, namespace);
        let concrete_value_ty = self.value_type.concretize(context, namespace);
        Type::new_with_hashmap(concrete_key_ty, concrete_value_ty)
    }

    fn is_type_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        _namespace: &Namespace,
    ) -> bool {
        // TODO - add checks for interfaces which `HashMap` would implement like `Iterator`, `Index`
        interface_bounds.len() == 0
    }

    fn try_infer_type_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&ConcreteTypesTuple>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
        namespace: &Namespace,
    ) -> Result<(), ()> {
        let CoreType::HashMap(hashmap_ty) = received_ty.core_ty() else {
            return Err(());
        };
        self.key_type.try_infer_type_or_check_equivalence(
            &hashmap_ty.key_type,
            inferred_concrete_types,
            global_concrete_types,
            num_inferred_types,
            inference_category,
            namespace,
        )?;
        self.value_type.try_infer_type_or_check_equivalence(
            &hashmap_ty.value_type,
            inferred_concrete_types,
            global_concrete_types,
            num_inferred_types,
            inference_category,
            namespace,
        )?;
        Ok(())
    }

    fn to_string(&self, interner: &Interner, namespace: &Namespace) -> String {
        format!(
            "{{{} : {}}}",
            self.key_type.to_string(interner, namespace),
            self.value_type.to_string(interner, namespace)
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
            .key_type
            .check_operator(
                &other_hashmap.key_type,
                &BinaryOperatorKind::DoubleEqual,
                namespace,
            )
            .is_some()
            && self
                .value_type
                .check_operator(
                    &other_hashmap.value_type,
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
    fn concrete_types(&self) -> ConcreteTypesTuple {
        ConcreteTypesTuple::new(vec![self.key_type.clone(), self.value_type.clone()])
    }
}
