use super::core::{CoreType, Type, TypeStringifyContext};
use super::traits::{OperatorCompatiblity, TypeLike};
use crate::scope::concrete::TypeGenericsInstantiationContext;
use crate::scope::traits::InstantiationContext;
use crate::types::traits::UserDefinedType;
use crate::{
    core::string_interner::IdentName,
    parser::type_checker::InferredConcreteTypesEntry,
    scope::{
        concrete::TurbofishTypes,
        namespace::Namespace,
        symbol::{
            core::SymbolIndex,
            interfaces::InterfaceBounds,
            types::{core::UserDefinedTypeData, generic_ty::GenericTypeDeclarationPlaceCategory},
        },
    },
};

#[derive(Debug)]
pub struct Generic {
    symbol_index: SymbolIndex<UserDefinedTypeData>,
}

impl Generic {
    pub fn new(symbol_index: SymbolIndex<UserDefinedTypeData>) -> Generic {
        Generic { symbol_index }
    }

    pub fn name(&self) -> IdentName {
        self.symbol_index.identifier_name()
    }
}

impl UserDefinedType for Generic {
    fn symbol_index(&self) -> SymbolIndex<UserDefinedTypeData> {
        self.symbol_index
    }

    fn concrete_types(&self) -> Option<&TurbofishTypes> {
        None
    }

    fn name(&self) -> IdentName {
        self.symbol_index.identifier_name()
    }
}

impl TypeLike for Generic {
    fn is_eq(&self, other_ty: &Type, _namespace: &Namespace) -> bool {
        let CoreType::Generic(generic_data) = other_ty.core_ty() else {
            return false;
        };

        self.name() == generic_data.name()
    }

    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        context: TypeGenericsInstantiationContext,
        namespace: &Namespace,
    ) -> bool {
        let is_other_ty_generic = match other_ty.core_ty() {
            CoreType::Generic(generic_data) => Some(generic_data),
            _ => None,
        };
        let self_ty_data = namespace
            .types_ref()
            .symbol_ref(self.symbol_index)
            .data_ref();
        let self_generic_data = self_ty_data.generic_data_ref();
        let self_index = self_generic_data.index();
        let self_category = self_generic_data.category();

        match self_category {
            GenericTypeDeclarationPlaceCategory::InCallable => match is_other_ty_generic {
                Some(generic_data) => {
                    let other_data = namespace
                        .types_ref()
                        .symbol_ref(generic_data.symbol_index)
                        .data_ref();
                    let other_generic_data = other_data.generic_data_ref();
                    let other_index = other_generic_data.index();
                    let other_category = other_generic_data.category();

                    match other_category {
                        GenericTypeDeclarationPlaceCategory::InCallable => {
                            self_index == other_index
                        }
                        GenericTypeDeclarationPlaceCategory::InType => false,
                    }
                }
                None => false,
            },
            GenericTypeDeclarationPlaceCategory::InType => match is_other_ty_generic {
                Some(_) => false,
                None => {
                    let concrete_types = match context.ty_generics_instantiation_args() {
                        Some(concrete_types) => concrete_types,
                        None => unreachable!(),
                    };
                    let concrete_self_ty = &concrete_types[self_index];

                    concrete_self_ty.is_eq(other_ty, namespace)
                }
            },
        }
    }

    fn concretize<'a, T: InstantiationContext<'a> + Copy>(
        &self,
        context: T,
        namespace: &Namespace,
    ) -> Type {
        let ty_data = namespace
            .types_ref()
            .symbol_ref(self.symbol_index)
            .data_ref();
        let generic_data = ty_data.generic_data_ref();
        let index = generic_data.index();
        let category = generic_data.category();

        match category {
            GenericTypeDeclarationPlaceCategory::InType => {
                match context.ty_generics_instantiation_args() {
                    Some(concrete_types) => concrete_types[index].clone(),
                    None => unreachable!(),
                }
            }
            GenericTypeDeclarationPlaceCategory::InCallable => {
                match context.callable_generics_instantiation_args() {
                    Some(concrete_types) => concrete_types[index].clone(),
                    None => unreachable!(),
                }
            }
        }
    }

    fn is_ty_bounded_by_interfaces(
        &self,
        interface_bounds: &InterfaceBounds,
        namespace: &Namespace,
    ) -> bool {
        let ty_data = namespace
            .types_ref()
            .symbol_ref(self.symbol_index)
            .data_ref();
        let ty_interface_bounds = ty_data.generic_data_ref().interface_bounds();

        interface_bounds.is_subset(ty_interface_bounds, namespace)
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
        let ty_data = namespace
            .types_ref()
            .symbol_ref(self.symbol_index)
            .data_ref();
        let generic_data_ref = ty_data.generic_data_ref();
        let index = generic_data_ref.index();
        let decl_place = generic_data_ref.category();

        if inference_category == decl_place {
            let entry_ty = &mut inferred_concrete_types[index];

            match entry_ty {
                InferredConcreteTypesEntry::Uninferred => {
                    *entry_ty = InferredConcreteTypesEntry::Inferred(received_ty.clone());
                    *num_inferred_types += 1;

                    Ok(())
                }
                InferredConcreteTypesEntry::Inferred(present_ty) => {
                    if !present_ty.is_eq(received_ty, namespace) {
                        return Err(());
                    }

                    Ok(())
                }
            }
        } else {
            debug_assert!(decl_place == GenericTypeDeclarationPlaceCategory::InType);
            let global_concrete_types = match global_concrete_types {
                Some(concrete_types) => concrete_types,
                None => unreachable!(),
            };
            let expected_ty = &global_concrete_types[index];

            if !expected_ty.is_eq(received_ty, namespace) {
                return Err(());
            }

            Ok(())
        }
    }

    fn to_string(&self, context: TypeStringifyContext) -> String {
        let ty_data = context
            .namespace()
            .types_ref()
            .symbol_ref(self.symbol_index)
            .data_ref();
        let generic_data = ty_data.generic_data_ref();
        let interface_bounds = generic_data.interface_bounds();

        format!(
            "{}{}",
            context.interner().lookup(self.name()),
            interface_bounds.to_string(context)
        )
    }
}

impl OperatorCompatiblity for Generic {
    // TODO - add implementations of below methods based on the interfaces bounding the generic type
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

    fn check_double_equal(&self, _other: &Type, _namespace: &Namespace) -> Option<Type> {
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
