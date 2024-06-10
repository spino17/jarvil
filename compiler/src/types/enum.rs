use super::{
    core::{CoreType, Type, TypeStringifyContext},
    helper::{try_infer_types_from_tuple, user_defined_ty_compare_fn},
    traits::{OperatorCompatiblity, TypeLike, UserDefinedType},
};
use crate::{
    core::string_interner::IdentName,
    parser::type_checker::InferredConcreteTypesEntry,
    scope::{
        concrete::{TurbofishTypes, TypeGenericsInstantiationContext},
        namespace::Namespace,
        symbol::{
            core::SymbolIndex,
            interfaces::InterfaceBounds,
            types::{core::UserDefinedTypeData, generic_ty::GenericTypeDeclarationPlaceCategory},
        },
        traits::InstantiationContext,
    },
};

#[derive(Debug)]
pub struct Enum {
    symbol_index: SymbolIndex<UserDefinedTypeData>,
    concrete_types: Option<TurbofishTypes>,
}

impl Enum {
    pub fn new(
        symbol_index: SymbolIndex<UserDefinedTypeData>,
        concrete_types: Option<TurbofishTypes>,
    ) -> Enum {
        Enum {
            symbol_index,
            concrete_types,
        }
    }
}

impl UserDefinedType for Enum {
    fn symbol_index(&self) -> SymbolIndex<UserDefinedTypeData> {
        self.symbol_index
    }

    fn concrete_types(&self) -> Option<&TurbofishTypes> {
        self.concrete_types.as_ref()
    }

    fn name(&self) -> IdentName {
        self.symbol_index.identifier_name()
    }
}

impl TypeLike for Enum {
    fn is_eq(&self, other_ty: &Type, namespace: &Namespace) -> bool {
        let CoreType::Enum(enum_data) = other_ty.core_ty() else {
            return false;
        };
        let ty_cmp_func =
            |ty1: &Type,
             ty2: &Type,
             _context: TypeGenericsInstantiationContext,
             namespace: &Namespace| { ty1.is_eq(ty2, namespace) };

        user_defined_ty_compare_fn(
            self,
            enum_data,
            ty_cmp_func,
            TypeGenericsInstantiationContext::default(),
            namespace,
        )
    }

    fn is_structurally_eq(
        &self,
        other_ty: &Type,
        context: TypeGenericsInstantiationContext,
        namespace: &Namespace,
    ) -> bool {
        let CoreType::Enum(enum_data) = other_ty.core_ty() else {
            return false;
        };
        let ty_cmp_func =
            |ty1: &Type,
             ty2: &Type,
             context: TypeGenericsInstantiationContext,
             namespace: &Namespace| { ty1.is_structurally_eq(ty2, context, namespace) };

        user_defined_ty_compare_fn(self, enum_data, ty_cmp_func, context, namespace)
    }

    fn concretize<'a, T: InstantiationContext<'a> + Copy>(
        &self,
        context: T,
        namespace: &Namespace,
    ) -> Type {
        let Some(concrete_types) = &self.concrete_types else {
            return Type::new_with_enum(self.symbol_index, None);
        };
        let mut concretized_concrete_types = vec![];

        for ty in concrete_types.iter() {
            concretized_concrete_types.push(ty.concretize(context, namespace));
        }

        Type::new_with_enum(
            self.symbol_index,
            Some(TurbofishTypes::new(concretized_concrete_types)),
        )
    }

    fn is_ty_bounded_by_interfaces(
        &self,
        _interface_bounds: &InterfaceBounds,
        _namespace: &Namespace,
    ) -> bool {
        unreachable!()
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
        let CoreType::Enum(enum_ty) = received_ty.core_ty() else {
            return Err(());
        };

        if self.name() != enum_ty.name() {
            return Err(());
        }

        let Some(generics_containing_types_tuple) = &self.concrete_types else {
            return Ok(());
        };
        let Some(base_types_tuple) = &enum_ty.concrete_types else {
            unreachable!()
        };

        try_infer_types_from_tuple(
            base_types_tuple.core_ref(),
            generics_containing_types_tuple.core_ref(),
            inferred_concrete_types,
            global_concrete_types,
            num_inferred_types,
            inference_category,
            namespace,
        )
    }

    fn to_string(&self, context: TypeStringifyContext) -> String {
        let mut s = context.interner().lookup(self.name());
        let Some(concrete_types) = &self.concrete_types else {
            return s;
        };

        s.push('<');
        s.push_str(&concrete_types.to_string(context));
        s.push('>');
        s
    }
}

impl OperatorCompatiblity for Enum {
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
