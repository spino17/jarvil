use super::{
    core::{AbstractType, CoreType, OperatorCompatiblity, Type},
    helper::{struct_enum_compare_fn, try_infer_types_from_tuple, StructEnumType},
};
use crate::{
    core::string_interner::{Interner, StrId},
    parser::type_checker::InferredConcreteTypesEntry,
    scope::{
        concrete::{ConcreteTypesTuple, ConcretizationContext},
        core::SymbolData,
        interfaces::InterfaceBounds,
        types::{core::UserDefinedTypeData, generic_type::GenericTypeDeclarationPlaceCategory},
    },
};

#[derive(Debug, Clone)]
pub struct Enum {
    pub symbol_data: SymbolData<UserDefinedTypeData>,
    pub concrete_types: Option<ConcreteTypesTuple>,
}

impl Enum {
    pub fn new(
        symbol_data: &SymbolData<UserDefinedTypeData>,
        concrete_types: Option<ConcreteTypesTuple>,
    ) -> Enum {
        Enum {
            symbol_data: symbol_data.clone(),
            concrete_types,
        }
    }
}

impl StructEnumType for Enum {
    fn get_concrete_types(&self) -> Option<&ConcreteTypesTuple> {
        self.concrete_types.as_ref()
    }

    fn get_name(&self) -> StrId {
        self.symbol_data.identifier_name()
    }
}

impl AbstractType for Enum {
    fn is_eq(&self, other_ty: &Type) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Enum(enum_data) => {
                let ty_cmp_func =
                    |ty1: &Type, ty2: &Type, _context: &ConcretizationContext| ty1.is_eq(ty2);
                struct_enum_compare_fn(
                    self,
                    enum_data,
                    ty_cmp_func,
                    &ConcretizationContext::default(),
                )
            }
            CoreType::Any => true,
            _ => false,
        }
    }

    fn is_structurally_eq(&self, other_ty: &Type, context: &ConcretizationContext) -> bool {
        let CoreType::Enum(enum_data) = other_ty.0.as_ref() else {
            return false;
        };
        let ty_cmp_func = |ty1: &Type, ty2: &Type, context: &ConcretizationContext| {
            ty1.is_structurally_eq(ty2, context)
        };
        struct_enum_compare_fn(self, enum_data, ty_cmp_func, context)
    }

    fn concretize(&self, context: &ConcretizationContext) -> Type {
        let Some(concrete_types) = &self.concrete_types else {
            return Type::new_with_enum(&self.symbol_data, None);
        };
        let mut concretized_concrete_types = vec![];
        for ty in concrete_types.iter() {
            concretized_concrete_types.push(ty.concretize(context));
        }
        Type::new_with_enum(
            &self.symbol_data,
            Some(ConcreteTypesTuple::new(concretized_concrete_types)),
        )
    }

    fn is_type_bounded_by_interfaces(&self, _interface_bounds: &InterfaceBounds) -> bool {
        unreachable!()
    }

    fn try_infer_type_or_check_equivalence(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&ConcreteTypesTuple>,
        num_inferred_types: &mut usize,
        inference_category: GenericTypeDeclarationPlaceCategory,
    ) -> Result<(), ()> {
        let CoreType::Enum(enum_ty) = received_ty.0.as_ref() else {
            return Err(());
        };
        if self.get_name() != enum_ty.get_name() {
            return Err(());
        }
        let Some(generics_containing_types_tuple) = &self.concrete_types else {
            return Ok(());
        };
        let base_types_tuple = match &enum_ty.concrete_types {
            Some(concrete_types) => concrete_types,
            None => unreachable!(),
        };
        try_infer_types_from_tuple(
            base_types_tuple.get_core_ref(),
            generics_containing_types_tuple.get_core_ref(),
            inferred_concrete_types,
            global_concrete_types,
            num_inferred_types,
            inference_category,
        )
    }

    fn to_string(&self, interner: &Interner) -> String {
        let mut s = interner.lookup(self.get_name()).to_string();
        let Some(concrete_types) = &self.concrete_types else {
            return s;
        };
        s.push('<');
        s.push_str(&concrete_types.to_string(interner));
        s.push('>');
        s
    }
}

impl OperatorCompatiblity for Enum {
    fn check_add(&self, _other: &Type) -> Option<Type> {
        None
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

    fn check_double_equal(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_greater(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_less(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_and(&self, _other: &Type) -> Option<Type> {
        None
    }

    fn check_or(&self, _other: &Type) -> Option<Type> {
        None
    }
}
