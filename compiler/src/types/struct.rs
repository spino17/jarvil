use super::core::{AbstractType, CoreType, OperatorCompatiblity, Type};
use super::helper::try_infer_types_from_tuple;
use crate::parser::type_checker::InferredConcreteTypesEntry;
use crate::scope::core::AbstractSymbolMetaData;
use crate::scope::types::generic_type::GenericTypeDeclarationPlaceCategory;
use crate::scope::{
    concrete::core::{ConcreteSymbolData, ConcreteTypesRegistryKey, ConcretizationContext},
    core::SymbolData,
    interfaces::InterfaceBounds,
    types::core::UserDefinedTypeData,
};

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub semantic_data: ConcreteSymbolData<UserDefinedTypeData>,
}

impl Struct {
    pub fn new(
        name: String,
        symbol_data: &SymbolData<UserDefinedTypeData>,
        index: Option<ConcreteTypesRegistryKey>,
    ) -> Struct {
        Struct {
            name,
            semantic_data: ConcreteSymbolData {
                symbol_data: symbol_data.clone(),
                index,
            },
        }
    }

    fn compare<F: Fn(&Type, &Type, &ConcretizationContext) -> bool>(
        &self,
        other: &Struct,
        ty_cmp_func: F,
        context: &ConcretizationContext,
    ) -> bool {
        if other.name.eq(&self.name) {
            match self.semantic_data.index {
                Some(self_key) => match other.semantic_data.index {
                    Some(other_key) => {
                        let self_symbol_data = self.semantic_data.get_core_ref();
                        let self_struct_data = self_symbol_data.get_struct_data_ref();
                        let self_concrete_types = &self_struct_data.get_concrete_types(self_key).0;
                        let self_len = self_concrete_types.len();

                        let other_symbol_data = other.semantic_data.get_core_ref();
                        let other_struct_data = other_symbol_data.get_struct_data_ref();
                        let other_concrete_types =
                            &other_struct_data.get_concrete_types(other_key).0;
                        let other_len = other_concrete_types.len();

                        assert!(self_len == other_len);
                        for i in 0..self_len {
                            if !ty_cmp_func(
                                &self_concrete_types[i],
                                &other_concrete_types[i],
                                context,
                            ) {
                                return false;
                            }
                        }
                        return true;
                    }
                    None => unreachable!(),
                },
                None => return true,
            }
        }
        return false;
    }
}

impl AbstractType for Struct {
    fn is_eq(&self, other_ty: &Type) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Struct(struct_data) => {
                let ty_cmp_func =
                    |ty1: &Type, ty2: &Type, _context: &ConcretizationContext| ty1.is_eq(ty2);
                self.compare(struct_data, ty_cmp_func, &ConcretizationContext::default())
            }
            CoreType::Any => true,
            _ => false,
        }
    }

    fn is_structurally_eq(&self, other_ty: &Type, context: &ConcretizationContext) -> bool {
        match other_ty.0.as_ref() {
            CoreType::Struct(struct_data) => {
                let ty_cmp_func = |ty1: &Type, ty2: &Type, context: &ConcretizationContext| {
                    ty1.is_structurally_eq(ty2, context)
                };
                self.compare(struct_data, ty_cmp_func, context)
            }
            _ => false,
        }
    }

    fn concretize(&self, context: &ConcretizationContext) -> Type {
        match self.semantic_data.index {
            Some(index) => {
                let symbol_data = self.semantic_data.get_core_ref();
                let struct_data = symbol_data.get_struct_data_ref();
                let concrete_types = &struct_data.get_concrete_types(index).0;
                let mut concretized_concrete_types = vec![];
                for ty in concrete_types {
                    concretized_concrete_types.push(ty.concretize(context));
                }
                let new_key = self
                    .semantic_data
                    .symbol_data
                    .register_concrete_types(Some(concretized_concrete_types), false);
                return Type::new_with_struct(
                    self.name.to_string(),
                    &self.semantic_data.symbol_data,
                    new_key,
                );
            }
            None => {
                return Type::new_with_struct(
                    self.name.to_string(),
                    &self.semantic_data.symbol_data,
                    None,
                )
            }
        }
    }

    fn is_type_bounded_by_interfaces(&self, interface_bounds: &InterfaceBounds) -> bool {
        let symbol_data = self.semantic_data.get_core_ref();
        match &symbol_data.get_struct_data_ref().implementing_interfaces {
            Some(ty_interface_bounds) => return interface_bounds.is_subset(ty_interface_bounds),
            None => return false,
        }
    }

    fn has_generics(&self) -> bool {
        let index = self.semantic_data.index;
        self.semantic_data
            .symbol_data
            .is_generics_present_in_tuple_at_index(index)
    }

    fn try_infer_type(
        &self,
        received_ty: &Type,
        inferred_concrete_types: &mut Vec<InferredConcreteTypesEntry>,
        global_concrete_types: Option<&Vec<Type>>,
        num_inferred_types: &mut usize,
        has_generics: &mut bool,
        inference_category: GenericTypeDeclarationPlaceCategory,
    ) -> Result<(), ()> {
        match received_ty.0.as_ref() {
            CoreType::Struct(struct_ty) => {
                if self.name == struct_ty.name {
                    match self.semantic_data.index {
                        Some(self_index) => {
                            let self_symbol_data = self.semantic_data.symbol_data.get_core_ref();
                            let self_struct_data = self_symbol_data.get_struct_data_ref();
                            let generics_containing_types_tuple =
                                &self_struct_data.get_concrete_types(self_index).0;

                            let other_index = struct_ty.semantic_data.index.unwrap();
                            let other_symbol_data =
                                struct_ty.semantic_data.symbol_data.get_core_ref();
                            let other_struct_data = other_symbol_data.get_struct_data_ref();
                            let base_types_tuple =
                                &other_struct_data.get_concrete_types(other_index).0;
                            try_infer_types_from_tuple(
                                base_types_tuple,
                                generics_containing_types_tuple,
                                inferred_concrete_types,
                                global_concrete_types,
                                num_inferred_types,
                                has_generics,
                                inference_category,
                            )
                        }
                        None => return Ok(()),
                    }
                } else {
                    return Err(());
                }
            }
            _ => Err(()),
        }
    }
}

impl ToString for Struct {
    fn to_string(&self) -> String {
        let mut s = self.name.to_string();
        match self.semantic_data.index {
            Some(index) => {
                s.push('<');
                let symbol_data = self.semantic_data.get_core_ref();
                let struct_data = symbol_data.get_struct_data_ref();
                let concrete_types = struct_data.get_concrete_types(index);
                s.push_str(&concrete_types.to_string());
                s.push('>');
                return s;
            }
            None => return s,
        }
    }
}

// TODO: operator compatiblity for struct types can be defined using interfaces
// This is called `operator-overloading`
impl OperatorCompatiblity for Struct {
    fn check_add(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `Add` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_subtract(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `Subtract` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_multiply(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `Multiply` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_divide(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `Divide` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_double_equal(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `Equal` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_greater(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `Greater` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_less(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `Less` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_and(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `And` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }

    fn check_or(&self, other: &Type) -> Option<Type> {
        match other.0.as_ref() {
            CoreType::Struct(other_struct) => {
                if self.name == other_struct.name {
                    // This will be replaced with checking whether struct implements `Or` interface
                    return None;
                } else {
                    return None;
                }
            }
            _ => None,
        }
    }
}
