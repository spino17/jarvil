use crate::{
    core::string_interner::StrId,
    scope::{
        concrete::{ConcreteTypesTuple, ConcretizationContext},
        core::{AbstractConcreteTypesHandler, GenericTypeParams},
    },
    types::core::{AbstractType, Type},
};
use text_size::TextRange;

#[derive(Debug)]
pub struct EnumTypeData {
    pub variants: Vec<(StrId, Option<Type>, TextRange)>,
    pub generics: Option<GenericTypeParams>,
    pub is_init: bool,
}

impl EnumTypeData {
    pub fn set_meta_data(&mut self, variants: Vec<(StrId, Option<Type>, TextRange)>) {
        self.variants = variants;
    }

    pub fn set_generics(&mut self, generics_spec: Option<GenericTypeParams>) {
        self.generics = generics_spec;
        self.is_init = true;
    }

    pub fn try_type_for_variant<'a>(
        &'a self,
        variant_name: StrId,
        global_concrete_types: Option<&'a ConcreteTypesTuple>,
    ) -> Option<Option<Type>> {
        for (curr_variant_name, ty, _) in &self.variants {
            if *curr_variant_name == variant_name {
                match ty {
                    Some(ty) => {
                        if ty.is_concretization_required() {
                            return Some(Some(ty.concretize(&ConcretizationContext::new(
                                global_concrete_types,
                                None,
                            ))));
                        } else {
                            return Some(Some(ty.clone()));
                        }
                    }
                    None => return Some(None),
                }
            }
        }
        None
    }

    pub fn try_index_for_variant<'a>(&'a self, variant_name: StrId) -> Option<usize> {
        for (index, (curr_variant_name, _, _)) in self.variants.iter().enumerate() {
            if *curr_variant_name == variant_name {
                return Some(index);
            }
        }
        None
    }
}

impl AbstractConcreteTypesHandler for EnumTypeData {
    fn is_initialized(&self) -> bool {
        self.is_init
    }
}

impl Default for EnumTypeData {
    fn default() -> Self {
        EnumTypeData {
            variants: vec![],
            generics: Option::default(),
            is_init: false,
        }
    }
}
