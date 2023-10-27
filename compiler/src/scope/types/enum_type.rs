use crate::{
    core::string_interner::StrId,
    scope::core::{AbstractConcreteTypesHandler, GenericTypeParams},
    types::core::Type,
};

pub struct EnumTypeData {
    variants: Vec<(StrId, Option<Type>)>,
    pub generics: Option<GenericTypeParams>,
    pub is_init: bool,
}

impl EnumTypeData {
    pub fn set_meta_data(&mut self, variants: Vec<(StrId, Option<Type>)>) {
        self.variants = variants;
    }

    pub fn set_generics(&mut self, generics_spec: Option<GenericTypeParams>) {
        self.generics = generics_spec;
        self.is_init = true;
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
