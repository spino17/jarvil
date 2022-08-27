use crate::scope::{core::SymbolData, user_defined_types::UserDefinedTypeData};
use super::core::{AbstractType, Type, CoreType};

#[derive(Debug)]
pub struct Struct {
    pub name: String,
    pub symbol_data: SymbolData<UserDefinedTypeData>,
}
impl Struct {
    pub fn new(name: String, symbol_data: &SymbolData<UserDefinedTypeData>) -> Struct {
        Struct {
            name,
            symbol_data: symbol_data.clone(),
        }
    } 
}
impl AbstractType for Struct {
    fn is_eq(&self, base_type: &Type) -> bool {
        match base_type.0.as_ref() {
            CoreType::STRUCT(struct_data) => {
                struct_data.name.eq(&self.name)
            },
            _ => false
        }
    }
}
impl ToString for Struct {
    fn to_string(&self) -> String {
        self.name.clone()
    }
}