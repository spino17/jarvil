use crate::types::core::Type;
use crate::types::r#struct::Struct;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

use super::function::FunctionData;

#[derive(Debug, Clone)]
pub enum UserDefinedTypeData {
    STRUCT(StructData),
    LAMBDA(LambdaTypeData),
    // GENERIC(GenericType),
}
impl UserDefinedTypeData {
    pub fn default_with_struct() -> Self {
        UserDefinedTypeData::STRUCT(StructData::default())
    }

    pub fn default_with_lambda() -> Self {
        UserDefinedTypeData::LAMBDA(LambdaTypeData::default())
    }
}

#[derive(Debug, Clone, Default)]
pub struct StructData {
    fields: Rc<FxHashMap<Rc<String>, Type>>,
    constructor: FunctionData,
    methods: Rc<RefCell<FxHashMap<String, FunctionData>>>,
    class_methods: Rc<RefCell<FxHashMap<String, FunctionData>>>,
}
impl StructData {
    pub fn set_fields(&mut self, fields: FxHashMap<Rc<String>, Type>) {
        self.fields = Rc::new(fields);
    }
}

#[derive(Debug, Clone, Default)]
pub struct LambdaTypeData {
    pub func_data: FunctionData,
}
