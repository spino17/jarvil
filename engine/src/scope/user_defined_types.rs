use crate::scope::function::CoreFunctionData;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum UserDefinedTypeData {
    STRUCT(Option<StructData>),
    LAMBDA(Option<LambdaTypeData>),
    // GENERIC(GenericType),
}

#[derive(Debug, Clone)]
pub struct StructData {
    fields: Rc<FxHashMap<String, Type>>,
    constructor: CoreFunctionData,
    methods: Rc<RefCell<FxHashMap<String, CoreFunctionData>>>,
    class_methods: Rc<RefCell<FxHashMap<String, CoreFunctionData>>>,
}

#[derive(Debug, Clone)]
pub struct LambdaTypeData {
    pub func_data: CoreFunctionData,
}
