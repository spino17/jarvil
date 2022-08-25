use crate::scope::function::FunctionData;
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
    constructor: FunctionData,
    methods: Rc<RefCell<FxHashMap<String, FunctionData>>>,
    class_methods: Rc<RefCell<FxHashMap<String, FunctionData>>>,
}

#[derive(Debug, Clone)]
pub struct LambdaTypeData {
    pub function_data: Option<FunctionData>,
}
