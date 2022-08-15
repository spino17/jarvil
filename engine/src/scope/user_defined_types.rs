use crate::scope::function::FunctionData;
use crate::types::core::Type;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum UserDefinedTypeData {
    STRUCT(StructData),
    LAMBDA(LambdaData),
    // GENERIC(GenericType),
}

#[derive(Debug, Clone)]
pub struct StructData {
    pub name: String,
    fields: Rc<FxHashMap<String, Type>>,
    constructor: FunctionData,
    methods: Rc<RefCell<FxHashMap<String, FunctionData>>>,
    class_methods: Rc<RefCell<FxHashMap<String, FunctionData>>>,
}

#[derive(Debug, Clone)]
pub enum StructMethod {
    METHOD(FunctionData),
    CLASS_METHOD(FunctionData),
}

#[derive(Debug, Clone)]
pub struct LambdaData {
    pub name: String,
    pub function_data: FunctionData,
}
