use std::cell::RefCell;
use rustc_hash::FxHashMap;
use std::rc::Rc;
use crate::types::core::{Type};
use crate::scope::function::FunctionData;

#[derive(Debug, Clone)]
pub enum UserDefinedTypeData {
    STRUCT(StructData),
    LAMBDA(LambdaData),
    // GENERIC(GenericType),
}

#[derive(Debug, Clone)]
pub struct StructData {
    pub name: Rc<String>,
    fields: Rc<FxHashMap<Rc<String>, Type>>,
    constructor: FunctionData,
    methods: Rc<RefCell<FxHashMap<Rc<String>, FunctionData>>>,
    class_methods: Rc<RefCell<FxHashMap<Rc<String>, FunctionData>>>,
}

#[derive(Debug, Clone)]
pub enum StructMethod {
    METHOD(FunctionData),
    CLASS_METHOD(FunctionData),
}

#[derive(Debug, Clone)]
pub struct LambdaData {
    pub name: Rc<String>,
    pub function_data: FunctionData,
}