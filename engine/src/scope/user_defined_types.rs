use std::cell::RefCell;
use rustc_hash::FxHashMap;
use std::rc::Rc;
use crate::types::core::{Type};
use crate::scope::function::FunctionData;

#[derive(Debug)]
pub enum UserDefinedTypeData {
    STRUCT(Struct),
    LAMBDA(Lambda),
    // GENERIC(GenericType),
}

#[derive(Debug)]
pub struct Struct {
    pub name: Rc<String>,
    fields: Rc<FxHashMap<Rc<String>, Type>>,
    constructor: FunctionData,
    methods: Rc<RefCell<FxHashMap<Rc<String>, FunctionData>>>,
    class_methods: Rc<RefCell<FxHashMap<Rc<String>, FunctionData>>>,
}

#[derive(Debug)]
pub enum StructFunction {
    METHOD(FunctionData),
    CLASS_METHOD(FunctionData),
}

#[derive(Debug)]
pub struct Lambda {
    pub name: Rc<String>,
    pub function_data: FunctionData,
}