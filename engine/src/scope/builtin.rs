use super::function::FunctionData;
use crate::types::core::Type;
use std::rc::Rc;

// print(obj: <any>)
pub fn print_meta_data() -> FunctionData {
    FunctionData {
        params: Rc::new(vec![(Rc::new("obj".to_string()), Type::new_with_any())]),
        return_type: Type::new_with_void(),
    }
}
