use std::ptr::NonNull;

struct CoreDictObject {}

#[derive(Clone)]
pub struct ListObject(NonNull<CoreDictObject>);
