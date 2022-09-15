use std::ptr::NonNull;

// Have a function which converts every value with type `Data` is converted into bytes or array of bytes
// hash function would simply take this array of bytes and spit out a index into the hash table array.

struct CoreDictObject {}

#[derive(Clone)]
pub struct ListObject(NonNull<CoreDictObject>);
