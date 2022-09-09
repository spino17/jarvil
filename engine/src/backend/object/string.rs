pub struct StringObject {
    len: usize,
    bytes: *mut u8, // an array of bytes
}
