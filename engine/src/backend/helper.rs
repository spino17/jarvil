pub fn get_machine_byte_factor() -> usize {
    let byte_multiple = match usize::BITS {
        8 => 1,
        16 => 2,
        32 => 4,
        64 => 8,
        _ => unreachable!("size of `usize` should be among 8, 16, 32 and 64 bits"),
    };
    byte_multiple
}
