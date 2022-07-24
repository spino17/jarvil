use crate::code::Code;

pub trait Formatter {
    fn format(code: &mut Code) -> String;
}