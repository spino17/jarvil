use crate::code::Code;

pub trait Formatter {
    fn format(code: &Code) -> String;
}