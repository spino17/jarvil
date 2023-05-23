use std::fmt::Display;
pub struct AnyonError {
    msg: String,
}

impl AnyonError {
    pub fn new(err_msg: String) -> Self {
        AnyonError { msg: err_msg }
    }
}

impl Display for AnyonError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}
