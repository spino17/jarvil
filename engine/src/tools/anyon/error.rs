pub struct AnyonError {
    msg: String,
}

impl AnyonError {
    pub fn new(err_msg: String) -> Self {
        AnyonError { msg: err_msg }
    }
}
