pub struct ErrorMetaData {
    
}

pub trait ErrorFormatter {
    fn format(meta_data: ErrorMetaData) -> String;
}

pub struct TerminalBasedFormatter {}
impl ErrorFormatter for TerminalBasedFormatter {
    fn format(meta_data: ErrorMetaData) -> String {
        todo!()
    }
}