
pub trait Visitor {
    fn visit(&self) -> Box<dyn Visitor>;
}