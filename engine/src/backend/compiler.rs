use crate::backend::{chunk::Chunk, object::core::ObjectTracker};

pub struct Compiler {
    object_tracker: ObjectTracker,
    pub chunk: Chunk,
    parent: Option<Box<Compiler>>,
}

impl Compiler {
    pub fn new(tracker: &ObjectTracker, parent: Option<Compiler>) -> Self {
        Compiler {
            object_tracker: tracker.clone(),
            chunk: Chunk::default(),
            parent: match parent {
                Some(p) => Some(Box::new(p)),
                None => None,
            },
        }
    }
}
