use crate::{
    ast::{
        ast::{ASTNode, BlockNode},
        walk::Visitor,
    },
    backend::{
        chunk::Chunk,
        compiler::Compiler,
        object::{core::ObjectTracker, function::FunctionObject},
    },
};

struct ByteCodeGenerator {
    compiler: Compiler,
    object_tracker: ObjectTracker,
}

impl ByteCodeGenerator {
    fn new() -> Self {
        let object_tracker = ObjectTracker::default();
        ByteCodeGenerator {
            compiler: Compiler::new(&object_tracker, None),
            object_tracker,
        }
    }

    fn generate_bytecode(mut self, ast: &BlockNode) -> (Chunk, ObjectTracker) {
        // generate bytecode for ast
        (self.compiler.chunk, self.object_tracker)
    }

    fn open_compiler(&mut self) {
        // TODO - add new compiler and set the previous one as parent
    }

    fn close_compiler(&mut self) -> Chunk {
        // TODO - set the parent compiler as the current compiler
        todo!()
    }
}

impl Visitor for ByteCodeGenerator {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        // TODO - catch `OkFunctionDeclarationNode` node here
        todo!()
    }
}
