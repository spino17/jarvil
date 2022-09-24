use crate::{
    ast::{
        ast::{ASTNode, BlockNode},
        walk::Visitor,
    },
    backend::{
        chunk::{Chunk, OpCode},
        compiler::Compiler,
        object::core::ObjectTracker,
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
            compiler: Compiler::new(&object_tracker),
            object_tracker,
        }
    }

    fn compile(mut self, ast: &BlockNode) -> (Chunk, ObjectTracker) {
        self.walk_block(ast);
        (self.compiler.chunk(), self.object_tracker)
    }

    fn emit_bytecode(&mut self, op_code: OpCode, line_number: usize) {
        self.compiler.emit_bytecode(op_code, line_number);
    }

    fn open_compiler(&mut self) {
        // TODO - add new compiler and set the previous one as parent
        self.compiler = Compiler::new_with_parent(&self.object_tracker, &self.compiler);
    }

    fn close_compiler(&mut self) -> Chunk {
        let chunk = self.compiler.chunk();
        let parent_compiler = match &self.compiler.0.as_ref().borrow().parent {
            Some(parent) => parent.clone(),
            None => unreachable!("attempt to close compiler should not be done at global level"),
        };
        self.compiler = parent_compiler;
        chunk
    }
}

impl Visitor for ByteCodeGenerator {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        // TODO - catch all statements here.
        // TODO - catch `OkFunctionDeclarationNode` node here and surround that with open_compiler and close_compiler and call walk on block
        // TODO - for block keep track of how many local variables are there and decrement them as soon as block gets over.
        todo!()
    }
}
