use core::num;
use std::{cell::RefCell, convert::TryInto, rc::Rc};

use crate::{
    ast::{
        ast::{ASTNode, BlockNode, OkFunctionDeclarationNode, StatementNode},
        walk::Visitor,
    },
    backend::{
        chunk::{Chunk, OpCode},
        compiler::Compiler,
        object::core::ObjectTracker,
    },
    parser::resolver::UpValue,
};

struct ByteCodeGenerator {
    compiler: Compiler,
    object_tracker: ObjectTracker,
}

impl ByteCodeGenerator {
    fn new() -> Self {
        let object_tracker = ObjectTracker::default();
        ByteCodeGenerator {
            compiler: Compiler::new(),
            object_tracker,
        }
    }

    fn compile(mut self, ast: &BlockNode) -> (Chunk, ObjectTracker) {
        let code_block = ast.0.as_ref().borrow();
        for stmt in &code_block.stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        (self.compiler.chunk(), self.object_tracker)
    }

    fn open_compiler(&mut self, upvalues: Rc<RefCell<Vec<UpValue>>>) {
        self.compiler = Compiler::new_with_parent(&self.compiler, upvalues);
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

    fn variable_decl_callback(&self, is_captured: bool) {
        self.compiler
            .0
            .as_ref()
            .borrow_mut()
            .variable_decl_callback(is_captured);
    }

    fn open_block(&self) {
        self.compiler.0.as_ref().borrow_mut().open_block();
    }

    fn close_block(&mut self) {
        let num_of_popped_elements = self.emit_pop_bytecode();
        self.compiler
            .0
            .as_ref()
            .borrow_mut()
            .close_block(num_of_popped_elements);
    }

    fn emit_bytecode(&mut self, op_code: OpCode, line_number: usize) {
        self.compiler
            .0
            .as_ref()
            .borrow_mut()
            .chunk
            .write_instruction(op_code, line_number);
    }

    fn emit_pop_bytecode(&mut self) -> usize {
        let compiler = self.compiler.0.as_ref().borrow();
        let len = compiler.locals.len();
        let curr_depth = compiler.depth();
        let mut num_of_popped_elements = 0;
        if len > 0 {
            let index = len - 1;
            while compiler.locals[index].depth == curr_depth {
                // TODO - check local at the index
                // generate OP_POP according to whether it's captured or not
                // override depth
                if compiler.locals[index].is_captured {
                    todo!()
                } else {
                    todo!()
                }
                num_of_popped_elements += 1;
                index -= 1;
            }
        }
        num_of_popped_elements
    }

    fn compile_block(&mut self, block: &BlockNode) {
        self.open_block();
        for stmt in &block.0.as_ref().borrow().stmts {
            self.walk_stmt_indent_wrapper(stmt);
        }
        self.close_block();
    }

    fn compile_stmt(&mut self, stmt: &StatementNode) {
        // TODO - make cases for all the stmts and compile them
        // TODO - as soon as we encounter a variable usage we check whether it's a local variable or an upvalue
        // depending on that we generate the LOAD/STORE instruction with appropiate index
        todo!()
    }

    fn compile_func_decl(&mut self, func_decl: &OkFunctionDeclarationNode) {
        let upvalues = func_decl
                                                    .context()
                                                    .expect(
                                                        "`context` in `CoreOkFunctionDeclarationNode` should be set after resolving first phase"
                                                    );
        let core_func_decl = func_decl.0.as_ref().borrow();
        self.open_compiler(upvalues);
        // TODO - walk on the statements
        let code = self.close_compiler();
        // close_compiler
        // make function object out of the chunk we get => if name is available then set the func_obj to symbol entry of the
        // function and if there is no name (in case of lambda assignment), just emit a bytecode to push the object on stack
        // and add it to the constant array in curr chunk
    }
}

impl Visitor for ByteCodeGenerator {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
        // TODO - catch all statements here.
        // TODO - catch `OkFunctionDeclarationNode` node here and surround that with open_compiler and close_compiler and call walk on block
        // TODO - for block keep track of how many local variables are there and decrement them as soon as block gets over.
        // TODO - for `VariableDeclarationNode` call variable_decl_callback and set the index returned from it to the symbol entry
        match node {
            ASTNode::STATEMENT(stmt) => {
                self.compile_stmt(stmt);
                return None;
            }
            ASTNode::BLOCK(block) => {
                self.compile_block(block);
                return None;
            }
            ASTNode::OK_FUNCTION_DECLARATION(func_decl) => {
                self.compile_func_decl(func_decl);
                return None;
            }
            _ => Some(()),
        }
    }
}
