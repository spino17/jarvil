use core::num;
use std::{cell::RefCell, convert::TryInto, rc::Rc};

use crate::{
    ast::{
        ast::{
            ASTNode, AssignmentNode, BlockNode, CoreFunctionDeclarationNode, CoreIdentifierNode,
            CoreLambdaDeclarationNode, CoreRAssignmentNode, CoreStatementNode, ExpressionNode,
            ExpressionStatementNode, LambdaDeclarationNode, Node, OkFunctionDeclarationNode,
            RAssignmentNode, ReturnStatementNode, StatementNode, TypeDeclarationNode,
            VariableDeclarationNode,
        },
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

    fn close_block(&mut self, line_number: usize) {
        let num_of_popped_elements = self.emit_block_close_bytecode(line_number); // emit bytecode for popping all the local variables declared in the block
        self.compiler
            .0
            .as_ref()
            .borrow_mut()
            .close_block(num_of_popped_elements); // then decrement the depth and drop all the elements belonging to the above variables
    }

    fn emit_bytecode(&mut self, op_code: OpCode, line_number: usize) {
        self.compiler
            .0
            .as_ref()
            .borrow_mut()
            .chunk
            .write_instruction(op_code, line_number);
    }

    fn emit_block_close_bytecode(&mut self, line_number: usize) -> usize {
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
        self.close_block(block.end_class_line_number());
    }

    fn compile_stmt(&mut self, stmt: &StatementNode) {
        // TODO - make cases for all the stmts and compile them
        // TODO - as soon as we encounter a variable usage we check whether it's a local variable or an upvalue
        // depending on that we generate the LOAD/STORE instruction with appropiate index
        match stmt.core_ref() {
            CoreStatementNode::EXPRESSION(expr_stmt) => self.compile_expression(expr_stmt),
            CoreStatementNode::ASSIGNMENT(assignment) => self.compile_assignment(assignment),
            CoreStatementNode::VARIABLE_DECLARATION(variable_decl) => {
                self.compile_variable_decl(variable_decl)
            }
            CoreStatementNode::FUNCTION_DECLARATION(func_decl) => match func_decl.core_ref() {
                CoreFunctionDeclarationNode::OK(ok_func_decl) => {
                    self.compile_func_decl(ok_func_decl)
                }
                CoreFunctionDeclarationNode::MISSING_TOKENS(_) => {
                    unreachable!("`MISSING_TOKENS` variant is not allowed uptill compiling phase")
                }
            },
            CoreStatementNode::TYPE_DECLARATION(type_decl) => self.compile_type_decl(type_decl),
            CoreStatementNode::STRUCT_STATEMENT(_) => return,
            CoreStatementNode::RETURN(return_stmt) => self.compile_return_stmt(return_stmt),
            CoreStatementNode::MISSING_TOKENS(_) => {
                unreachable!("`MISSING_TOKENS` variant is not allowed uptill compiling phase")
            }
        }
    }

    fn compile_expression(&mut self, expr: &ExpressionStatementNode) {
        todo!()
    }

    fn compile_assignment(&mut self, assignment: &AssignmentNode) {
        // TODO - get the symbol entry from left side starting identifier
        // check whether it's a local variable or an upvalue and generate the bytecode accordingly using the index information.
        // set the value equal to the top of stack
        todo!()
    }

    fn compile_variable_decl(&mut self, variable_decl: &VariableDeclarationNode) {
        let is_captured = match variable_decl.core_ref().name.core_ref() {
            CoreIdentifierNode::OK(ok_identifier) => {
                match ok_identifier.variable_symbol_data(
                    "param name should be resolved to `SymbolData<VariableData>`",
                ) {
                    Some(symbol_data) => symbol_data.0.as_ref().borrow().is_captured,
                    None => unreachable!("each identifier should already be resolved"),
                }
            }
            _ => unreachable!(
                "`MISSING_TOKENS` and `SKIPPED` variant is not allowed uptill compiling phase"
            ),
        };
        self.variable_decl_callback(is_captured);
        self.compile_r_assign(&variable_decl.core_ref().r_assign);
    }

    fn compile_r_assign(&mut self, r_assign: &RAssignmentNode) {
        // either compile expression or compile closure and push the object on stack
        match r_assign.core_ref() {
            CoreRAssignmentNode::LAMBDA(lambda_decl) => match lambda_decl.core_ref() {
                CoreFunctionDeclarationNode::OK(ok_lambda_decl) => {
                    self.compile_func_decl(ok_lambda_decl)
                }
                CoreFunctionDeclarationNode::MISSING_TOKENS(_) => {
                    unreachable!("`MISSING_TOKENS` variant is not allowed uptill compiling phase")
                }
            },
            CoreRAssignmentNode::EXPRESSION(expr_stmt) => self.compile_expression(expr_stmt),
            CoreRAssignmentNode::MISSING_TOKENS(_) => {
                unreachable!("`MISSING_TOKENS` variant is not allowed uptill compiling phase")
            }
        }
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

    fn compile_type_decl(&mut self, type_decl: &TypeDeclarationNode) {
        todo!()
    }

    fn compile_return_stmt(&mut self, return_stmt: &ReturnStatementNode) {
        todo!()
    }
}

impl Visitor for ByteCodeGenerator {
    fn visit(&mut self, node: &ASTNode) -> Option<()> {
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
