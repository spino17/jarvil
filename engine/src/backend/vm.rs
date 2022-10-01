use super::{
    chunk::{Chunk, OpCode, OP_CODES_MAP},
    data::Data,
    helper::get_machine_byte_factor,
    object::core::ObjectTracker,
    operators::{eval_binary_op, eval_unary_op},
    stack::Stack,
};
use crate::lexer::token::{BinaryOperatorKind, UnaryOperatorKind};
use std::{convert::TryInto, fmt::Display};

pub enum InterpretResult {
    OK,
    COMPILE_ERROR, // receiving compiler error is a bug in the frontend!
    RUNTIME_ERROR,
}

pub struct VM {
    // TODO - add allocator also to be used to allocate and deallocate all the memory.
    pub object_tracker: ObjectTracker,
    pub chunk: Chunk, // will remove this once `ByteCodeGenerator` is in place
    ip: usize,        // `ip` points to the instruction about to be executed
    stack: Stack,
}

impl VM {
    pub fn new() -> Self {
        VM {
            object_tracker: ObjectTracker::default(), // TODO - take this from compiler unit
            chunk: Chunk::default(),
            ip: 0,
            stack: Stack::new(),
        }
    }

    pub fn advance_ip(&mut self) {
        self.ip = self.ip + 1;
    }

    pub fn read_constant(&mut self) -> Data {
        let byte_multiple = get_machine_byte_factor();
        let offset = self.ip;
        let v = self.chunk.code[offset..offset + byte_multiple]
            .try_into()
            .unwrap();
        let const_value = &self.chunk.constants[usize::from_be_bytes(v)];
        self.ip = offset + byte_multiple;
        const_value.clone()
    }

    pub fn run(&mut self) -> InterpretResult {
        // TODO - add a catch panic wrapper to encounter runtime errors
        // this loop mimicks the CPU cycle of `decode -> execute -> store -> fetch`
        loop {
            // println!("{}", self.chunk.disassemble_instruction(self.ip).0); // for debugging purposes
            match OP_CODES_MAP[usize::from(self.chunk.code[self.ip])] {
                OpCode::RETURN => {
                    self.advance_ip();
                    println!("{}", self.stack.pop());
                    return InterpretResult::OK;
                }
                OpCode::PUSH_CONSTANT => {
                    self.advance_ip();
                    let const_value = self.read_constant();
                    self.stack.push(const_value);
                }
                OpCode::PUSH_TRUE => {
                    self.advance_ip();
                    self.stack.push(Data::BOOL(true));
                }
                OpCode::PUSH_FALSE => {
                    self.advance_ip();
                    self.stack.push(Data::BOOL(false));
                }
                OpCode::UNARY_OP_MINUS => {
                    self.advance_ip();
                    let result = eval_unary_op(self.stack.pop(), UnaryOperatorKind::Minus);
                    self.stack.push(result)
                }
                OpCode::UNARY_OP_NOT => {
                    self.advance_ip();
                    let result = eval_unary_op(self.stack.pop(), UnaryOperatorKind::Not);
                    self.stack.push(result)
                }
                OpCode::BINARY_OP_ADD => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result = eval_binary_op(
                        l_data,
                        r_data,
                        BinaryOperatorKind::Add,
                        &mut self.object_tracker,
                    );
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_SUBTRACT => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result = eval_binary_op(
                        l_data,
                        r_data,
                        BinaryOperatorKind::Subtract,
                        &mut self.object_tracker,
                    );
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_MULTIPLY => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result = eval_binary_op(
                        l_data,
                        r_data,
                        BinaryOperatorKind::Multiply,
                        &mut self.object_tracker,
                    );
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_DIVIDE => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result = eval_binary_op(
                        l_data,
                        r_data,
                        BinaryOperatorKind::Divide,
                        &mut self.object_tracker,
                    );
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_DOUBLE_EQUAL => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result = eval_binary_op(
                        l_data,
                        r_data,
                        BinaryOperatorKind::DoubleEqual,
                        &mut self.object_tracker,
                    );
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_NOT_EQUAL => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result = eval_binary_op(
                        l_data,
                        r_data,
                        BinaryOperatorKind::NotEqual,
                        &mut self.object_tracker,
                    );
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_GREATER => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result = eval_binary_op(
                        l_data,
                        r_data,
                        BinaryOperatorKind::Greater,
                        &mut self.object_tracker,
                    );
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_GREATER_EQUAL => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result = eval_binary_op(
                        l_data,
                        r_data,
                        BinaryOperatorKind::GreaterEqual,
                        &mut self.object_tracker,
                    );
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_LESS => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result = eval_binary_op(
                        l_data,
                        r_data,
                        BinaryOperatorKind::Less,
                        &mut self.object_tracker,
                    );
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_LESS_EQUAL => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result = eval_binary_op(
                        l_data,
                        r_data,
                        BinaryOperatorKind::LessEqual,
                        &mut self.object_tracker,
                    );
                    self.stack.push(result);
                }
            }
        }
    }
}

impl Display for VM {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.object_tracker.to_string())
    }
}
