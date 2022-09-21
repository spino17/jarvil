use super::{
    chunk::{Chunk, OpCode, OP_CODES_MAP},
    data::Data,
    helper::get_machine_byte_multiple,
    object::core::{CoreObject, Object},
    operators::{eval_binary_op, eval_unary_op},
    stack::Stack,
};
use crate::lexer::token::{BinaryOperatorKind, UnaryOperatorKind};
use std::{convert::TryInto, fmt::Display, ptr::NonNull};

pub enum InterpretResult {
    OK,
    COMPILE_ERROR, // receiving compiler error is a bug in the frontend!
    RUNTIME_ERROR,
}

pub struct VM {
    // TODO - add allocator also to be used to allocate and deallocate all the memory.
    objects: NonNull<Object>,
    objects_len: usize,
    pub chunk: Chunk, // will remove this once `ByteCodeGenerator` is in place
    ip: usize,        // `ip` points to the instruction about to be executed
    stack: Stack,
}

impl VM {
    pub fn new() -> Self {
        VM {
            objects: NonNull::dangling(),
            objects_len: 0,
            chunk: Chunk::default(),
            ip: 0,
            stack: Stack::new(),
        }
    }

    pub fn set_object(&mut self, core_object: CoreObject) -> Object {
        let obj = if self.objects_len == 0 {
            Object {
                core: core_object,
                next: None,
            }
        } else {
            let ptr = self.objects.clone();
            Object {
                core: core_object,
                next: Some(ptr),
            }
        };
        self.objects = unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(obj.clone()))) };
        self.objects_len = self.objects_len + 1;
        //println!("allocated: {}", obj);
        obj
    }

    pub fn advance_ip(&mut self) {
        self.ip = self.ip + 1;
    }

    pub fn read_constant(&mut self) -> Data {
        let byte_multiple = get_machine_byte_multiple();
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
                    let result = eval_binary_op(l_data, r_data, BinaryOperatorKind::Add, self);
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_SUBTRACT => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result = eval_binary_op(l_data, r_data, BinaryOperatorKind::Subtract, self);
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_MULTIPLY => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result = eval_binary_op(l_data, r_data, BinaryOperatorKind::Multiply, self);
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_DIVIDE => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result = eval_binary_op(l_data, r_data, BinaryOperatorKind::Divide, self);
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_EQUAL => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result =
                        eval_binary_op(l_data, r_data, BinaryOperatorKind::DoubleEqual, self);
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_NOT_EQUAL => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result = eval_binary_op(l_data, r_data, BinaryOperatorKind::NotEqual, self);
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_GREATER => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result = eval_binary_op(l_data, r_data, BinaryOperatorKind::Greater, self);
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_GREATER_EQUAL => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result =
                        eval_binary_op(l_data, r_data, BinaryOperatorKind::GreaterEqual, self);
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_LESS => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result = eval_binary_op(l_data, r_data, BinaryOperatorKind::Less, self);
                    self.stack.push(result);
                }
                OpCode::BINARY_OP_LESS_EQUAL => {
                    self.advance_ip();
                    let r_data = self.stack.pop();
                    let l_data = self.stack.pop();
                    let result =
                        eval_binary_op(l_data, r_data, BinaryOperatorKind::LessEqual, self);
                    self.stack.push(result);
                }
            }
        }
    }
}

impl Drop for VM {
    fn drop(&mut self) {
        // free all the remaining objects
        if self.objects_len != 0 {
            unsafe {
                let mut next = Some(self.objects.clone());
                while let Some(ptr) = next {
                    next = (*ptr.as_ptr()).next;
                    (&*ptr.as_ptr()).inner_drop();
                    let _x = Box::from_raw(ptr.as_ptr());
                }
            }
        }
    }
}

impl Display for VM {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = "".to_string();
        if self.objects_len != 0 {
            unsafe {
                let mut next = Some(self.objects.clone());
                let mut flag = false;
                while let Some(ptr) = next {
                    next = (*ptr.as_ptr()).next;
                    if flag {
                        s.push_str(" -> ");
                    }
                    s.push_str(&(*ptr.as_ptr()).to_string());
                    flag = true;
                }
            }
        }
        write!(f, "{}", s)
    }
}
