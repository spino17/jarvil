use super::{
    chunk::{Chunk, OpCode, OP_CODES_MAP},
    helper::get_machine_byte_multiple,
    object::core::Data,
    stack::Stack,
};
use std::convert::TryInto;

pub enum InterpretResult {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
}

pub struct VM {
    chunk: Chunk,
    ip: usize, // `ip` points to the instruction about to be executed
    stack: Stack,
}
impl VM {
    pub fn new(chunk: Chunk) -> Self {
        VM {
            chunk,
            ip: 0,
            stack: Stack::new(),
        }
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
                OpCode::OP_RETURN => {
                    self.advance_ip();
                    println!("{}", self.stack.pop());
                    return InterpretResult::OK;
                }
                OpCode::OP_CONSTANT => {
                    self.advance_ip();
                    let const_value = self.read_constant();
                    self.stack.push(const_value);
                }
                OpCode::OP_TRUE => {
                    self.advance_ip();
                    self.stack.push(Data::BOOL(true));
                }
                OpCode::OP_FALSE => {
                    self.advance_ip();
                    self.stack.push(Data::BOOL(false));
                }
                OpCode::OP_NEGATE => {
                    self.advance_ip();
                    match self.stack.pop() {
                        Data::INT(val) => self.stack.push(Data::INT(-val)),
                        Data::FLOAT(val) => self.stack.push(Data::FLOAT(-val)),
                        _ => return InterpretResult::COMPILE_ERROR,
                    }
                }
                OpCode::OP_NOT => {
                    self.advance_ip();
                    match self.stack.pop() {
                        Data::BOOL(val) => self.stack.push(Data::BOOL(!val)),
                        _ => return InterpretResult::COMPILE_ERROR,
                    }
                }
                OpCode::OP_ADD => {
                    self.advance_ip();
                    decode_arithmetic_op!(+, self);
                }
                OpCode::OP_SUBTRACT => {
                    self.advance_ip();
                    decode_arithmetic_op!(-, self);
                }
                OpCode::OP_MULTIPLY => {
                    self.advance_ip();
                    decode_arithmetic_op!(*, self);
                }
                OpCode::OP_DIVIDE => {
                    self.advance_ip();
                    let r_val = match self.stack.pop() {
                        Data::INT(val) => val as f32,
                        Data::FLOAT(val) => val,
                        _ => return InterpretResult::COMPILE_ERROR,
                    };
                    let l_val = match self.stack.pop() {
                        Data::INT(val) => val as f32,
                        Data::FLOAT(val) => val,
                        _ => return InterpretResult::COMPILE_ERROR,
                    };
                    self.stack.push(Data::FLOAT(l_val / r_val));
                }
                OpCode::OP_EQUAL => {
                    self.advance_ip();
                    decode_equality_op!(==, self);
                }
                OpCode::OP_NOT_EQUAL => {
                    self.advance_ip();
                    decode_equality_op!(!=, self);
                }
                OpCode::OP_GREATER => {
                    self.advance_ip();
                    decode_comparison_op!(>, self);
                }
                OpCode::OP_GREATER_EQUAL => {
                    self.advance_ip();
                    decode_comparison_op!(>=, self);
                }
                OpCode::OP_LESS => {
                    self.advance_ip();
                    decode_comparison_op!(<, self);
                }
                OpCode::OP_LESS_EQUAL => {
                    self.advance_ip();
                    decode_comparison_op!(<=, self);
                }
            }
        }
    }
}
