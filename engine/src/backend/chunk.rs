#[macro_use]
use jarvil_macros::OpCodeUtil;

use super::helper::get_machine_byte_factor;
use crate::backend::data::Data;
use std::{convert::TryInto, fmt::Display};

// TOS: Top of Stack
// TOi: (i + 1)th entry from top of stack
#[derive(OpCodeUtil)]
pub enum OpCode {
    RETURN,
    PUSH_CONSTANT, // TOS = constants[index], where `index` is operand in the instruction => PUSH_CONSTANT index
    PUSH_TRUE,     // => TOS = True
    PUSH_FALSE,    // TOS = False
    POP,           // TOS
    LOAD_LOCAL, // TOS = stack[index], where `index` is operand in the instruction => LOAD_LOCAL index
    STORE_LOCAL, // stack[index] = TOS, where `index` is operand in the instruction => STORE_LOCAL index
    LOAD_UPVALUE, // TOS = frame.closure.upvalues[index], where `index` is operand in the instruction => LOAD_UPVALUE index
    STORE_UPVALUE, // *frame.closure.upvalues[index] = TOS, where `index` is operand in the instruction => STORE_UPVALUE index
    UNARY_OP_MINUS, // TOS = -TOS
    UNARY_OP_NOT,  // TOS = not TOS
    BINARY_OP_ADD, // TOS = TO1 + TOS
    BINARY_OP_SUBTRACT, // TOS = TO1 - TOS
    BINARY_OP_MULTIPLY, // TOS = TO1 * TOS
    BINARY_OP_DIVIDE, // TOS = TO1 / TOS
    BINARY_OP_DOUBLE_EQUAL, // TOS = (TO1 == TOS)
    BINARY_OP_NOT_EQUAL, // TOS = (TO1 != TOS)
    BINARY_OP_GREATER, // TOS = (TO1 > TOS)
    BINARY_OP_GREATER_EQUAL, // TOS = (TO1 >= TOS)
    BINARY_OP_LESS, // TOS = (TO1 < TOS)
    BINARY_OP_LESS_EQUAL, // TOS = (TO1 <= TOS)
}

pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Data>,
    pub line_numbers: Vec<usize>,
}

impl Chunk {
    pub fn write_byte(&mut self, byte: u8, line_number: usize) {
        self.code.push(byte);
        self.line_numbers.push(line_number);
    }

    pub fn read_byte(&self, offset: usize) -> usize {
        let v = self.code[offset];
        v.into()
    }

    pub fn read_usize(&self, offset: usize) -> usize {
        let byte_multiple = get_machine_byte_factor();
        let v = self.code[offset..offset + byte_multiple]
            .try_into()
            .unwrap();
        usize::from_be_bytes(v)
    }

    pub fn write_instruction(&mut self, op_code: OpCode, line_number: usize) {
        self.write_byte(op_code.to_byte(), line_number);
    }

    pub fn write_push_constant(&mut self, const_value: Data, line_number: usize) {
        let const_index = self.constants.len();
        self.constants.push(const_value);
        self.code.push(OpCode::PUSH_CONSTANT.to_byte());
        self.code.extend_from_slice(&const_index.to_be_bytes());
        self.line_numbers.push(line_number);
    }

    pub fn disassemble_instruction(&self, offset: usize) -> (String, usize) {
        let op_code = &OP_CODES_MAP[usize::from(self.code[offset])];
        let op_code_str = op_code.to_string();
        match op_code {
            OpCode::RETURN => (op_code_str, offset + 1),
            OpCode::PUSH_CONSTANT => {
                // instruction: PUSH_CONSTANT index
                // NOTE: `index` is usize
                let byte_multiple = get_machine_byte_factor();
                let v = self.code[offset + 1..offset + (byte_multiple + 1)]
                    .try_into()
                    .unwrap();
                let const_value = &self.constants[usize::from_be_bytes(v)];
                (
                    format!("{} {}", op_code_str, const_value),
                    offset + (byte_multiple + 1),
                )
            }
            OpCode::UNARY_OP_MINUS => (op_code_str, offset + 1),
            OpCode::BINARY_OP_ADD => (op_code_str, offset + 1),
            OpCode::BINARY_OP_SUBTRACT => (op_code_str, offset + 1),
            OpCode::BINARY_OP_MULTIPLY => (op_code_str, offset + 1),
            OpCode::BINARY_OP_DIVIDE => (op_code_str, offset + 1),
            OpCode::PUSH_TRUE => (op_code_str, offset + 1),
            OpCode::PUSH_FALSE => (op_code_str, offset + 1),
            OpCode::UNARY_OP_NOT => (op_code_str, offset + 1),
            OpCode::BINARY_OP_DOUBLE_EQUAL => (op_code_str, offset + 1),
            OpCode::BINARY_OP_NOT_EQUAL => (op_code_str, offset + 1),
            OpCode::BINARY_OP_GREATER => (op_code_str, offset + 1),
            OpCode::BINARY_OP_GREATER_EQUAL => (op_code_str, offset + 1),
            OpCode::BINARY_OP_LESS => (op_code_str, offset + 1),
            OpCode::BINARY_OP_LESS_EQUAL => (op_code_str, offset + 1),
            OpCode::POP => (op_code_str, offset + 1),
            OpCode::LOAD_LOCAL => {
                // instruction: LOAD_LOCAL index
                // NOTE: `index` is u8
                (
                    format!("{} {}", op_code_str, self.read_byte(offset + 1)),
                    offset + 2,
                )
            }
            OpCode::STORE_LOCAL => {
                // instruction: STORE_LOCAL index
                // NOTE: `index` is u8
                (
                    format!("{} {}", op_code_str, self.read_byte(offset + 1)),
                    offset + 2,
                )
            }
            OpCode::LOAD_UPVALUE => {
                // instruction: LOAD_UPVALUE index
                // NOTE: `index` is u8
                (
                    format!("{} {}", op_code_str, self.read_byte(offset + 1)),
                    offset + 2,
                )
            }
            OpCode::STORE_UPVALUE => {
                // instruction: STORE_UPVALUE index
                // NOTE: `index` is u8
                (
                    format!("{} {}", op_code_str, self.read_byte(offset + 1)),
                    offset + 2,
                )
            }
        }
    }

    pub fn disassemble(&self) -> Vec<String> {
        let mut offset = 0;
        let mut parsed_instructions: Vec<String> = vec![];
        let mut inst_index = 0;
        while offset < self.code.len() {
            let (str_rep, new_offset) = self.disassemble_instruction(offset);
            let mut inst_str = format!("{}: ", self.line_numbers[inst_index]);
            inst_str.push_str(&str_rep);
            parsed_instructions.push(inst_str);
            offset = new_offset;
            inst_index = inst_index + 1;
        }
        parsed_instructions
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut display_str = "".to_string();
        let inst_vec = self.disassemble();
        for inst in inst_vec {
            display_str.push_str("\n");
            display_str.push_str(&inst);
        }
        write!(f, "{}", display_str)
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Chunk {
            code: vec![],
            constants: vec![],
            line_numbers: vec![],
        }
    }
}
