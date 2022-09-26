#[macro_use]
use jarvil_macros::OpCodeUtil;

use super::helper::get_machine_byte_multiple;
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
                   //POPN,  // pop n elements from the stack
}

// impl_opcode_display!(RETURN, PUSH_CONSTANT);

/*
impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            OpCode::RETURN => "RETURN",
            OpCode::PUSH_CONSTANT => "PUSH_CONSTANT",
            OpCode::UNARY_OP_MINUS => "UNARY_OP_MINUS",
            OpCode::BINARY_OP_ADD => "BINARY_OP_ADD",
            OpCode::BINARY_OP_SUBTRACT => "BINARY_OP_SUBTRACT",
            OpCode::BINARY_OP_MULTIPLY => "BINARY_OP_MULTIPLY",
            OpCode::BINARY_OP_DIVIDE => "BINARY_OP_DIVIDE",
            OpCode::PUSH_TRUE => "PUSH_TRUE",
            OpCode::PUSH_FALSE => "PUSH_FALSE",
            OpCode::UNARY_OP_NOT => "UNARY_OP_NOT",
            OpCode::BINARY_OP_DOUBLE_EQUAL => "BINARY_OP_DOUBLE_EQUAL",
            OpCode::BINARY_OP_NOT_EQUAL => "BINARY_OP_NOT_EQUAL",
            OpCode::BINARY_OP_GREATER => "BINARY_OP_GREATER",
            OpCode::BINARY_OP_GREATER_EQUAL => "BINARY_OP_GREATER_EQUAL",
            OpCode::BINARY_OP_LESS => "BINARY_OP_LESS",
            OpCode::BINARY_OP_LESS_EQUAL => "BINARY_OP_LESS_EQUAL",
        };
        write!(f, "{}", str)
    }
}
 */

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

    pub fn write_instruction(&mut self, op_code: OpCode, line_number: usize) {
        self.write_byte(op_code.to_byte(), line_number);
    }

    pub fn write_constant(&mut self, const_value: Data, line_number: usize) {
        let const_index = self.constants.len();
        self.constants.push(const_value);
        self.code.push(OpCode::PUSH_CONSTANT.to_byte());
        self.code.extend_from_slice(&const_index.to_be_bytes());
        self.line_numbers.push(line_number);
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

    pub fn disassemble_instruction(&self, offset: usize) -> (String, usize) {
        let op_code = &OP_CODES_MAP[usize::from(self.code[offset])];
        let op_code_str = op_code.to_string();
        match op_code {
            OpCode::RETURN => (op_code_str, offset + 1),
            OpCode::PUSH_CONSTANT => {
                let byte_multiple = get_machine_byte_multiple();
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
            //OpCode::POPN => ("POPN".to_string(), offset + 1),
        }
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
