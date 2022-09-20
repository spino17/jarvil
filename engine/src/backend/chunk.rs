#[macro_use]
use jarvil_macros::OpCodeUtil;

use super::helper::get_machine_byte_multiple;
use crate::backend::data::Data;
use std::{convert::TryInto, fmt::Display};

#[derive(OpCodeUtil)]
pub enum OpCode {
    RETURN,        // 0
    PUSH_CONSTANT, // 1
    NEGATE,        // 2
    ADD,           // 3
    SUBTRACT,      // 4
    MULTIPLY,      // 5
    DIVIDE,        // 6
    TRUE,          // 7
    FALSE,         // 8
    NOT,           // 9
    EQUAL,         // 10
    NOT_EQUAL,     // 11
    GREATER,       // 12
    GREATER_EQUAL, // 13
    LESS,          // 14
    LESS_EQUAL,    // 15
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
        match OP_CODES_MAP[usize::from(self.code[offset])] {
            OpCode::RETURN => ("RETURN".to_string(), offset + 1),
            OpCode::PUSH_CONSTANT => {
                let byte_multiple = get_machine_byte_multiple();
                let v = self.code[offset + 1..offset + (byte_multiple + 1)]
                    .try_into()
                    .unwrap();
                let const_value = &self.constants[usize::from_be_bytes(v)];
                (
                    format!("CONSTANT {}", const_value),
                    offset + (byte_multiple + 1),
                )
            }
            OpCode::NEGATE => ("NEGATE".to_string(), offset + 1),
            OpCode::ADD => ("ADD".to_string(), offset + 1),
            OpCode::SUBTRACT => ("SUBTRACT".to_string(), offset + 1),
            OpCode::MULTIPLY => ("MULTIPLY".to_string(), offset + 1),
            OpCode::DIVIDE => ("DIVIDE".to_string(), offset + 1),
            OpCode::TRUE => ("TRUE".to_string(), offset + 1),
            OpCode::FALSE => ("FALSE".to_string(), offset + 1),
            OpCode::NOT => ("NOT".to_string(), offset + 1),
            OpCode::EQUAL => ("EQUAL".to_string(), offset + 1),
            OpCode::NOT_EQUAL => ("NOT_EQUAL".to_string(), offset + 1),
            OpCode::GREATER => ("GREATER".to_string(), offset + 1),
            OpCode::GREATER_EQUAL => ("GREATER_EQUAL".to_string(), offset + 1),
            OpCode::LESS => ("LESS".to_string(), offset + 1),
            OpCode::LESS_EQUAL => ("LESS_EQUAL".to_string(), offset + 1),
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
