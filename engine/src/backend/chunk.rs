use super::helper::get_machine_byte_multiple;
use crate::backend::object::data::Data;
use std::{convert::TryInto, fmt::Display};

pub enum OpCode {
    OP_RETURN,        // 0
    OP_CONSTANT,      // 1
    OP_NEGATE,        // 2
    OP_ADD,           // 3
    OP_SUBTRACT,      // 4
    OP_MULTIPLY,      // 5
    OP_DIVIDE,        // 6
    OP_TRUE,          // 7
    OP_FALSE,         // 8
    OP_NOT,           // 9
    OP_EQUAL,         // 10
    OP_NOT_EQUAL,     // 11
    OP_GREATER,       // 12
    OP_GREATER_EQUAL, // 13
    OP_LESS,          // 14
    OP_LESS_EQUAL,    // 15
}

impl OpCode {
    pub fn to_byte(&self) -> u8 {
        match self {
            OpCode::OP_RETURN => 0,
            OpCode::OP_CONSTANT => 1,
            OpCode::OP_NEGATE => 2,
            OpCode::OP_ADD => 3,
            OpCode::OP_SUBTRACT => 4,
            OpCode::OP_MULTIPLY => 5,
            OpCode::OP_DIVIDE => 6,
            OpCode::OP_TRUE => 7,
            OpCode::OP_FALSE => 8,
            OpCode::OP_NOT => 9,
            OpCode::OP_EQUAL => 10,
            OpCode::OP_NOT_EQUAL => 11,
            OpCode::OP_GREATER => 12,
            OpCode::OP_GREATER_EQUAL => 13,
            OpCode::OP_LESS => 14,
            OpCode::OP_LESS_EQUAL => 15,
        }
    }
}

pub const OP_CODES_MAP: [OpCode; 16] = [
    OpCode::OP_RETURN,
    OpCode::OP_CONSTANT,
    OpCode::OP_NEGATE,
    OpCode::OP_ADD,
    OpCode::OP_SUBTRACT,
    OpCode::OP_MULTIPLY,
    OpCode::OP_DIVIDE,
    OpCode::OP_TRUE,
    OpCode::OP_FALSE,
    OpCode::OP_NOT,
    OpCode::OP_EQUAL,
    OpCode::OP_NOT_EQUAL,
    OpCode::OP_GREATER,
    OpCode::OP_GREATER_EQUAL,
    OpCode::OP_LESS,
    OpCode::OP_LESS_EQUAL,
];

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
        self.code.push(OpCode::OP_CONSTANT.to_byte());
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
            OpCode::OP_RETURN => ("RETURN".to_string(), offset + 1),
            OpCode::OP_CONSTANT => {
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
            OpCode::OP_NEGATE => ("NEGATE".to_string(), offset + 1),
            OpCode::OP_ADD => ("ADD".to_string(), offset + 1),
            OpCode::OP_SUBTRACT => ("SUBTRACT".to_string(), offset + 1),
            OpCode::OP_MULTIPLY => ("MULTIPLY".to_string(), offset + 1),
            OpCode::OP_DIVIDE => ("DIVIDE".to_string(), offset + 1),
            OpCode::OP_TRUE => ("TRUE".to_string(), offset + 1),
            OpCode::OP_FALSE => ("FALSE".to_string(), offset + 1),
            OpCode::OP_NOT => ("NOT".to_string(), offset + 1),
            OpCode::OP_EQUAL => ("EQUAL".to_string(), offset + 1),
            OpCode::OP_NOT_EQUAL => ("NOT_EQUAL".to_string(), offset + 1),
            OpCode::OP_GREATER => ("GREATER".to_string(), offset + 1),
            OpCode::OP_GREATER_EQUAL => ("GREATER_EQUAL".to_string(), offset + 1),
            OpCode::OP_LESS => ("LESS".to_string(), offset + 1),
            OpCode::OP_LESS_EQUAL => ("LESS_EQUAL".to_string(), offset + 1),
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
