use super::helper::get_machine_byte_multiple;
use std::{convert::TryInto, fmt::Display, vec};

pub enum OpCode {
    OP_RETURN,   // 0
    OP_CONSTANT, // 1
}
impl OpCode {
    pub fn to_byte(&self) -> u8 {
        match self {
            OpCode::OP_RETURN => 0,
            OpCode::OP_CONSTANT => 1,
        }
    }
}

pub enum Data {
    INT(i32),
    FLOAT(f32),
    LITERAL(String),
    BOOL(bool),
}
impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Data::INT(val) => write!(f, "{}", val),
            Data::FLOAT(val) => write!(f, "{}", val),
            Data::LITERAL(val) => write!(f, "{}", val),
            Data::BOOL(val) => write!(f, "{}", val),
        }
    }
}

const OP_CODES_MAP: [OpCode; 2] = [OpCode::OP_RETURN, OpCode::OP_CONSTANT];

pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Data>,
    line_numbers: Vec<usize>,
}
impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: vec![],
            constants: vec![],
            line_numbers: vec![],
        }
    }

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
            let (mut str_rep, new_offset) = self.disassemble_instruction(offset);
            str_rep.push_str(&format!("{}", self.line_numbers[inst_index]));
            parsed_instructions.push(str_rep);
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
