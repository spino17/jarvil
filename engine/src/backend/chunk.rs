use super::helper::get_machine_byte_multiple;
use std::rc::Rc;
use std::{convert::TryInto, fmt::Display, vec};

pub enum OpCode {
    OP_RETURN,   // 0
    OP_CONSTANT, // 1
    OP_NEGATE,   // 2
    OP_ADD,      // 3
    OP_SUBTRACT, // 4
    OP_MULTIPLY, // 5
    OP_DIVIDE,   // 6
    OP_TRUE,     // 7
    OP_FALSE,    // 8
    OP_NOT,      // 9
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
        }
    }
}

pub const OP_CODES_MAP: [OpCode; 10] = [
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
];

#[derive(Clone)]
pub enum Data {
    INT(i32),
    FLOAT(f32),
    LITERAL(Rc<String>),
    BOOL(bool),
}
impl Data {
    fn new_with_int(val: i32) -> Self {
        Data::INT(val)
    }

    fn new_with_float(val: f32) -> Self {
        Data::FLOAT(val)
    }

    fn new_with_bool(val: bool) -> Self {
        Data::BOOL(val)
    }

    fn is_int(&self) -> Option<i32> {
        match self {
            Data::INT(val) => Some(val.clone()),
            _ => None,
        }
    }

    fn is_float(&self) -> Option<f32> {
        match self {
            Data::FLOAT(val) => Some(val.clone()),
            _ => None,
        }
    }

    fn is_bool(&self) -> Option<bool> {
        match self {
            Data::BOOL(val) => Some(val.clone()),
            _ => None,
        }
    }
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
