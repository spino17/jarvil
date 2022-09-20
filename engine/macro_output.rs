pub mod chunk {
    #[macro_use]
    use jarvil_macros::OpCodeUtil;
    use super::helper::get_machine_byte_multiple;
    use crate::backend::data::Data;
    use std::{convert::TryInto, fmt::Display};
    pub enum OpCode {
        RETURN,
        PUSH_CONSTANT,
        NEGATE,
        ADD,
        SUBTRACT,
        MULTIPLY,
        DIVIDE,
        TRUE,
        FALSE,
        NOT,
        EQUAL,
        NOT_EQUAL,
        GREATER,
        GREATER_EQUAL,
        LESS,
        LESS_EQUAL,
    }
    impl OpCode {
        pub fn to_byte(&self) -> u8 {
            match self {
                OpCode::RETURN => 0,
                OpCode::PUSH_CONSTANT => 1,
                OpCode::NEGATE => 2,
                OpCode::ADD => 3,
                OpCode::SUBTRACT => 4,
                OpCode::MULTIPLY => 5,
                OpCode::DIVIDE => 6,
                OpCode::TRUE => 7,
                OpCode::FALSE => 8,
                OpCode::NOT => 9,
                OpCode::EQUAL => 10,
                OpCode::NOT_EQUAL => 11,
                OpCode::GREATER => 12,
                OpCode::GREATER_EQUAL => 13,
                OpCode::LESS => 14,
                OpCode::LESS_EQUAL => 15,
                _ => ::core::panicking::panic("internal error: entered unreachable code"),
            }
        }
    }
    pub const OP_CODES_MAP: [OpCode; 16] = [
        OpCode::RETURN,
        OpCode::PUSH_CONSTANT,
        OpCode::NEGATE,
        OpCode::ADD,
        OpCode::SUBTRACT,
        OpCode::MULTIPLY,
        OpCode::DIVIDE,
        OpCode::TRUE,
        OpCode::FALSE,
        OpCode::NOT,
        OpCode::EQUAL,
        OpCode::NOT_EQUAL,
        OpCode::GREATER,
        OpCode::GREATER_EQUAL,
        OpCode::LESS,
        OpCode::LESS_EQUAL,
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
            self.code.push(OpCode::PUSH_CONSTANT.to_byte());
            self.code.extend_from_slice(&const_index.to_be_bytes());
            self.line_numbers.push(line_number);
        }
        pub fn disassemble(&self) -> Vec<String> {
            let mut offset = 0;
            let mut parsed_instructions: Vec<String> = ::alloc::vec::Vec::new();
            let mut inst_index = 0;
            while offset < self.code.len() {
                let (str_rep, new_offset) = self.disassemble_instruction(offset);
                let mut inst_str = {
                    let res = ::alloc::fmt::format(::core::fmt::Arguments::new_v1(
                        &["", ": "],
                        &[::core::fmt::ArgumentV1::new_display(
                            &self.line_numbers[inst_index],
                        )],
                    ));
                    res
                };
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
                        {
                            let res = ::alloc::fmt::format(::core::fmt::Arguments::new_v1(
                                &["CONSTANT "],
                                &[::core::fmt::ArgumentV1::new_display(&const_value)],
                            ));
                            res
                        },
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
            f.write_fmt(::core::fmt::Arguments::new_v1(
                &[""],
                &[::core::fmt::ArgumentV1::new_display(&display_str)],
            ))
        }
    }
    impl Default for Chunk {
        fn default() -> Self {
            Chunk {
                code: ::alloc::vec::Vec::new(),
                constants: ::alloc::vec::Vec::new(),
                line_numbers: ::alloc::vec::Vec::new(),
            }
        }
    }
}
