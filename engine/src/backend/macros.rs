macro_rules! decode_op {
    ($t: tt, $u: expr) => {
        match $u.stack.pop() {
            Data::INT(r_val) => {
                match $u.stack.pop() {
                    Data::INT(l_val) => {
                        $u.stack.push(Data::INT(l_val $t r_val));
                    },
                    Data::FLOAT(l_val) => {
                        $u.stack.push(Data::FLOAT(l_val $t r_val as f32));
                    },
                    _ => return InterpretResult::RUNTIME_ERROR,
                }
            },
            Data::FLOAT(r_val) => {
                match $u.stack.pop() {
                    Data::INT(l_val) => {
                        $u.stack.push(Data::FLOAT(l_val as f32 $t r_val));
                    },
                    Data::FLOAT(l_val) => {
                        $u.stack.push(Data::FLOAT(l_val $t r_val));
                    },
                    _ => return InterpretResult::RUNTIME_ERROR,
                }
            },
            _ => return InterpretResult::RUNTIME_ERROR
        }
    };
}
