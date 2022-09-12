macro_rules! decode_arithmetic_op {
    ($t: tt, $u: expr) => {
        match $u.stack.pop() {
            Data::INT(r_val) => {
                match $u.stack.pop() {
                    Data::INT(l_val) => {
                        $u.stack.push(Data::INT(l_val $t r_val));
                    },
                    Data::FLOAT(l_val) => {
                        $u.stack.push(Data::FLOAT(l_val $t r_val as f64));
                    },
                    _ => return InterpretResult::COMPILE_ERROR,
                }
            },
            Data::FLOAT(r_val) => {
                match $u.stack.pop() {
                    Data::INT(l_val) => {
                        $u.stack.push(Data::FLOAT(l_val as f64 $t r_val));
                    },
                    Data::FLOAT(l_val) => {
                        $u.stack.push(Data::FLOAT(l_val $t r_val));
                    },
                    _ => return InterpretResult::COMPILE_ERROR,
                }
            },
            _ => return InterpretResult::COMPILE_ERROR
        }
    };
}

macro_rules! decode_comparison_op {
    ($t: tt, $u: expr) => {
        match $u.stack.pop() {
            Data::INT(r_val) => {
                match $u.stack.pop() {
                    Data::INT(l_val) => {
                        $u.stack.push(Data::BOOL(l_val $t r_val))
                    },
                    Data::FLOAT(l_val) => {
                        $u.stack.push(Data::BOOL(l_val $t r_val as f64))
                    },
                    _ => return InterpretResult::COMPILE_ERROR
                }
            },
            Data::FLOAT(r_val) => {
                match $u.stack.pop() {
                    Data::INT(l_val) => {
                        $u.stack.push(Data::BOOL((l_val as f64) $t r_val))
                    },
                    Data::FLOAT(l_val) => {
                        $u.stack.push(Data::BOOL(l_val $t r_val))
                    },
                    _ => return InterpretResult::COMPILE_ERROR
                }
            },
            _ => return InterpretResult::COMPILE_ERROR
        }
    }
}

macro_rules! decode_equality_op {
    ($t: tt, $u: expr) => {
        match $u.stack.pop() {
            Data::INT(r_val) => {
                match $u.stack.pop() {
                    Data::INT(l_val) => {
                        $u.stack.push(Data::BOOL(l_val $t r_val))
                    },
                    Data::FLOAT(l_val) => {
                        $u.stack.push(Data::BOOL(l_val $t r_val as f64))
                    },
                    _ => return InterpretResult::COMPILE_ERROR
                }
            },
            Data::FLOAT(r_val) => {
                match $u.stack.pop() {
                    Data::INT(l_val) => {
                        $u.stack.push(Data::BOOL(l_val as f64 $t r_val))
                    },
                    Data::FLOAT(l_val) => {
                        $u.stack.push(Data::BOOL(l_val $t r_val))
                    },
                    _ => return InterpretResult::COMPILE_ERROR
                }
            },
            Data::BOOL(r_val) => {
                match $u.stack.pop() {
                    Data::BOOL(l_val) => $u.stack.push(Data::BOOL(l_val $t r_val)),
                    _ => return InterpretResult::COMPILE_ERROR
                }
            }
        }
    }
}
