macro_rules! impl_opcode {
    ($(($t: ident, $v: tt)),*) => {
        impl OpCode {
            pub fn to_byte(&self) -> u8 {
                match self {
                    $(
                        OpCode::$t => $v,
                    )*
                }
            }
        }
    };
}

macro_rules! impl_opcode_map {
    (($($t: ident),*), $v: tt) => {
        pub const OP_CODES_MAP: [OpCode; $v] = [
            $(
                OpCode::$t,
            )*
        ];
    };
}

macro_rules! impl_eval_arithmetic_op {
    ($s: tt, $t: ident, $u: ident, $v: ident, $r: ident) => {
        match $t {
            Data::INT(l_val) => {
                match $u {
                    Data::INT(r_val) => return Data::INT(l_val $s r_val),
                    Data::FLOAT(r_val) => return Data::FLOAT(l_val as f64 $s r_val),
                    _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG)
                }
            }
            Data::FLOAT(l_val) => {
                match $u {
                    Data::INT(r_val) => return Data::FLOAT(l_val $s r_val as f64),
                    Data::FLOAT(r_val) => return Data::FLOAT(l_val $s r_val),
                    _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG)
                }
            }
            Data::OBJ(l_obj) => {
                match $u {
                    Data::OBJ(r_obj) => return $v(l_obj, r_obj, $r),
                    _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG)
                }
            }
            Data::BOOL(_) => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG)
        }
    };
}

macro_rules! impl_eval_comparison_op {
    ($s: tt, $t: ident, $u: ident, $v: ident, $r: ident) => {
        match $t {
            Data::INT(l_val) => {
                match $u {
                    Data::INT(r_val) => return Data::BOOL(l_val $s r_val),
                    Data::FLOAT(r_val) => return Data::BOOL((l_val as f64) $s r_val),
                    _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG)
                }
            }
            Data::FLOAT(l_val) => {
                match $u {
                    Data::INT(r_val) => return Data::BOOL(l_val $s r_val as f64),
                    Data::FLOAT(r_val) => return Data::BOOL(l_val $s r_val),
                    _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG)
                }
            }
            Data::OBJ(l_obj) => {
                match $u {
                    Data::OBJ(r_obj) => return $v(l_obj, r_obj, $r),
                    _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG)
                }
            }
            Data::BOOL(_) => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG)
        }
    };
}

macro_rules! impl_eval_logical_op {
    ($s: tt, $t: ident, $u: ident, $v: ident, $r: ident) => {
        match $t {
            Data::BOOL(l_val) => {
                match $u {
                    Data:: BOOL(r_val) => return Data::BOOL(l_val $s r_val),
                    _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG)
                }
            }
            Data::OBJ(l_obj) => match $u {
                Data::OBJ(r_obj) => return $v(l_obj, r_obj, $r),
                _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG),
            }
            _ => unreachable!("{}", TYPE_CHECK_BUG_ERROR_MSG)
        }
    };
}
