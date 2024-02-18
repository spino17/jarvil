macro_rules! impl_op_compatiblity {
    ($n: ident, $t: ident, $v: expr, $u: expr) => {
        match $v.0.as_ref() {
            CoreType::Atomic(atomic_type) => atomic_type.$t($u, $n),
            CoreType::Array(array_type) => array_type.$t($u, $n),
            CoreType::HashMap(hashmap_type) => hashmap_type.$t($u, $n),
            CoreType::Struct(struct_type) => struct_type.$t($u, $n),
            CoreType::Tuple(tuple_type) => tuple_type.$t($u, $n),
            CoreType::Lambda(lambda_type) => lambda_type.$t($u, $n),
            CoreType::Generic(generic_type) => generic_type.$t($u, $n),
            CoreType::Enum(enum_type) => enum_type.$t($u, $n),
            CoreType::Unknown => None,
            CoreType::Void => None,
            CoreType::Unset => None,
            CoreType::Any => None,
        }
    };
}
