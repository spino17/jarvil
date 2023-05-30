macro_rules! impl_op_compatiblity {
    ($t: ident, $v: expr, $u: expr) => {
        match $v.0.as_ref() {
            CoreType::Atomic(atomic_type) => atomic_type.$t($u),
            CoreType::Array(array_type) => array_type.$t($u),
            CoreType::HashMap(hashmap_type) => hashmap_type.$t($u),
            CoreType::Struct(struct_type) => struct_type.$t($u),
            CoreType::Tuple(tuple_type) => tuple_type.$t($u),
            CoreType::Lambda(lambda_type) => lambda_type.$t($u),
            CoreType::Unknown => return None,
            CoreType::Void => return None,
            CoreType::Unset => return None,
            CoreType::Any => return None,
        }
    };
}
