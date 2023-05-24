macro_rules! impl_op_compatiblity {
    ($t: ident, $v: expr, $u: expr) => {
        match $v.0.as_ref() {
            CoreType::ATOMIC(atomic_type) => atomic_type.$t($u),
            CoreType::ARRAY(array_type) => array_type.$t($u),
            CoreType::HASHMAP(hashmap_type) => hashmap_type.$t($u),
            CoreType::STRUCT(struct_type) => struct_type.$t($u),
            CoreType::TUPLE(tuple_type) => tuple_type.$t($u),
            CoreType::LAMBDA(lambda_type) => lambda_type.$t($u),
            CoreType::UNKNOWN => return None,
            CoreType::VOID => return None,
            CoreType::UNSET => return None,
            CoreType::ANY => return None,
        }
    };
}