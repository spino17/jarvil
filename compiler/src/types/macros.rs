macro_rules! impl_op_compatiblity {
    ($t: ident, $v: expr, $u: expr, $w: expr) => {
        match $v.0.as_ref() {
            CoreType::Atomic(atomic_type) => atomic_type.$t($u, $w),
            CoreType::Array(array_type) => array_type.$t($u, $w),
            CoreType::HashMap(hashmap_type) => hashmap_type.$t($u, $w),
            CoreType::Struct(struct_type) => struct_type.$t($u, $w),
            CoreType::Tuple(tuple_type) => tuple_type.$t($u, $w),
            CoreType::Lambda(lambda_type) => lambda_type.$t($u, $w),
            CoreType::Generic(generic_type) => generic_type.$t($u, $w),
            CoreType::Unknown => return None,
            CoreType::Void => return None,
            CoreType::Unset => return None,
            CoreType::Any => return None,
        }
    };
}
