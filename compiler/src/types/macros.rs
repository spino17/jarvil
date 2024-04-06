macro_rules! impl_op_compatiblity {
    ($n: ident, $t: ident, $v: expr, $u: expr) => {
        match $v.0.as_ref() {
            CoreType::Atomic(atomic_ty) => atomic_ty.$t($u, $n),
            CoreType::Array(array_ty) => array_ty.$t($u, $n),
            CoreType::HashMap(hashmap_ty) => hashmap_ty.$t($u, $n),
            CoreType::Struct(struct_ty) => struct_ty.$t($u, $n),
            CoreType::Tuple(tuple_ty) => tuple_ty.$t($u, $n),
            CoreType::Lambda(lambda_ty) => lambda_ty.$t($u, $n),
            CoreType::Generic(generic_ty) => generic_ty.$t($u, $n),
            CoreType::Enum(enum_ty) => enum_ty.$t($u, $n),
            CoreType::Unknown => None,
            CoreType::Void => None,
            CoreType::Unset => None,
        }
    };
}
