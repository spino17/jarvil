extern crate proc_macro;
use proc_macro::*;
use quote::quote;
use crate::get_macro_expr_stmt;

pub fn impl_opcode_util_macro(ast: &syn::DeriveInput) -> TokenStream {
    let enum_name = &ast.ident;
    if !enum_name.to_string().eq("OpCode") {
        panic!("`OpCodeUtil` macro should only be used for `crate::backend::chunk::OpCode` enum")
    }
    let enum_data = match &ast.data {
        syn::Data::Enum(enum_data) => enum_data,
        _ => panic!("`OpCodeUtil` macro should only be used for `crate::backend::chunk::OpCode` enum")
    };
    let variant_iter = &mut enum_data.variants.iter();
    let mut count: usize = 0;
    let mut impl_opcode_str = "".to_string();
    let mut impl_opcode_map_str = "(".to_string();
    let mut flag = false;
    while let Some(variant) = variant_iter.next() {
        let variant_name = &variant.ident.to_string();
        if flag {
            impl_opcode_str.push_str(", ");
            impl_opcode_map_str.push_str(", ");
        }
        impl_opcode_str.push_str(&format!("({}, {})", variant_name, count));
        impl_opcode_map_str.push_str(&format!("{}", variant_name));
        flag = true;
        count = count + 1;
    }
    impl_opcode_map_str.push_str(")");
    impl_opcode_map_str.push_str(&format!(", {}", count));
    let impl_opcode_macro = get_macro_expr_stmt("impl_opcode", &impl_opcode_str);
    let impl_opcode_map_macro = get_macro_expr_stmt("impl_opcode_map", &impl_opcode_map_str);
    let gen = quote! {
        #impl_opcode_macro;
        #impl_opcode_map_macro;
    };
    gen.into()
}