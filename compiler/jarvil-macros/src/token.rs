extern crate proc_macro;
use crate::helper::get_macro_expr_stmt;
use proc_macro::*;
use quote::quote;

pub fn impl_tokenify_macro(ast: &syn::DeriveInput) -> TokenStream {
    let enum_name = &ast.ident;
    if !enum_name.to_string().eq("CoreToken") {
        panic!("tokenify macro should only be used for `crate::lexer::token::CoreToken` enum")
    }
    let enum_data = match &ast.data {
        syn::Data::Enum(enum_data) => enum_data,
        _ => panic!("tokenify macro should only be used for `crate::lexer::token::CoreToken` enum"),
    };
    let variant_iter = &mut enum_data.variants.iter();
    let mut args_str = "".to_string();
    let Some(variant) = variant_iter.next() else {
        unreachable!()
    };
    args_str.push_str(&variant.ident.to_string());
    for variant in variant_iter.by_ref() {
        let variant_name = &variant.ident.to_string();
        if variant_name.eq("LEXICAL_ERROR") {
            continue;
        }
        args_str.push_str(&format!(", {}", variant_name));
    }
    let symbols_check_macro_stmt = get_macro_expr_stmt("impl_symbols_check", &args_str);
    let token_to_string_macro = get_macro_expr_stmt("impl_token_to_string", &args_str);
    args_str.push_str(", LEXICAL_ERROR");
    let symbols_is_eq = get_macro_expr_stmt("impl_symbols_is_eq", &args_str);
    let gen = quote! {
        impl #enum_name {
            #symbols_check_macro_stmt;
            fn LEXICAL_ERROR(&self) -> bool {
                match self {
                    CoreToken::LEXICAL_ERROR(_) => true,
                    _ => false,
                }
            }
           #symbols_is_eq;
           #token_to_string_macro;
        }
    };
    gen.into()
}
