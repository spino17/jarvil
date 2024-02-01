extern crate proc_macro;
use crate::helper::get_macro_expr_stmt;
use proc_macro::*;
use quote::quote;

pub fn impl_tokenify_macro(ast: &syn::DeriveInput) -> TokenStream {
    if !ast.ident.to_string().eq("CoreToken") {
        panic!("tokenify macro should only be used for `crate::lexer::token::CoreToken` enum")
    }
    let syn::Data::Enum(enum_data) = &ast.data else {
        unreachable!()
    };
    let variants = &enum_data.variants;
    let token_eq_funcs = variants.iter().map(|variant| {
        let variant_name = &variant.ident;
        quote! {
            fn #variant_name(&self) -> bool {
                match self {
                    CoreToken::#variant_name => true,
                    _ => false,
                }
            }
        }
    });
    let matching_arms_for_is_eq = variants.iter().map(|variant| {
        let variant_name = &variant.ident;
        quote! {
            #variant_name => self.#variant_name(),
        }
    });
    let is_eq_func = quote! {
        pub fn is_eq(&self, symbol: &str) -> bool {
            match symbol {
                #(#matching_arms_for_is_eq)*
                "\n" => self.NEWLINE(),
                _ => unreachable!("unrecognized symbol found: `{}`", symbol),
            }
        }
    };

    let matching_arms_for_to_string = variants.iter().map(|variant| {
        let variant_name = &variant.ident;
        quote! {
            CoreToken::#variant_name => #variant_name,
        }
    });
    let to_string_func = quote! {
        pub fn to_string(&self) -> &'static str {
            let symbol_str = match self {
                #(#matching_arms_for_to_string)*
            };
            symbol_str
        }
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
        impl CoreToken {
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
