extern crate proc_macro;
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
    let gen = quote! {
        impl CoreToken {
            #(#token_eq_funcs)*
            #is_eq_func
            #to_string_func
        }
    };
    gen.into()
}
