extern crate proc_macro;
use crate::helper::get_macro_expr_stmt;
use proc_macro::*;
use quote::quote;
use syn::{punctuated::Punctuated, token::Comma, Expr, ExprTuple, Token};

pub fn get_tuple_from_str(entry_1: &str, entry_2: &str) -> syn::ExprTuple {
    let mut elems: Punctuated<Expr, Comma> = syn::punctuated::Punctuated::default();
    elems.push(Expr::Lit(syn::ExprLit {
        attrs: vec![],
        lit: syn::Lit::Str(syn::LitStr::new(entry_1, proc_macro2::Span::call_site())),
    }));
    elems.push_punct(Token![,](proc_macro2::Span::call_site()));
    elems.push(Expr::Lit(syn::ExprLit {
        attrs: vec![],
        lit: syn::Lit::Str(syn::LitStr::new(entry_2, proc_macro2::Span::call_site())),
    }));
    syn::ExprTuple {
        attrs: vec![],
        paren_token: syn::token::Paren {
            span: proc_macro2::Span::call_site(),
        },
        elems,
    }
}

pub fn impl_nodify_macro(ast: &syn::DeriveInput) -> TokenStream {
    let enum_name = &ast.ident;
    if !enum_name.to_string().eq("ASTNode") {
        panic!("nodify macro should only be used for `crate::ast::ast::ASTNode` enum")
    }
    let enum_data = match &ast.data {
        syn::Data::Enum(enum_data) => enum_data,
        _ => panic!("nodify macro should only be used for `crate::ast::ast::ASTNode` enum"),
    };
    let variant_iter = &mut enum_data.variants.iter();
    let mut impl_ast_node = "".to_string();
    let mut flag = false;
    let mut variants_info: Vec<ExprTuple> = vec![];
    for variant in variant_iter.by_ref() {
        let variant_name = variant.ident.to_string(); // eg. `BLOCK`
        let field_name = match &variant.fields {
            syn::Fields::Unnamed(field) => field,
            _ => panic!("data of `ASTNode` enum should be named"),
        };
        let field_type = match field_name.unnamed.iter().next() {
            Some(field) => &field.ty,
            None => panic!("each variant of `ASTNode` enum should have respective node"),
        };
        let variant_field_name = match field_type {
            syn::Type::Path(field_type) => match field_type.path.segments.iter().next() {
                Some(field_type_name) => &field_type_name.ident,
                _ => panic!("each variant of `ASTNode` enum always have a node"),
            },
            _ => panic!("type of the data in the variant of `ASTNode` should be a node"),
        };
        let variant_field_name = variant_field_name.to_string(); // eg. BlockNode
        variants_info.push(get_tuple_from_str(&variant_name, &variant_field_name));
        if flag {
            impl_ast_node.push_str(", ");
        }
        impl_ast_node.push_str(&format!(
            "({}, {}, new_with_{})",
            &variant_name, &variant_field_name, &variant_field_name
        ));
        flag = true;
    }
    let impl_ast_node = get_macro_expr_stmt("impl_ast_node", &impl_ast_node);
    let gen = quote! {
        impl ASTNode {
            #impl_ast_node;
        }
    };
    gen.into()
}

pub fn impl_node_macro(ast: &syn::DeriveInput) -> TokenStream {
    let core_node_name = &ast.ident;
    let node_ty =
        proc_macro2::Ident::new(&ast.ident.to_string()[4..], proc_macro2::Span::call_site());
    let syn::Data::Enum(enum_data) = &ast.data else {
        panic!("node macro should only be used for enum type `Core` AST nodes")
    };
    let variants = &enum_data.variants;
    let matching_arms_for_range = variants.iter().map(|variant| {
        let variant_name = &variant.ident;
        quote! {
            #core_node_name::#variant_name(x) => {
                impl_range!(x, x)
            }
        }
    });
    let matching_arms_for_start_line_number = variants.iter().map(|variant| {
        let variant_name = &variant.ident;
        quote! {
            #core_node_name::#variant_name(x) => {
                x.start_line_number()
            }
        }
    });
    let gen = quote! {
        impl Node for #node_ty {
            fn range(&self) -> TextRange {
                match &self.0.as_ref() {
                    #(#matching_arms_for_range)*,
                }
            }
            fn start_line_number(&self) -> usize {
                match &self.0.as_ref() {
                    #(#matching_arms_for_start_line_number)*,
                }
            }
        }
    };
    gen.into()
}
