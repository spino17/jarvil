extern crate proc_macro;
use std::fmt::format;

use proc_macro::*;
use quote::quote;
use syn::{punctuated::Punctuated, PathSegment, token::Colon2};
use crate::get_macro_expr_stmt;

pub fn impl_weak_nodes_macro(ast: &syn::DeriveInput) -> TokenStream {
    let enum_name = &ast.ident;
    if !enum_name.to_string().eq("ASTNode") {
        panic!("tokenify macro should only be used for `crate::lexer::token::CoreToken` enum")
    }
    let enum_data = match &ast.data {
        syn::Data::Enum(enum_data) => enum_data,
        _ => panic!("tokenify macro should only be used for `crate::lexer::token::CoreToken` enum")
    };
    let variant_iter = &mut enum_data.variants.iter();
    let mut impl_weak_str = "".to_string();
    let mut weak_str = "".to_string();
    let mut impl_ast_node = "".to_string();
    let mut flag = false;
    while let Some(variant) = variant_iter.next() {
        let variant_name = variant.ident.to_string();  // eg. `BLOCK`
        let field_name = match &variant.fields {
            syn::Fields::Unnamed(field) => field,
            _ => panic!("data of `ASTNode` enum should be named"),
        };
        let field_type = match field_name.unnamed.iter().next() {
            Some(field) => &field.ty,
            None => panic!("each variant of `ASTNode` enum should have respective node")
        };
        let variant_field_name = match field_type {
            syn::Type::Path(field_type) => {
                match field_type.path.segments.iter().next() {
                    Some(field_type_name) => &field_type_name.ident,
                    _ => panic!("each variant of `ASTNode` enum always have a node")
                }
            },
            _ => panic!("type of the data in the variant of `ASTNode` should be a node")
        };
        let variant_field_name = variant_field_name.to_string();  // eg. BlockNode
        let weak_name = format!("Weak{}", &variant_field_name);
        let core_name = format!("Core{}", &variant_field_name);
        if flag {
            impl_weak_str.push_str(", ");
            weak_str.push_str(", ");
            impl_ast_node.push_str(", ");
        }
        impl_weak_str.push_str(&format!("({}, {})", &weak_name, &core_name));
        weak_str.push_str(&format!("({}, {})", &variant_name, &weak_name));
        impl_ast_node.push_str(&format!("({}, {}, new_with_{})", &variant_name, &variant_field_name, &variant_field_name));
        flag = true;
    }
    let impl_weak_node_macro_stmt = get_macro_expr_stmt("impl_weak_node", &impl_weak_str);
    let weak_nodes_macro_stmt = get_macro_expr_stmt("weak_ast_nodes", &weak_str);
    let impl_ast_node = get_macro_expr_stmt("impl_ast_node", &impl_ast_node);
    let gen = quote! {
        #impl_weak_node_macro_stmt;
        #weak_nodes_macro_stmt;
        impl ASTNode {
            #impl_ast_node;
        }
    };
    gen.into()
}

pub fn type_from_str(type_name: &str) -> syn::Type {
    let mut punc: Punctuated<PathSegment, Colon2> = syn::punctuated::Punctuated::new();
    punc.push(syn::PathSegment{
        ident: proc_macro2::Ident::new(type_name, proc_macro2::Span::call_site()),
        arguments: syn::PathArguments::None,
    });
    syn::Type::Path(syn::TypePath{
        qself: None,
        path: syn::Path{
            leading_colon: None,
            segments: punc,
        }
    })
}

pub fn impl_node_trait(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident.to_string();  // eg. CoreBlockNode
    let node_type = type_from_str(&name[4..]);
    let enum_data = match &ast.data {
        syn::Data::Enum(enum_data) => enum_data,
        _ => panic!("Node macro should only be used for `Core<Node>` enum")
    };
    let variant_iter = &mut enum_data.variants.iter();
    let mut flag = false;
    let mut common_str = "".to_string();
    while let Some(variant) = variant_iter.next() {
        let variant_name = variant.ident.to_string();  // eg. `BLOCK`
        if flag {
            common_str.push_str(", ");
        }
        common_str.push_str(&format!("({}, {})", name, variant_name));
        flag = true;
    }
    let range_macro_str = format!("&self.0.as_ref(), {}", &common_str);
    let start_line_number_str = format!("&self.0.as_ref(), start_line_number, {}", &common_str);
    let range_macro_expr = get_macro_expr_stmt("impl_node_variant_for_range", &range_macro_str);
    let start_line_number_macro_expr = get_macro_expr_stmt("impl_enum_variant", &start_line_number_str);
    let gen = quote! {
        impl Node for #node_type {
            fn range(&self) -> TextRange {
                #range_macro_expr
            }
            fn start_line_number(&self) -> usize {
                #start_line_number_macro_expr
            }
        }
    };
    gen.into()
}