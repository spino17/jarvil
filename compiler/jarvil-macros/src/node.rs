extern crate proc_macro;
use crate::helper::get_macro_expr_stmt;
use proc_macro::*;
use quote::quote;
use syn::{
    punctuated::Punctuated,
    token::{Colon2, Comma},
    Expr, ExprTuple, PathSegment, Token,
};

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

pub fn type_from_str(type_name: &str) -> syn::Type {
    let mut punc: Punctuated<PathSegment, Colon2> = syn::punctuated::Punctuated::new();
    punc.push(syn::PathSegment {
        ident: proc_macro2::Ident::new(type_name, proc_macro2::Span::call_site()),
        arguments: syn::PathArguments::None,
    });
    syn::Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path {
            leading_colon: None,
            segments: punc,
        },
    })
}

pub fn impl_node_macro(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident.to_string(); // eg. CoreBlockNode
    let node_type = type_from_str(&name[4..]);
    let enum_data = match &ast.data {
        syn::Data::Enum(enum_data) => enum_data,
        _ => panic!("Node macro should only be used for `Core<Node>` enum"),
    };
    let variant_iter = &mut enum_data.variants.iter();
    let mut flag = false;
    let mut common_str = "".to_string();
    for variant in variant_iter.by_ref() {
        let variant_name = variant.ident.to_string(); // eg. `BLOCK`
        if flag {
            common_str.push_str(", ");
        }
        common_str.push_str(&format!("({}, {})", name, variant_name));
        flag = true;
    }
    let range_macro_str = format!("&self.0.as_ref(), {}", &common_str);
    let start_line_number_str = format!("&self.0.as_ref(), start_line_number, {}", &common_str);
    let range_macro_expr = get_macro_expr_stmt("impl_node_variant_for_range", &range_macro_str);
    let start_line_number_macro_expr =
        get_macro_expr_stmt("impl_enum_variant", &start_line_number_str);
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
