extern crate proc_macro;
use proc_macro::*;
use quote::quote;
use syn::{
    token::Comma, FnArg, GenericArgument, PathArguments, PathSegment, Stmt, Type,
};
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
        }
        impl_weak_str.push_str(&format!("({}, {})", &weak_name, &core_name));
        weak_str.push_str(&format!("({}, {})", &variant_name, &weak_name));
        flag = true;
    }
    let impl_weak_node_macro_stmt = get_macro_expr_stmt("impl_weak_node", &impl_weak_str);
    let weak_nodes_macro_stmt = get_macro_expr_stmt("weak_ast_nodes", &weak_str);
    let gen = quote! {
        #impl_weak_node_macro_stmt;
        #weak_nodes_macro_stmt;
    };
    gen.into()
}