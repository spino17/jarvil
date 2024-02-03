extern crate proc_macro;
use proc_macro::*;
use quote::quote;
use syn::Variant;

fn get_variant_field_name(variant: &Variant) -> &proc_macro2::Ident {
    let syn::Fields::Unnamed(field_name) = &variant.fields else {
        panic!("data of `ASTNode` enum should be named")
    };
    let field_ty = match field_name.unnamed.iter().next() {
        Some(field) => &field.ty,
        None => panic!("each variant of `ASTNode` enum should have respective node"),
    };
    let variant_field_name = match field_ty {
        syn::Type::Path(field_ty) => match field_ty.path.segments.iter().next() {
            Some(field_ty_name) => &field_ty_name.ident,
            _ => panic!("each variant of `ASTNode` enum always have a node"),
        },
        _ => panic!("type of the data in the variant of `ASTNode` should be a node"),
    };
    variant_field_name
}

pub fn impl_nodify_macro(ast: &syn::DeriveInput) -> TokenStream {
    if !ast.ident.to_string().eq("ASTNode") {
        panic!("nodify macro should only be used for `crate::ast::ast::ASTNode` enum")
    }
    let syn::Data::Enum(enum_data) = &ast.data else {
        panic!("nodify macro should only be used for `crate::ast::ast::ASTNode` enum")
    };
    let variants = &enum_data.variants;
    let ast_node_new_methods = variants.iter().map(|variant| {
        let variant_name = &variant.ident;
        let variant_field_name = get_variant_field_name(variant);
        let variant_new_method_name = proc_macro2::Ident::new(
            &format!("new_with_{}", variant_field_name.to_string()),
            proc_macro2::Span::call_site(),
        );
        quote! {
            pub fn #variant_new_method_name(x: &#variant_field_name) -> Self {
                ASTNode::#variant_name(x.clone())
            }
        }
    });
    let serialize_impl_node_methods = variants.iter().map(|variant| {
        let node_name = get_variant_field_name(variant);
        quote! {
            impl Serialize for #node_name {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where
                    S: serde::Serializer,
                {
                    self.0.as_ref().serialize(serializer)
                }
            }
        }
    });
    let gen = quote! {
        impl ASTNode {
            #(#ast_node_new_methods)*
        }
        #(#serialize_impl_node_methods)*
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
