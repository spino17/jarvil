use proc_macro::TokenStream;
use std::str::FromStr;
use syn::{
    punctuated::Punctuated, token::Colon2, Expr, ExprMacro, PathArguments, PathSegment, Stmt, Token,
};

// This method is taken from Tokio-macros
pub fn token_stream_with_error(mut tokens: TokenStream, error: syn::Error) -> TokenStream {
    tokens.extend(TokenStream::from(error.into_compile_error()));
    tokens
}

pub fn get_macro_expr_stmt(macro_name: &str, macro_expr_str: &str) -> Stmt {
    let mut punc: Punctuated<PathSegment, Colon2> = Punctuated::new();
    punc.push(syn::PathSegment {
        ident: syn::Ident::new(macro_name, quote::__private::Span::call_site()),
        arguments: PathArguments::None,
    });
    let token_stream = match proc_macro2::TokenStream::from_str(macro_expr_str) {
        Ok(token_stream) => token_stream,
        Err(e) => unreachable!("string should be lexically correct: {:?}", e),
    };
    let expr_macro = ExprMacro {
        attrs: vec![],
        mac: syn::Macro {
            path: syn::Path {
                leading_colon: None,
                segments: punc,
            },
            bang_token: Token![!](quote::__private::Span::call_site()),
            delimiter: syn::MacroDelimiter::Paren(syn::token::Paren {
                span: quote::__private::Span::call_site(),
            }),
            tokens: token_stream,
        },
    };

    Stmt::Expr(Expr::Macro(expr_macro))
}
