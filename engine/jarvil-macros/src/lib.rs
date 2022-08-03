extern crate proc_macro;
use proc_macro::*;

#[proc_macro_attribute]
pub fn log_entry_and_exit(args: TokenStream, input: TokenStream) -> TokenStream {
    let x = format!(r#"
        fn dummy() {{
            println!("entering");
            println!("args tokens: {{}}", {args});
            println!("input tokens: {{}}", {input});
            println!("exiting");
        }}
    "#,
            args = args.into_iter().count(),
            input = input.into_iter().count(),
    );

    x.parse().expect("Generated invalid tokens")
}