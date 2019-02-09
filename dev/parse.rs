extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{parse_macro_input, File};

#[proc_macro]
pub fn r#mod(input: TokenStream) -> TokenStream {
    let file = parse_macro_input!(input as File);
    println!("{:#?}", file);
    "fn main() {}".parse().unwrap()
}
