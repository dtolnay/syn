use quote::quote;
use syn::Item;

#[test]
fn parse_crate_root_custom_inner_attribute() {
    let tokens = quote! {
        #![feature(custom_inner_attributes)]
        #[prelude_import]
        use std::prelude::rust_2021::*;
        #[macro_use]
        extern crate std;
    };
    let error = syn::parse2::<Item>(tokens).unwrap_err();
    assert_eq!(
        error.to_compile_error().to_string(),
        r#"std :: compile_error ! { "expected square brackets" }"#
    );
}
