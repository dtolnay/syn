#![cfg(feature = "expand")]

extern crate syn;
use syn::*;

#[macro_use]
extern crate quote;
use quote::Tokens;

extern crate tempdir;
use tempdir::TempDir;

use std::fs::File;
use std::io::{Read, Write};

#[test]
fn test_cfg() {
    let original = quote! {
        use super::*;

        #[derive(A)]
        struct P;

        #[cfg_attr(feature = "q", derive(A))]
        struct Q;

        #[derive(A)]
        #[cfg(feature = "r")]
        struct R;

        #[cfg(feature = "s1")]
        #[cfg(all(feature = "s2", feature = "s3"))]
        #[cfg_attr(feature = "s4", derive(A))]
        struct S;
    };

    let expected = quote! {
        // Unmodified from the input
        use super::*;

        type P = ();

        #[cfg(feature = "q")]
        type Q = ();

        #[cfg(feature = "r")]
        type R = ();

        #[cfg(all(feature = "s1", feature = "s2", feature = "s3", feature = "s4"))]
        type S = ();
    };

    test_expand(original, expected);
}

#[test]
fn test_recursive() {
    let original = quote! {
        #[d]
        #[cfg_attr(feature = "f", derive(Copy, B, Clone))]
        #[e]
        #[cfg(feature = "e")]
        struct T;
    };

    let expected = quote! {
        // From #[derive(A)] on struct S produced by #[derive(B)]
        #[cfg(all(feature = "e", feature = "f", feature = "g"))]
        type S = ();

        // From #[derive(B)] on struct T
        #[cfg(all(feature = "e", feature = "f"))]
        impl B for T {}

        // From the input
        #[d]
        #[cfg_attr(feature = "f", derive(Copy, Clone))]
        #[e]
        #[cfg(feature = "e")]
        struct T;
    };

    test_expand(original, expected);
}

fn test_expand(original: Tokens, expected: Tokens) {
    let dir = TempDir::new("syn").expect("create temp dir");
    let src_path = dir.path().join("expand.in.rs");
    let dst_path = dir.path().join("expand.rs");

    // Write the src file
    let mut src_file = File::create(&src_path).expect("create temp file");
    src_file.write_all(original.to_string().as_bytes()).expect("write temp file");

    // Run expansion
    let mut registry = Registry::new();
    registry.add_derive("A", expand_a);
    registry.add_derive("B", expand_b);
    registry.expand_file(&src_path, &dst_path).unwrap();

    // Read the dst file
    let mut expanded = String::new();
    let mut dst_file = File::open(&dst_path).expect("open output file");
    dst_file.read_to_string(&mut expanded).expect("read output file");
    let krate = parse_crate(&expanded).expect("parse output file");

    assert_eq!(quote!(#krate), expected);
}

fn expand_a(input: MacroInput) -> Result<Expanded, String> {
    let name = &input.ident;
    let out = quote! {
        type #name = ();
    };
    Ok(Expanded {
        new_items: parse_items(&out.to_string()).unwrap(),
        original: None,
    })
}

fn expand_b(input: MacroInput) -> Result<Expanded, String> {
    assert_eq!(quote!(#input), quote! {
        #[d]
        #[cfg_attr(feature = "f", derive(Copy, Clone))]
        #[e]
        struct T;
    });
    let out = quote! {
        #[cfg_attr(feature = "g", derive(A))]
        struct S;

        impl B for T {}
    };
    Ok(Expanded {
        new_items: parse_items(&out.to_string()).unwrap(),
        original: Some(input),
    })
}
