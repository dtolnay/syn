mod features;

use quote::quote;
use syn::{PatOr, Pat};

#[test]
fn test_pat_ident() {
    match syn::parse2(quote!(self)).unwrap() {
        Pat::Ident(_) => (),
        value => panic!("expected PatIdent, got {:?}", value),
    }
}

#[test]
fn test_pat_path() {
    match syn::parse2(quote!(self::CONST)).unwrap() {
        Pat::Path(_) => (),
        value => panic!("expected PatPath, got {:?}", value),
    }
}

#[test]
fn test_pat_or() {
    match syn::parse2(quote!("0" | "1")).unwrap() {
        Pat::Or(_) => (),
        value => panic!("expected PatOr, got {:?}", value),
    }

    match syn::parse2(quote!(| "0" | "1")).unwrap() {
        Pat::Or(PatOr { leading_vert: Some(_), .. } ) => (),
        value => panic!("expected PatOr, got {:?}", value),
    }
}