#[macro_use]
extern crate quote;
extern crate syn;

mod features;

#[test]
fn test_pat_ident() {
    match syn::parse2(quote!(self)).unwrap() {
        syn::Pat::Ident(_) => (),
        value => panic!("expected PatIdent, got {:?}", value),
    }
}

#[test]
fn test_pat_path() {
    match syn::parse2(quote!(self::CONST)).unwrap() {
        syn::Pat::Path(_) => (),
        value => panic!("expected PatPath, got {:?}", value),
    }
}
