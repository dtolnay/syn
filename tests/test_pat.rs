// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#[macro_use]
extern crate quote;
extern crate syn;

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
