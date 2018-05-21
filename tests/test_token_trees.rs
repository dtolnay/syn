// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![cfg(feature = "extra-traits")]

extern crate proc_macro2;
#[macro_use]
extern crate quote;
extern crate syn;

use proc_macro2::Delimiter::*;
use proc_macro2::*;
use syn::{AttrStyle, Attribute, Lit};

fn alone(c: char) -> TokenTree {
    Punct::new(c, Spacing::Alone).into()
}

fn joint(c: char) -> TokenTree {
    Punct::new(c, Spacing::Joint).into()
}

fn delimited(delim: Delimiter, tokens: Vec<TokenTree>) -> TokenTree {
    Group::new(delim, tokens.into_iter().collect()).into()
}

fn word(sym: &str) -> TokenTree {
    Ident::new(sym, Span::call_site()).into()
}

#[test]
fn test_struct() {
    let raw = "
        #[derive(Debug, Clone)]
        pub struct Item {
            pub ident: Ident,
            pub attrs: Vec<Attribute>,
        }
    ";

    let expected = vec![
        alone('#'),
        delimited(
            Bracket,
            vec![
                word("derive"),
                delimited(Parenthesis, vec![word("Debug"), alone(','), word("Clone")]),
            ],
        ),
        word("pub"),
        word("struct"),
        word("Item"),
        delimited(
            Brace,
            vec![
                word("pub"),
                word("ident"),
                alone(':'),
                word("Ident"),
                alone(','),
                word("pub"),
                word("attrs"),
                alone(':'),
                word("Vec"),
                alone('<'),
                word("Attribute"),
                joint('>'),
                alone(','),
            ],
        ),
    ];

    fn wrap(tts: TokenStream) -> Attribute {
        Attribute {
            style: AttrStyle::Outer,
            pound_token: Default::default(),
            bracket_token: Default::default(),
            path: Ident::new("test", Span::call_site()).into(),
            tts,
            is_sugared_doc: false,
        }
    }

    let result = wrap(raw.parse().unwrap());
    let expected = wrap(expected.into_iter().collect());
    if result != expected {
        panic!("{:#?}\n!=\n{:#?}", result.tts, expected.tts);
    }
}

#[test]
fn test_literal_mangling() {
    let raw = "0_4";
    let parsed: Lit = syn::parse_str(raw).unwrap();
    assert_eq!(raw, quote!(#parsed).to_string());
}
