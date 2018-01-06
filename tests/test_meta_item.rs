// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![cfg(feature = "extra-traits")]

extern crate proc_macro2;
extern crate syn;

use syn::*;
use syn::buffer::TokenBuffer;
use proc_macro2::{Literal, TokenStream};

#[macro_use]
mod macros;

fn lit<T: Into<Literal>>(t: T) -> Lit {
    Lit::new(t.into(), Default::default())
}

#[test]
fn test_meta_item_word() {
    run_test("#[foo]", MetaItem::Term("foo".into()))
}

#[test]
fn test_meta_item_name_value() {
    run_test(
        "#[foo = 5]",
        MetaNameValue {
            ident: "foo".into(),
            eq_token: Default::default(),
            lit: lit(Literal::integer(5)),
        },
    )
}

#[test]
fn test_meta_item_list_lit() {
    run_test(
        "#[foo(5)]",
        MetaItemList {
            ident: "foo".into(),
            paren_token: Default::default(),
            nested: punctuated![NestedMetaItem::Literal(lit(Literal::integer(5)))],
        },
    )
}

#[test]
fn test_meta_item_list_word() {
    run_test(
        "#[foo(bar)]",
        MetaItemList {
            ident: "foo".into(),
            paren_token: Default::default(),
            nested: punctuated![NestedMetaItem::MetaItem(MetaItem::Term("bar".into()))],
        },
    )
}

#[test]
fn test_meta_item_list_name_value() {
    run_test(
        "#[foo(bar = 5)]",
        MetaItemList {
            ident: "foo".into(),
            paren_token: Default::default(),
            nested: punctuated![
                NestedMetaItem::MetaItem(
                    MetaNameValue {
                        ident: "bar".into(),
                        eq_token: Default::default(),
                        lit: lit(Literal::integer(5)),
                    }.into(),
                ),
            ],
        },
    )
}

#[test]
fn test_meta_item_multiple() {
    run_test(
        "#[foo(word, name = 5, list(name2 = 6), word2)]",
        MetaItemList {
            ident: "foo".into(),
            paren_token: Default::default(),
            nested: punctuated![
                NestedMetaItem::MetaItem(MetaItem::Term("word".into())),
                NestedMetaItem::MetaItem(
                    MetaNameValue {
                        ident: "name".into(),
                        eq_token: Default::default(),
                        lit: lit(Literal::integer(5)),
                    }.into(),
                ),
                NestedMetaItem::MetaItem(
                    MetaItemList {
                        ident: "list".into(),
                        paren_token: Default::default(),
                        nested: punctuated![
                            NestedMetaItem::MetaItem(
                                MetaNameValue {
                                    ident: "name2".into(),
                                    eq_token: Default::default(),
                                    lit: lit(Literal::integer(6)),
                                }.into(),
                            ),
                        ],
                    }.into(),
                ),
                NestedMetaItem::MetaItem(MetaItem::Term("word2".into())),
            ],
        },
    )
}

fn run_test<T: Into<MetaItem>>(input: &str, expected: T) {
    let tokens = input.parse::<TokenStream>().unwrap();
    let buf = TokenBuffer::new(tokens);
    let attr = match Attribute::parse_outer(buf.begin()) {
        Ok((e, rest)) => {
            assert!(rest.eof());
            e
        }
        Err(err) => panic!(err),
    };
    assert_eq!(expected.into(), attr.meta_item().unwrap());
}
