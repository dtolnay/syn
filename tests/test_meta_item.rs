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

use proc_macro2::{Literal, Span, TokenStream};
use syn::buffer::TokenBuffer;
use syn::*;

#[macro_use]
mod macros;

fn lit<T: Into<Literal>>(t: T) -> Lit {
    Lit::new(t.into())
}

#[test]
fn test_meta_item_word() {
    run_test("#[foo]", Meta::Word("foo".into()))
}

#[test]
fn test_meta_item_name_value() {
    run_test(
        "#[foo = 5]",
        MetaNameValue {
            ident: "foo".into(),
            eq_token: Default::default(),
            lit: lit(Literal::i32_unsuffixed(5)),
        },
    )
}

#[test]
fn test_meta_item_bool_value() {
    run_test(
        "#[foo = true]",
        MetaNameValue {
            ident: "foo".into(),
            eq_token: Default::default(),
            lit: Lit::Bool(LitBool {
                value: true,
                span: Span::call_site(),
            }),
        },
    );
    run_test(
        "#[foo = false]",
        MetaNameValue {
            ident: "foo".into(),
            eq_token: Default::default(),
            lit: Lit::Bool(LitBool {
                value: false,
                span: Span::call_site(),
            }),
        },
    )
}

#[test]
fn test_meta_item_list_lit() {
    run_test(
        "#[foo(5)]",
        MetaList {
            ident: "foo".into(),
            paren_token: Default::default(),
            nested: punctuated![NestedMeta::Literal(lit(Literal::i32_unsuffixed(5)))],
        },
    )
}

#[test]
fn test_meta_item_list_word() {
    run_test(
        "#[foo(bar)]",
        MetaList {
            ident: "foo".into(),
            paren_token: Default::default(),
            nested: punctuated![NestedMeta::Meta(Meta::Word("bar".into()))],
        },
    )
}

#[test]
fn test_meta_item_list_name_value() {
    run_test(
        "#[foo(bar = 5)]",
        MetaList {
            ident: "foo".into(),
            paren_token: Default::default(),
            nested: punctuated![
                NestedMeta::Meta(
                    MetaNameValue {
                        ident: "bar".into(),
                        eq_token: Default::default(),
                        lit: lit(Literal::i32_unsuffixed(5)),
                    }.into(),
                ),
            ],
        },
    )
}

#[test]
fn test_meta_item_list_bool_value() {
    run_test(
        "#[foo(bar = true)]",
        MetaList {
            ident: "foo".into(),
            paren_token: Default::default(),
            nested: punctuated![
                NestedMeta::Meta(
                    MetaNameValue {
                        ident: "bar".into(),
                        eq_token: Default::default(),
                        lit: Lit::Bool(LitBool {
                            value: true,
                            span: Span::call_site()
                        }),
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
        MetaList {
            ident: "foo".into(),
            paren_token: Default::default(),
            nested: punctuated![
                NestedMeta::Meta(Meta::Word("word".into())),
                NestedMeta::Meta(
                    MetaNameValue {
                        ident: "name".into(),
                        eq_token: Default::default(),
                        lit: lit(Literal::i32_unsuffixed(5)),
                    }.into(),
                ),
                NestedMeta::Meta(
                    MetaList {
                        ident: "list".into(),
                        paren_token: Default::default(),
                        nested: punctuated![
                            NestedMeta::Meta(
                                MetaNameValue {
                                    ident: "name2".into(),
                                    eq_token: Default::default(),
                                    lit: lit(Literal::i32_unsuffixed(6)),
                                }.into(),
                            ),
                        ],
                    }.into(),
                ),
                NestedMeta::Meta(Meta::Word("word2".into())),
            ],
        },
    )
}

fn run_test<T: Into<Meta>>(input: &str, expected: T) {
    let tokens = input.parse::<TokenStream>().unwrap();
    let buf = TokenBuffer::new2(tokens);
    let attr = match Attribute::parse_outer(buf.begin()) {
        Ok((e, rest)) => {
            assert!(rest.eof());
            e
        }
        Err(err) => panic!(err),
    };
    assert_eq!(expected.into(), attr.interpret_meta().unwrap());
}
