// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![cfg(feature = "extra-traits")]

extern crate proc_macro2;
extern crate quote;
extern crate syn;

use proc_macro2::{Ident, Literal, Span};
use syn::parse::Parser;
use syn::*;

#[macro_use]
mod macros;

fn lit<T: Into<Literal>>(t: T) -> Lit {
    Lit::new(t.into())
}

fn ident(s: &str) -> Ident {
    Ident::new(s, Span::call_site())
}

#[test]
fn test_meta_item_word() {
    run_test("#[foo]", Meta::Word(ident("foo")))
}

#[test]
fn test_meta_item_name_value() {
    run_test(
        "#[foo = 5]",
        MetaNameValue {
            ident: ident("foo").into(),
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
            ident: ident("foo").into(),
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
            ident: ident("foo").into(),
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
            ident: ident("foo").into(),
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
            ident: ident("foo").into(),
            paren_token: Default::default(),
            nested: punctuated![NestedMeta::Meta(Meta::Word(ident("bar").into()))],
        },
    )
}

#[test]
fn test_meta_item_list_name_value() {
    run_test(
        "#[foo(bar = 5)]",
        MetaList {
            ident: ident("foo").into(),
            paren_token: Default::default(),
            nested: punctuated![NestedMeta::Meta(
                MetaNameValue {
                    ident: ident("bar").into(),
                    eq_token: Default::default(),
                    lit: lit(Literal::i32_unsuffixed(5)),
                }
                .into(),
            )],
        },
    )
}

#[test]
fn test_meta_item_list_bool_value() {
    run_test(
        "#[foo(bar = true)]",
        MetaList {
            ident: ident("foo").into(),
            paren_token: Default::default(),
            nested: punctuated![NestedMeta::Meta(
                MetaNameValue {
                    ident: ident("bar").into(),
                    eq_token: Default::default(),
                    lit: Lit::Bool(LitBool {
                        value: true,
                        span: Span::call_site()
                    }),
                }
                .into(),
            )],
        },
    )
}

#[test]
fn test_meta_item_multiple() {
    run_test(
        "#[foo(word, name = 5, list(name2 = 6), word2)]",
        MetaList {
            ident: ident("foo").into(),
            paren_token: Default::default(),
            nested: punctuated![
                NestedMeta::Meta(Meta::Word(ident("word").into())),
                NestedMeta::Meta(
                    MetaNameValue {
                        ident: ident("name").into(),
                        eq_token: Default::default(),
                        lit: lit(Literal::i32_unsuffixed(5)),
                    }
                    .into(),
                ),
                NestedMeta::Meta(
                    MetaList {
                        ident: ident("list").into(),
                        paren_token: Default::default(),
                        nested: punctuated![NestedMeta::Meta(
                            MetaNameValue {
                                ident: ident("name2").into(),
                                eq_token: Default::default(),
                                lit: lit(Literal::i32_unsuffixed(6)),
                            }
                            .into(),
                        )],
                    }
                    .into(),
                ),
                NestedMeta::Meta(Meta::Word(ident("word2").into())),
            ],
        },
    )
}

#[test]
fn test_bool_lit() {
    run_test(
        "#[foo(true)]",
        MetaList {
            ident: ident("foo").into(),
            paren_token: Default::default(),
            nested: punctuated![NestedMeta::Literal(Lit::Bool(LitBool {
                value: true,
                span: Span::call_site(),
            }))],
        },
    )
}

#[test]
fn test_parse_meta_item_word() {
    let raw = "hello";

    let expected = Meta::Word(ident("hello"));

    assert_eq!(expected, syn::parse_str(raw).unwrap());
}

#[test]
fn test_parse_meta_name_value() {
    let raw = "foo = 5";

    let expected = MetaNameValue {
        ident: ident("foo").into(),
        eq_token: Default::default(),
        lit: lit(Literal::i32_unsuffixed(5)),
    };

    assert_eq!(expected, syn::parse_str(raw).unwrap());

    let expected = Meta::NameValue(expected);

    assert_eq!(expected, syn::parse_str(raw).unwrap());
}

#[test]
fn test_parse_meta_name_value_with_keyword() {
    let raw = "static = 5";

    let expected = MetaNameValue {
        ident: ident("static").into(),
        eq_token: Default::default(),
        lit: lit(Literal::i32_unsuffixed(5)),
    };

    assert_eq!(expected, syn::parse_str(raw).unwrap());

    let expected = Meta::NameValue(expected);

    assert_eq!(expected, syn::parse_str(raw).unwrap());
}

#[test]
fn test_parse_meta_name_value_with_bool() {
    let raw = "true = 5";

    let expected = MetaNameValue {
        ident: ident("true").into(),
        eq_token: Default::default(),
        lit: lit(Literal::i32_unsuffixed(5)),
    };

    assert_eq!(expected, syn::parse_str(raw).unwrap());

    let expected = Meta::NameValue(expected);

    assert_eq!(expected, syn::parse_str(raw).unwrap());
}

#[test]
fn test_parse_meta_item_list_lit() {
    let raw = "foo(5)";

    let expected = MetaList {
        ident: ident("foo").into(),
        paren_token: Default::default(),
        nested: punctuated![NestedMeta::Literal(lit(Literal::i32_unsuffixed(5)))],
    };

    assert_eq!(expected, syn::parse_str(raw).unwrap());

    let expected = Meta::List(expected);

    assert_eq!(expected, syn::parse_str(raw).unwrap());
}

#[test]
fn test_parse_meta_item_multiple() {
    let raw = "foo(word, name = 5, list(name2 = 6), word2)";

    let expected = MetaList {
        ident: ident("foo").into(),
        paren_token: Default::default(),
        nested: punctuated![
            NestedMeta::Meta(Meta::Word(ident("word").into())),
            NestedMeta::Meta(
                MetaNameValue {
                    ident: ident("name").into(),
                    eq_token: Default::default(),
                    lit: lit(Literal::i32_unsuffixed(5)),
                }
                .into(),
            ),
            NestedMeta::Meta(
                MetaList {
                    ident: ident("list").into(),
                    paren_token: Default::default(),
                    nested: punctuated![NestedMeta::Meta(
                        MetaNameValue {
                            ident: ident("name2").into(),
                            eq_token: Default::default(),
                            lit: lit(Literal::i32_unsuffixed(6)),
                        }
                        .into(),
                    )],
                }
                .into(),
            ),
            NestedMeta::Meta(Meta::Word(ident("word2").into())),
        ],
    };

    assert_eq!(expected, syn::parse_str(raw).unwrap());

    let expected = Meta::List(expected);

    assert_eq!(expected, syn::parse_str(raw).unwrap());
}

#[test]
fn test_parse_nested_meta() {
    let raw = "5";

    let expected = NestedMeta::Literal(lit(Literal::i32_unsuffixed(5)));

    assert_eq!(expected, syn::parse_str(raw).unwrap());

    let raw = "list(name2 = 6)";

    let expected = NestedMeta::Meta(
        MetaList {
            ident: ident("list").into(),
            paren_token: Default::default(),
            nested: punctuated![NestedMeta::Meta(
                MetaNameValue {
                    ident: ident("name2").into(),
                    eq_token: Default::default(),
                    lit: lit(Literal::i32_unsuffixed(6)),
                }
                .into(),
            )],
        }
        .into(),
    );

    assert_eq!(expected, syn::parse_str(raw).unwrap());
}

fn run_test<T: Into<Meta>>(input: &str, expected: T) {
    let attrs = Attribute::parse_outer.parse_str(input).unwrap();
    assert_eq!(attrs.len(), 1);
    let attr = attrs.into_iter().next().unwrap();
    let expected = expected.into();
    assert_eq!(expected, attr.interpret_meta().unwrap());
    assert_eq!(expected, attr.parse_meta().unwrap());
}
