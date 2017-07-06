#![cfg(feature = "extra-traits")]

extern crate syn;
extern crate synom;
extern crate proc_macro2;

use synom::SynomBuffer;
use proc_macro2::{Literal, TokenStream};
use syn::*;

fn lit<T: Into<Literal>>(t: T) -> Lit {
    Lit {
        value: LitKind::Other(t.into()),
        span: Default::default(),
    }
}

#[test]
fn test_meta_item_word() {
    run_test("#[foo]", MetaItem::Term("foo".into()))
}

#[test]
fn test_meta_item_name_value() {
    run_test("#[foo = 5]", MetaNameValue {
        ident: "foo".into(),
        eq_token: Default::default(),
        lit: lit(Literal::integer(5)),
    })
}

#[test]
fn test_meta_item_list_lit() {
    run_test("#[foo(5)]", MetaItemList {
        ident: "foo".into(),
        paren_token: Default::default(),
        nested: vec![
            NestedMetaItem::Literal(lit(Literal::integer(5))),
        ].into(),
    })
}

#[test]
fn test_meta_item_list_word() {
    run_test("#[foo(bar)]", MetaItemList {
        ident: "foo".into(),
        paren_token: Default::default(),
        nested: vec![
            NestedMetaItem::MetaItem(MetaItem::Term("bar".into())),
        ].into(),
    })
}

#[test]
fn test_meta_item_list_name_value() {
    run_test("#[foo(bar = 5)]", MetaItemList {
        ident: "foo".into(),
        paren_token: Default::default(),
        nested: vec![
            NestedMetaItem::MetaItem(MetaNameValue {
                ident: "bar".into(),
                eq_token: Default::default(),
                lit: lit(Literal::integer(5))
            }.into()),
        ].into(),
    })
}

#[test]
fn test_meta_item_multiple() {
    run_test("#[foo(word, name = 5, list(name2 = 6), word2)]", MetaItemList {
        ident: "foo".into(),
        paren_token: Default::default(),
        nested: vec![
            NestedMetaItem::MetaItem(MetaItem::Term("word".into())),
            NestedMetaItem::MetaItem(MetaNameValue {
                ident: "name".into(),
                eq_token: Default::default(),
                lit: lit(Literal::integer(5)),
            }.into()),
            NestedMetaItem::MetaItem(MetaItemList {
                ident: "list".into(),
                paren_token: Default::default(),
                nested: vec![
                    NestedMetaItem::MetaItem(MetaNameValue {
                        ident: "name2".into(),
                        eq_token: Default::default(),
                        lit: lit(Literal::integer(6)),
                    }.into())
                ].into(),
            }.into()),
            NestedMetaItem::MetaItem(MetaItem::Term("word2".into())),
        ].into(),
    })
}

fn run_test<T: Into<MetaItem>>(input: &str, expected: T) {
    let tokens = input.parse::<TokenStream>().unwrap();
    let buf = SynomBuffer::new(tokens);
    let attr = match Attribute::parse_outer(buf.begin()) {
        Ok((rest, e)) => {
            assert!(rest.eof());
            e
        }
        Err(_) => panic!("failed to parse"),
    };
    assert_eq!(expected.into(), attr.meta_item().unwrap());
}
