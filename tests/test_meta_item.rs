#![cfg(feature = "extra-traits")]

extern crate syn;
use syn::*;

#[test]
fn test_meta_item_word() {
    run_test("#[foo]", MetaItem::Word("foo".into()))
}

#[test]
fn test_meta_item_name_value() {
    run_test("#[foo = 5]", MetaNameValue {
        ident: "foo".into(),
        lit: Lit::Int(5, IntTy::Unsuffixed),
    })
}

#[test]
fn test_meta_item_list_lit() {
    run_test("#[foo(5)]", MetaItemList {
        ident: "foo".into(),
        nested: vec![
            NestedMetaItem::Literal(Lit::Int(5, IntTy::Unsuffixed)),
        ],
    })
}

#[test]
fn test_meta_item_list_word() {
    run_test("#[foo(bar)]", MetaItemList {
        ident: "foo".into(),
        nested: vec![
            NestedMetaItem::MetaItem(MetaItem::Word("bar".into())),
        ],
    })
}

#[test]
fn test_meta_item_list_name_value() {
    run_test("#[foo(bar = 5)]", MetaItemList {
        ident: "foo".into(),
        nested: vec![
            NestedMetaItem::MetaItem(MetaNameValue {
                ident: "bar".into(),
                lit: Lit::Int(5, IntTy::Unsuffixed),
            }.into()),
        ],
    })
}

#[test]
fn test_meta_item_multiple() {
    run_test("#[foo(word, name = 5, list(name2 = 6), word2)]", MetaItemList {
        ident: "foo".into(),
        nested: vec![
            NestedMetaItem::MetaItem(MetaItem::Word("word".into())),
            NestedMetaItem::MetaItem(MetaNameValue {
                ident: "name".into(),
                lit: Lit::Int(5, IntTy::Unsuffixed),
            }.into()),
            NestedMetaItem::MetaItem(MetaItemList {
                ident: "list".into(),
                nested: vec![
                    NestedMetaItem::MetaItem(MetaNameValue {
                        ident: "name2".into(),
                        lit: Lit::Int(6, IntTy::Unsuffixed),
                    }.into())
                ],
            }.into()),
            NestedMetaItem::MetaItem(MetaItem::Word("word2".into())),
        ],
    })
}

fn run_test<T: Into<MetaItem>>(input: &str, expected: T) {
    let attr = parse_outer_attr(input).unwrap();
    assert_eq!(expected.into(), attr.meta_item().unwrap());
}
