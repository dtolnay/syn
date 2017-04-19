extern crate syn;
use syn::*;

#[test]
fn test_meta_item_word() {
    run_test("#[foo]", MetaItem::Word("foo".into()))
}

#[test]
fn test_meta_item_name_value() {
    run_test("#[foo = 5]", MetaItem::NameValue("foo".into(),
        Lit::Int(5, IntTy::Unsuffixed)))
}

#[test]
fn test_meta_item_list_lit() {
    run_test("#[foo(5)]", MetaItem::List("foo".into(), vec![
        NestedMetaItem::Literal(Lit::Int(5, IntTy::Unsuffixed)),
    ]))
}

#[test]
fn test_meta_item_list_word() {
    run_test("#[foo(bar)]", MetaItem::List("foo".into(), vec![
        NestedMetaItem::MetaItem(MetaItem::Word("bar".into())),
    ]))
}

#[test]
fn test_meta_item_list_name_value() {
    run_test("#[foo(bar = 5)]", MetaItem::List("foo".into(), vec![
        NestedMetaItem::MetaItem(MetaItem::NameValue("bar".into(),
            Lit::Int(5, IntTy::Unsuffixed))),
    ]))
}

#[test]
fn test_meta_item_multiple() {
    run_test("#[foo(word, name = 5, list(name2 = 6), word2)]", MetaItem::List("foo".into(), vec![
        NestedMetaItem::MetaItem(MetaItem::Word("word".into())),
        NestedMetaItem::MetaItem(MetaItem::NameValue("name".into(),
            Lit::Int(5, IntTy::Unsuffixed))),
        NestedMetaItem::MetaItem(MetaItem::List("list".into(), vec![
            NestedMetaItem::MetaItem(MetaItem::NameValue("name2".into(),
                Lit::Int(6, IntTy::Unsuffixed)))
        ])),
        NestedMetaItem::MetaItem(MetaItem::Word("word2".into())),
    ]))
}

fn run_test(input: &str, expected: MetaItem) {
    let attr = parse_outer_attr(input).unwrap();
    assert_eq!(expected, attr.meta_item().unwrap());
}
