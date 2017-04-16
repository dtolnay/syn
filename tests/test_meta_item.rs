extern crate syn;
use syn::*;

#[test]
fn test_meta_item_word() {
    run_test("#[foo]", MetaItem::Word)
}

#[test]
fn test_meta_item_name_value() {
    run_test("#[foo = 5]", MetaItem::NameValue(Lit::Int(5, IntTy::Unsuffixed)))
}

#[test]
fn test_meta_item_list_lit() {
    run_test("#[foo(5)]", MetaItem::List(vec![NestedMetaItem::Literal(Lit::Int(5, IntTy::Unsuffixed))]))
}

#[test]
fn test_meta_item_list_word() {
    run_test("#[foo(bar)]", MetaItem::List(vec![NestedMetaItem::MetaItem(Ident::from("bar"), MetaItem::Word)]))
}

#[test]
fn test_meta_item_list_name_value() {
    run_test("#[foo(bar = 5)]", MetaItem::List(vec![NestedMetaItem::MetaItem(Ident::from("bar"), MetaItem::NameValue(Lit::Int(5, IntTy::Unsuffixed)))]))
}

#[test]
fn test_meta_item_multiple() {
    run_test("#[foo(word, name = 5, list(name2 = 6), word2)]", MetaItem::List(vec![
        NestedMetaItem::MetaItem(Ident::from("word"), MetaItem::Word),
        NestedMetaItem::MetaItem(Ident::from("name"), MetaItem::NameValue(Lit::Int(5, IntTy::Unsuffixed))),
        NestedMetaItem::MetaItem(Ident::from("list"), MetaItem::List(vec![
            NestedMetaItem::MetaItem(Ident::from("name2"), MetaItem::NameValue(Lit::Int(6, IntTy::Unsuffixed)))
        ])),
        NestedMetaItem::MetaItem(Ident::from("word2"), MetaItem::Word),
    ]))
}

fn run_test(input: &str, expected: MetaItem) {
    let attr = parse_outer_attr(input).unwrap();
    assert_eq!(expected, attr.meta_item().unwrap());
}
