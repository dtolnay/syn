extern crate syn;

mod features;

#[macro_use]
mod macros;

use syn::parse::Parse;
use syn::{Meta, MetaList, MetaNameValue, NestedMeta};
use std::fmt::Debug;

#[test]
fn test_parse_meta_item_word() {
    let code = "hello";

    snapshot!(code as Meta);
}

#[test]
fn test_parse_meta_name_value() {
    test::<MetaNameValue>("foo = 5");
}

#[test]
fn test_parse_meta_name_value_with_path() {
    test::<MetaNameValue>("foo::bar = 5");
}

#[test]
fn test_parse_meta_name_value_with_keyword() {
    test::<MetaNameValue>("static = 5");
}

#[test]
fn test_parse_meta_name_value_with_keyword_and_path() {
    test::<MetaNameValue>("extern::static = 5");
}

#[test]
fn test_parse_meta_name_value_with_bool() {
    test::<MetaNameValue>("true = 5");
}

#[test]
fn test_parse_meta_item_list_lit() {
    test::<MetaList>("foo(5)");
}

#[test]
fn test_parse_meta_item_list_with_path() {
    test::<MetaList>("foo::bar(5)");
}

#[test]
fn test_parse_meta_item_multiple() {
    test::<MetaList>("foo(word, name = 5, list(name2 = 6), word2)");
}

#[test]
fn test_parse_meta_item_multiple_with_path() {
    test::<MetaList>("foo::bar(word, name = 5, list(name2 = 6), word2, path::true)");
}

#[test]
fn test_parse_nested_meta() {
    let code = "5";
    snapshot!(code as NestedMeta);

    let code = "list(name2 = 6)";
    snapshot!(code as NestedMeta);
}

fn test<T>(input: &str)
where
    T: Parse + Into<Meta> + Debug,
{
    let inner = snapshot!(input as T);
    let meta = snapshot!(input as Meta);

    assert_eq!(meta, inner.into());
}
