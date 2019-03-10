extern crate syn;

mod features;

#[macro_use]
mod macros;

use syn::parse::Parser;
use syn::Attribute;

#[test]
fn test_meta_item_word() {
    test("#[foo]")
}

#[test]
fn test_meta_item_name_value() {
    test("#[foo = 5]")
}

#[test]
fn test_meta_item_bool_value() {
    test("#[foo = true]");
    test("#[foo = false]")
}

#[test]
fn test_meta_item_list_lit() {
    test("#[foo(5)]")
}

#[test]
fn test_meta_item_list_word() {
    test("#[foo(bar)]")
}

#[test]
fn test_meta_item_list_name_value() {
    test("#[foo(bar = 5)]")
}

#[test]
fn test_meta_item_list_bool_value() {
    test("#[foo(bar = true)]")
}

#[test]
fn test_meta_item_multiple() {
    test("#[foo(word, name = 5, list(name2 = 6), word2)]")
}

#[test]
fn test_bool_lit() {
    test("#[foo(true)]")
}

fn test(input: &str) {
    let attrs = Attribute::parse_outer.parse_str(input).unwrap();

    assert_eq!(attrs.len(), 1);

    let attr = attrs.into_iter().next().unwrap();
    let interpret = snapshot!(attr.interpret_meta().unwrap());
    let parse = snapshot!(attr.parse_meta().unwrap());

    assert_eq!(interpret, parse);
}
