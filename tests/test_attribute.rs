extern crate syn;

mod features;

#[macro_use]
mod macros;

use syn::parse::Parser;
use syn::Attribute;

#[test]
fn test_meta_item_path() {
    test("#[foo]")
}

#[test]
fn test_meta_item_path_multiple_segments() {
    test("#[foo::bar]")
}

#[test]
fn test_meta_item_path_leading_colon() {
    test("#[::foo::bar]")
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
fn test_meta_item_name_value_multiple_segments() {
    test("#[foo::bar = 5]")
}

#[test]
fn test_meta_item_name_value_leading_colon() {
    test("#[::foo::bar = 5]")
}

#[test]
fn test_meta_item_list_lit() {
    test("#[foo(5)]")
}

#[test]
fn test_meta_item_list_path() {
    test("#[foo(bar::Baz)]")
}

#[test]
fn test_meta_item_list_path_multiple_segments() {
    test("#[foo::bar(baz::Qux)]")
}

#[test]
fn test_meta_item_list_path_leading_colon() {
    test("#[::foo::bar(::baz::Qux)]")
}

#[test]
fn test_meta_item_list_name_value() {
    test("#[foo(bar = 5)]")
}

#[test]
fn test_meta_item_list_name_value_multiple_segments() {
    test("#[foo(bar::baz = 5)]")
}

#[test]
fn test_meta_item_list_bool_value() {
    test("#[foo(bar = true)]")
}

#[test]
fn test_meta_item_list_bool_value_multiple_segments() {
    test("#[foo(bar::baz = true)]")
}

#[test]
fn test_meta_item_multiple() {
    test("#[foo(word, name = 5, list(name2 = 6), word2)]")
}

#[test]
fn test_meta_item_multiple_with_path() {
    test("#[foo::bar(word, name = 5, list(name2 = 6), word2, ::path::true)]")
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
