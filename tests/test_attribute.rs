extern crate syn;

mod features;

#[macro_use]
mod macros;

use syn::parse::Parser;
use syn::{Attribute, Meta};

#[test]
fn test_meta_item_word() {
    let meta = test("#[foo]");

    snapshot!(meta, @r###"Word("foo")"###);
}

#[test]
fn test_meta_item_name_value() {
    let meta = test("#[foo = 5]");

    snapshot!(meta, @r###"
   ⋮Meta::NameValue {
   ⋮    ident: "foo",
   ⋮    lit: 5,
   ⋮}
    "###);
}

#[test]
fn test_meta_item_bool_value() {
    let meta = test("#[foo = true]");;

    snapshot!(meta, @r###"
   ⋮Meta::NameValue {
   ⋮    ident: "foo",
   ⋮    lit: Lit::Bool {
   ⋮        value: true,
   ⋮    },
   ⋮}
    "###);

    let meta = test("#[foo = false]");

    snapshot!(meta, @r###"
   ⋮Meta::NameValue {
   ⋮    ident: "foo",
   ⋮    lit: Lit::Bool {
   ⋮        value: false,
   ⋮    },
   ⋮}
    "###);
}

#[test]
fn test_meta_item_list_lit() {
    let meta = test("#[foo(5)]");

    snapshot!(meta, @r###"
   ⋮Meta::List {
   ⋮    ident: "foo",
   ⋮    nested: [
   ⋮        Literal(5),
   ⋮    ],
   ⋮}
    "###);
}

#[test]
fn test_meta_item_list_word() {
    let meta = test("#[foo(bar)]");

    snapshot!(meta, @r###"
   ⋮Meta::List {
   ⋮    ident: "foo",
   ⋮    nested: [
   ⋮        Meta(Word("bar")),
   ⋮    ],
   ⋮}
    "###);
}

#[test]
fn test_meta_item_list_name_value() {
    let meta = test("#[foo(bar = 5)]");

    snapshot!(meta, @r###"
   ⋮Meta::List {
   ⋮    ident: "foo",
   ⋮    nested: [
   ⋮        Meta(Meta::NameValue {
   ⋮            ident: "bar",
   ⋮            lit: 5,
   ⋮        }),
   ⋮    ],
   ⋮}
    "###);
}

#[test]
fn test_meta_item_list_bool_value() {
    let meta = test("#[foo(bar = true)]");

    snapshot!(meta, @r###"
   ⋮Meta::List {
   ⋮    ident: "foo",
   ⋮    nested: [
   ⋮        Meta(Meta::NameValue {
   ⋮            ident: "bar",
   ⋮            lit: Lit::Bool {
   ⋮                value: true,
   ⋮            },
   ⋮        }),
   ⋮    ],
   ⋮}
    "###);
}

#[test]
fn test_meta_item_multiple() {
    let meta = test("#[foo(word, name = 5, list(name2 = 6), word2)]");

    snapshot!(meta, @r###"
   ⋮Meta::List {
   ⋮    ident: "foo",
   ⋮    nested: [
   ⋮        Meta(Word("word")),
   ⋮        Meta(Meta::NameValue {
   ⋮            ident: "name",
   ⋮            lit: 5,
   ⋮        }),
   ⋮        Meta(Meta::List {
   ⋮            ident: "list",
   ⋮            nested: [
   ⋮                Meta(Meta::NameValue {
   ⋮                    ident: "name2",
   ⋮                    lit: 6,
   ⋮                }),
   ⋮            ],
   ⋮        }),
   ⋮        Meta(Word("word2")),
   ⋮    ],
   ⋮}
    "###);
}

#[test]
fn test_bool_lit() {
    let meta = test("#[foo(true)]");

    snapshot!(meta, @r###"
   ⋮Meta::List {
   ⋮    ident: "foo",
   ⋮    nested: [
   ⋮        Literal(Lit::Bool {
   ⋮            value: true,
   ⋮        }),
   ⋮    ],
   ⋮}
    "###);
}

fn test(input: &str) -> Meta {
    let attrs = Attribute::parse_outer.parse_str(input).unwrap();

    assert_eq!(attrs.len(), 1);
    let attr = attrs.into_iter().next().unwrap();

    attr.parse_meta().unwrap()
}
