use super::*;

use common::word;
use helper::escaped_string;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Attribute {
    pub value: MetaItem,
    pub is_sugared_doc: bool,
}

/// A compile-time attribute item.
///
/// E.g. `#[test]`, `#[derive(..)]` or `#[feature = "foo"]`
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MetaItem {
    /// Word meta item.
    ///
    /// E.g. `test` as in `#[test]`
    Word(Ident),
    /// List meta item.
    ///
    /// E.g. `derive(..)` as in `#[derive(..)]`
    List(Ident, Vec<MetaItem>),
    /// Name value meta item.
    ///
    /// E.g. `feature = "foo"` as in `#[feature = "foo"]`
    NameValue(Ident, String),
}

named!(pub attribute<&str, Attribute>, chain!(
    punct!("#") ~
    punct!("[") ~
    meta_item: meta_item ~
    punct!("]"),
    move || Attribute {
        value: meta_item,
        is_sugared_doc: false,
    }
));

named!(quoted<&str, String>, delimited!(
    punct!("\""),
    escaped_string,
    tag_s!("\"")
));

named!(meta_item<&str, MetaItem>, alt!(
    chain!(
        ident: word ~
        punct!("(") ~
        inner: separated_list!(punct!(","), meta_item) ~
        punct!(")"),
        move || MetaItem::List(ident, inner)
    )
    |
    chain!(
        ident: word ~
        punct!("=") ~
        string: quoted,
        move || MetaItem::NameValue(ident, string)
    )
    |
    map!(word, MetaItem::Word)
));
