use super::*;

use std::iter;

/// Doc-comments are promoted to attributes that have `is_sugared_doc` = true
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Attribute {
    pub style: AttrStyle,

    /// The path of the attribute.
    ///
    /// E.g. `derive` in `#[derive(Copy)]`
    /// E.g. `crate::precondition` in `#[crate::precondition x < 5]`
    pub path: Path,

    /// Any tokens after the path.
    ///
    /// E.g. `( Copy )` in `#[derive(Copy)]`
    /// E.g. `x < 5` in `#[crate::precondition x < 5]`
    pub tts: Vec<TokenTree>,

    pub is_sugared_doc: bool,
}

impl Attribute {
    /// Parses the tokens after the path as a [`MetaItem`](enum.MetaItem.html) if possible.
    pub fn meta_item(&self) -> Option<MetaItem> {
        let name = if self.path.segments.len() == 1 {
            &self.path.segments[0].ident
        } else {
            return None;
        };

        if self.tts.is_empty() {
            return Some(MetaItem::Word(name.clone()));
        }

        if self.tts.len() == 1 {
            if let TokenTree::Delimited(Delimited { delim: DelimToken::Paren, ref tts }) = self.tts[0] {
                fn nested_meta_item_from_tokens(tts: &[TokenTree]) -> Option<(NestedMetaItem, &[TokenTree])> {
                    assert!(!tts.is_empty());

                    match tts[0] {
                        TokenTree::Token(Token::Literal(ref lit)) => {
                            Some((NestedMetaItem::Literal(lit.clone()), &tts[1..]))
                        }

                        TokenTree::Token(Token::Ident(ref ident)) => {
                            if tts.len() >= 3 {
                                if let TokenTree::Token(Token::Eq) = tts[1] {
                                    if let TokenTree::Token(Token::Literal(ref lit)) = tts[2] {
                                        return Some((NestedMetaItem::MetaItem(MetaItem::NameValue(ident.clone(), lit.clone())), &tts[3..]));
                                    }
                                }
                            }

                            if tts.len() >= 2 {
                                if let TokenTree::Delimited(Delimited { delim: DelimToken::Paren, tts: ref inner_tts }) = tts[1] {
                                    return match list_of_nested_meta_items_from_tokens(vec![], inner_tts) {
                                        Some(nested_meta_items) => {
                                            Some((NestedMetaItem::MetaItem(MetaItem::List(ident.clone(), nested_meta_items)), &tts[2..]))
                                        }

                                        None => None
                                    };
                                }
                            }

                            Some((NestedMetaItem::MetaItem(MetaItem::Word(ident.clone())), &tts[1..]))
                        }

                        _ => None
                    }
                }

                fn list_of_nested_meta_items_from_tokens(mut result: Vec<NestedMetaItem>, tts: &[TokenTree]) -> Option<Vec<NestedMetaItem>> {
                    if tts.is_empty() {
                        return Some(result);
                    }

                    match nested_meta_item_from_tokens(tts) {
                        Some((nested_meta_item, rest)) => {
                            result.push(nested_meta_item);
                            if rest.is_empty() {
                                list_of_nested_meta_items_from_tokens(result, rest)
                            }
                            else if let TokenTree::Token(Token::Comma) = rest[0] {
                                list_of_nested_meta_items_from_tokens(result, &rest[1..])
                            }
                            else {
                                None
                            }
                        }

                        None => None
                    }
                }

                if let Some(nested_meta_items) = list_of_nested_meta_items_from_tokens(vec![], tts) {
                    return Some(MetaItem::List(name.clone(), nested_meta_items));
                }
            }
        }

        if self.tts.len() == 2 {
            if let TokenTree::Token(Token::Eq) = self.tts[0] {
                if let TokenTree::Token(Token::Literal(ref lit)) = self.tts[1] {
                    return Some(MetaItem::NameValue(name.clone(), lit.clone()));
                }
            }
        }

        None
    }
}

/// Distinguishes between Attributes that decorate items and Attributes that
/// are contained as statements within items. These two cases need to be
/// distinguished for pretty-printing.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum AttrStyle {
    /// Attribute of the form `#![...]`.
    Outer,

    /// Attribute of the form `#[...]`.
    Inner,
}

/// A compile-time attribute item.
///
/// E.g. `#[test]`, `#[derive(..)]` or `#[feature = "foo"]`
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum MetaItem {
    /// Word meta item.
    ///
    /// E.g. `test` as in `#[test]`
    Word(Ident),

    /// List meta item.
    ///
    /// E.g. `derive(..)` as in `#[derive(..)]`
    List(Ident, Vec<NestedMetaItem>),

    /// Name-value meta item.
    ///
    /// E.g. `feature = "foo"` as in `#[feature = "foo"]`
    NameValue(Ident, Lit),
}

impl MetaItem {
    /// Name of the item.
    ///
    /// E.g. `test` as in `#[test]`, `derive` as in `#[derive(..)]`, and
    /// `feature` as in `#[feature = "foo"]`.
    pub fn name(&self) -> &str {
        match *self {
            MetaItem::Word(ref name) |
            MetaItem::List(ref name, _) |
            MetaItem::NameValue(ref name, _) => name.as_ref(),
        }
    }
}

/// Possible values inside of compile-time attribute lists.
///
/// E.g. the '..' in `#[name(..)]`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum NestedMetaItem {
    /// A full `MetaItem`.
    ///
    /// E.g. `Copy` in `#[derive(Copy)]` would be a `MetaItem::Word(Ident::from("Copy"))`.
    MetaItem(MetaItem),

    /// A Rust literal.
    ///
    /// E.g. `"name"` in `#[rename("name")]`.
    Literal(Lit),
}

pub trait FilterAttrs<'a> {
    type Ret: Iterator<Item = &'a Attribute>;

    fn outer(self) -> Self::Ret;
    fn inner(self) -> Self::Ret;
}

impl<'a, T> FilterAttrs<'a> for T
    where T: IntoIterator<Item = &'a Attribute>
{
    type Ret = iter::Filter<T::IntoIter, fn(&&Attribute) -> bool>;

    fn outer(self) -> Self::Ret {
        fn is_outer(attr: &&Attribute) -> bool {
            attr.style == AttrStyle::Outer
        }
        self.into_iter().filter(is_outer)
    }

    fn inner(self) -> Self::Ret {
        fn is_inner(attr: &&Attribute) -> bool {
            attr.style == AttrStyle::Inner
        }
        self.into_iter().filter(is_inner)
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use lit::{Lit, StrStyle};
    use mac::{Token, TokenTree};
    use mac::parsing::token_trees;
    use ty::parsing::mod_style_path;
    use synom::{TokenKind, IResult};

    pub fn inner_doc_comment(i: &[synom::TokenTree]) -> IResult<&[synom::TokenTree], String> {
        match i.first() {
            Some(&synom::TokenTree { kind: TokenKind::Literal(ref lit), .. }) => {
                let s = lit.to_string();
                if s.starts_with("//!") || s.starts_with("/*!") {
                    IResult::Done(&i[1..], s)
                } else {
                    IResult::Error
                }
            }
            _ => IResult::Error,
        }
    }

    pub fn outer_doc_comment(i: &[synom::TokenTree]) -> IResult<&[synom::TokenTree], String> {
        match i.first() {
            Some(&synom::TokenTree { kind: TokenKind::Literal(ref lit), .. }) => {
                let s = lit.to_string();
                if s.starts_with("///") || s.starts_with("/**") {
                    IResult::Done(&i[1..], s)
                } else {
                    IResult::Error
                }
            }
            _ => IResult::Error,
        }
    }

    #[cfg(feature = "full")]
    named!(pub inner_attr -> Attribute, alt!(
        do_parse!(
            punct!("#") >>
            punct!("!") >>
            path_and_tts: delim!(Bracket, tuple!(mod_style_path, token_trees)) >>
            ({
                let (path, tts) = path_and_tts;

                Attribute {
                    style: AttrStyle::Inner,
                    path: path,
                    tts: tts,
                    is_sugared_doc: false,
                }
            })
        )
        |
        do_parse!(
            comment: inner_doc_comment >>
            (Attribute {
                style: AttrStyle::Inner,
                path: "doc".into(),
                tts: vec![
                    TokenTree::Token(Token::Eq),
                    TokenTree::Token(Token::Literal(Lit::Str(comment.into(), StrStyle::Cooked))),
                ],
                is_sugared_doc: true,
            })
        )
    ));

    named!(pub outer_attr -> Attribute, alt!(
        do_parse!(
            punct!("#") >>
            path_and_tts: delim!(Bracket, tuple!(mod_style_path, token_trees)) >>
            ({
                let (path, tts) = path_and_tts;

                Attribute {
                    style: AttrStyle::Outer,
                    path: path,
                    tts: tts,
                    is_sugared_doc: false,
                }
            })
        )
        |
        do_parse!(
            comment: outer_doc_comment >>
            (Attribute {
                style: AttrStyle::Outer,
                path: "doc".into(),
                tts: vec![
                    TokenTree::Token(Token::Eq),
                    TokenTree::Token(Token::Literal(Lit::Str(comment.into(), StrStyle::Cooked))),
                ],
                is_sugared_doc: true,
            })
        )
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use lit::{Lit, StrStyle};
    use mac::{Token, TokenTree};
    use quote::{Tokens, ToTokens};
    use ty::Path;

    impl ToTokens for Attribute {
        fn to_tokens(&self, tokens: &mut Tokens) {
            // If this was a sugared doc, emit it in its original form instead of `#[doc = "..."]`
            match *self {
                Attribute {
                    style,
                    path: Path { global: false, ref segments },
                    ref tts,
                    is_sugared_doc: true,
                } if segments.len() == 1 &&
                     segments[0].ident == "doc" &&
                     segments[0].parameters.is_empty() &&
                     tts.len() == 2 =>
                {
                    if let TokenTree::Token(Token::Eq) = self.tts[0] {
                        if let TokenTree::Token(Token::Literal(Lit::Str(ref value, StrStyle::Cooked))) = self.tts[1] {
                            match style {
                                AttrStyle::Inner if value.starts_with("//!") => {
                                    tokens.append(&format!("{}\n", value));
                                    return;
                                }
                                AttrStyle::Inner if value.starts_with("/*!") => {
                                    tokens.append(value);
                                    return;
                                }
                                AttrStyle::Outer if value.starts_with("///") => {
                                    tokens.append(&format!("{}\n", value));
                                    return;
                                }
                                AttrStyle::Outer if value.starts_with("/**") => {
                                    tokens.append(value);
                                    return;
                                }
                                _ => {}
                            }
                        }
                    }
                }

                _ => {}
            }

            tokens.append("#");
            if let AttrStyle::Inner = self.style {
                tokens.append("!");
            }
            tokens.append("[");
            self.path.to_tokens(tokens);
            tokens.append_all(&self.tts);
            tokens.append("]");
        }
    }

    impl ToTokens for MetaItem {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                MetaItem::Word(ref ident) => {
                    ident.to_tokens(tokens);
                }
                MetaItem::List(ref ident, ref inner) => {
                    ident.to_tokens(tokens);
                    tokens.append("(");
                    tokens.append_separated(inner, ",");
                    tokens.append(")");
                }
                MetaItem::NameValue(ref name, ref value) => {
                    name.to_tokens(tokens);
                    tokens.append("=");
                    value.to_tokens(tokens);
                }
            }
        }
    }

    impl ToTokens for NestedMetaItem {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                NestedMetaItem::MetaItem(ref nested) => {
                    nested.to_tokens(tokens);
                }
                NestedMetaItem::Literal(ref lit) => {
                    lit.to_tokens(tokens);
                }
            }
        }
    }
}
