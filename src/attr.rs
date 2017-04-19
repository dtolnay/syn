use super::*;

use std::iter;

/// Doc-comments are promoted to attributes that have `is_sugared_doc` = true
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Attribute {
    pub style: AttrStyle,
    pub path: Path,
    pub tts: Vec<TokenTree>,
    pub is_sugared_doc: bool,
}

impl Attribute {
    pub fn meta_item(&self) -> Option<MetaItem> {
        if self.tts.is_empty() {
            return Some(MetaItem::Word);
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
                                match (&tts[1], &tts[2]) {
                                    (&TokenTree::Token(Token::Eq), &TokenTree::Token(Token::Literal(ref lit))) => {
                                        return Some((NestedMetaItem::MetaItem(ident.clone(), MetaItem::NameValue(lit.clone())), &tts[3..]));
                                    }

                                    _ => {}
                                }
                            }

                            if tts.len() >= 2 {
                                if let TokenTree::Delimited(Delimited { delim: DelimToken::Paren, tts: ref inner_tts }) = tts[1] {
                                    return match list_of_nested_meta_items_from_tokens(vec![], inner_tts) {
                                        Some(nested_meta_items) => {
                                            Some((NestedMetaItem::MetaItem(ident.clone(), MetaItem::List(nested_meta_items)), &tts[2..]))
                                        }

                                        None => None
                                    };
                                }
                            }

                            Some((NestedMetaItem::MetaItem(ident.clone(), MetaItem::Word), &tts[1..]))
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
                    return Some(MetaItem::List(nested_meta_items));
                }
            }
        }

        if self.tts.len() == 2 {
            match (&self.tts[0], &self.tts[1]) {
                (&TokenTree::Token(Token::Eq), &TokenTree::Token(Token::Literal(ref lit))) => {
                    return Some(MetaItem::NameValue(lit.clone()));
                }

                _ => {}
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
    Word,

    /// List meta item.
    ///
    /// E.g. `derive(..)` as in `#[derive(..)]`
    List(Vec<NestedMetaItem>),

    /// Name-value meta item.
    ///
    /// E.g. `feature = "foo"` as in `#[feature = "foo"]`
    NameValue(Lit),
}

/// Possible values inside of compile-time attribute lists.
///
/// E.g. the '..' in `#[name(..)]`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum NestedMetaItem {
    /// A full `MetaItem`.
    ///
    /// E.g. `Copy` in `#[derive(Copy)]` would be a `(Ident::from("Copy"), MetaItem::Word)`.
    MetaItem(Ident, MetaItem),

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
    use synom::space::{block_comment, whitespace};
    use ty::parsing::path;

    #[cfg(feature = "full")]
    named!(pub inner_attr -> Attribute, alt!(
        do_parse!(
            punct!("#") >>
            punct!("!") >>
            path_and_tts: delimited!(
                punct!("["),
                tuple!(path, token_trees),
                punct!("]")
            ) >>
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
            punct!("//!") >>
            content: take_until!("\n") >>
            (Attribute {
                style: AttrStyle::Inner,
                path: "doc".into(),
                tts: vec![
                    TokenTree::Token(Token::Eq),
                    TokenTree::Token(Token::Literal(Lit::Str(format!("//!{}", content).into(), StrStyle::Cooked))),
                ],
                is_sugared_doc: true,
            })
        )
        |
        do_parse!(
            option!(whitespace) >>
            peek!(tag!("/*!")) >>
            com: block_comment >>
            (Attribute {
                style: AttrStyle::Inner,
                path: "doc".into(),
                tts: vec![
                    TokenTree::Token(Token::Eq),
                    TokenTree::Token(Token::Literal(Lit::Str(com.into(), StrStyle::Cooked))),
                ],
                is_sugared_doc: true,
            })
        )
    ));

    named!(pub outer_attr -> Attribute, alt!(
        do_parse!(
            punct!("#") >>
            path_and_tts: delimited!(
                punct!("["),
                tuple!(path, token_trees),
                punct!("]")
            ) >>
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
            punct!("///") >>
            not!(tag!("/")) >>
            content: take_until!("\n") >>
            (Attribute {
                style: AttrStyle::Outer,
                path: "doc".into(),
                tts: vec![
                    TokenTree::Token(Token::Eq),
                    TokenTree::Token(Token::Literal(Lit::Str(format!("///{}", content).into(), StrStyle::Cooked))),
                ],
                is_sugared_doc: true,
            })
        )
        |
        do_parse!(
            option!(whitespace) >>
            peek!(tuple!(tag!("/**"), not!(tag!("*")))) >>
            com: block_comment >>
            (Attribute {
                style: AttrStyle::Outer,
                path: "doc".into(),
                tts: vec![
                    TokenTree::Token(Token::Eq),
                    TokenTree::Token(Token::Literal(Lit::Str(com.into(), StrStyle::Cooked))),
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

    impl ToTokens for Attribute {
        fn to_tokens(&self, tokens: &mut Tokens) {
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
                    match (&self.tts[0], &self.tts[1]) {
                        (&TokenTree::Token(Token::Eq),
                         &TokenTree::Token(Token::Literal(Lit::Str(ref value, StrStyle::Cooked)))) =>
                        {
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

                        _ => {}
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
                MetaItem::Word => {}
                MetaItem::List(ref inner) => {
                    tokens.append("(");
                    tokens.append_separated(inner, ",");
                    tokens.append(")");
                }
                MetaItem::NameValue(ref value) => {
                    tokens.append("=");
                    value.to_tokens(tokens);
                }
            }
        }
    }

    impl ToTokens for NestedMetaItem {
        fn to_tokens(&self, tokens: &mut Tokens) {
            match *self {
                NestedMetaItem::MetaItem(ref ident, ref nested) => {
                    ident.to_tokens(tokens);
                    nested.to_tokens(tokens);
                }
                NestedMetaItem::Literal(ref lit) => {
                    lit.to_tokens(tokens);
                }
            }
        }
    }
}
