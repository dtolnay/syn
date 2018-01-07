// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use super::*;
use punctuated::Punctuated;

use std::iter;

use proc_macro2::{Delimiter, Spacing, TokenNode, TokenStream, TokenTree};

#[cfg(feature = "extra-traits")]
use std::hash::{Hash, Hasher};
#[cfg(feature = "extra-traits")]
use tt::TokenStreamHelper;

ast_struct! {
    /// An attribute like `#[repr(transparent)]`.
    ///
    /// # Syntax
    ///
    /// Rust has six types of attributes.
    ///
    /// - Outer attributes like `#[repr(transparent)]`. These appear outside or
    ///   in front of the item they describe.
    /// - Inner attributes like `#![feature(proc_macro)]`. These appear inside
    ///   of the item they describe, usually a module.
    /// - Outer doc comments like `/// # Example`.
    /// - Inner doc comments like `//! Please file an issue`.
    /// - Outer block comments `/** # Example */`.
    /// - Inner block comments `/*! Please file an issue */`.
    ///
    /// The `style` field of type `AttrStyle` distinguishes whether an attribute
    /// is outer or inner. Doc comments and block comments are promoted to
    /// attributes that have `is_sugared_doc` set to true, as this is how they
    /// are processed by the compiler and by `macro_rules!` macros.
    ///
    /// The `path` field gives the possibly colon-delimited path against which
    /// the attribute is resolved. It is equal to `"doc"` for desugared doc
    /// comments. The `tts` field contains the rest of the attribute body as
    /// tokens.
    ///
    /// ```text
    /// #[derive(Copy)]      #[crate::precondition x < 5]
    ///   ^^^^^^~~~~~~         ^^^^^^^^^^^^^^^^^^^ ~~~~~
    ///    path  tts                   path         tts
    /// ```
    ///
    /// Use the [`meta_item`] method to try parsing the tokens of an attribute
    /// into the structured representation that is used by convention across
    /// most Rust libraries.
    ///
    /// [`meta_item`]: #method.meta_item
    pub struct Attribute #manual_extra_traits {
        pub pound_token: Token![#],
        pub style: AttrStyle,
        pub bracket_token: token::Bracket,
        pub path: Path,
        pub tts: TokenStream,
        pub is_sugared_doc: bool,
    }
}

#[cfg(feature = "extra-traits")]
impl Eq for Attribute {}

#[cfg(feature = "extra-traits")]
impl PartialEq for Attribute {
    fn eq(&self, other: &Self) -> bool {
        self.style == other.style && self.pound_token == other.pound_token
            && self.bracket_token == other.bracket_token && self.path == other.path
            && TokenStreamHelper(&self.tts) == TokenStreamHelper(&other.tts)
            && self.is_sugared_doc == other.is_sugared_doc
    }
}

#[cfg(feature = "extra-traits")]
impl Hash for Attribute {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.style.hash(state);
        self.pound_token.hash(state);
        self.bracket_token.hash(state);
        self.path.hash(state);
        TokenStreamHelper(&self.tts).hash(state);
        self.is_sugared_doc.hash(state);
    }
}

impl Attribute {
    /// Parses the tokens after the path as a [`MetaItem`](enum.MetaItem.html) if possible.
    pub fn meta_item(&self) -> Option<MetaItem> {
        let name = if self.path.segments.len() == 1 {
            &self.path.segments.first().unwrap().value().ident
        } else {
            return None;
        };

        if self.tts.is_empty() {
            return Some(MetaItem::Term(*name));
        }

        let tts = self.tts.clone().into_iter().collect::<Vec<_>>();

        if tts.len() == 1 {
            if let TokenNode::Group(Delimiter::Parenthesis, ref ts) = tts[0].kind {
                let tokens = ts.clone().into_iter().collect::<Vec<_>>();
                if let Some(nested_meta_items) = list_of_nested_meta_items_from_tokens(&tokens) {
                    return Some(MetaItem::List(MetaItemList {
                        paren_token: token::Paren(tts[0].span),
                        ident: *name,
                        nested: nested_meta_items,
                    }));
                }
            }
        }

        if tts.len() == 2 {
            if let TokenNode::Op('=', Spacing::Alone) = tts[0].kind {
                if let TokenNode::Literal(ref lit) = tts[1].kind {
                    return Some(MetaItem::NameValue(MetaNameValue {
                        ident: *name,
                        eq_token: Token![=]([tts[0].span]),
                        lit: Lit::new(lit.clone(), tts[1].span),
                    }));
                }
            }
        }

        None
    }
}

fn nested_meta_item_from_tokens(tts: &[TokenTree]) -> Option<(NestedMetaItem, &[TokenTree])> {
    assert!(!tts.is_empty());

    match tts[0].kind {
        TokenNode::Literal(ref lit) => {
            let lit = Lit::new(lit.clone(), tts[0].span);
            Some((NestedMetaItem::Literal(lit), &tts[1..]))
        }

        TokenNode::Term(sym) => {
            let ident = Ident::new(sym.as_str(), tts[0].span);
            if tts.len() >= 3 {
                if let TokenNode::Op('=', Spacing::Alone) = tts[1].kind {
                    if let TokenNode::Literal(ref lit) = tts[2].kind {
                        let pair = MetaNameValue {
                            ident: Ident::new(sym.as_str(), tts[0].span),
                            eq_token: Token![=]([tts[1].span]),
                            lit: Lit::new(lit.clone(), tts[2].span),
                        };
                        return Some((MetaItem::NameValue(pair).into(), &tts[3..]));
                    }
                }
            }

            if tts.len() >= 2 {
                if let TokenNode::Group(Delimiter::Parenthesis, ref inner_tts) = tts[1].kind {
                    let inner_tts = inner_tts.clone().into_iter().collect::<Vec<_>>();
                    return match list_of_nested_meta_items_from_tokens(&inner_tts) {
                        Some(nested_meta_items) => {
                            let list = MetaItemList {
                                ident: ident,
                                paren_token: token::Paren(tts[1].span),
                                nested: nested_meta_items,
                            };
                            Some((MetaItem::List(list).into(), &tts[2..]))
                        }

                        None => None,
                    };
                }
            }

            Some((MetaItem::Term(ident).into(), &tts[1..]))
        }

        _ => None,
    }
}

fn list_of_nested_meta_items_from_tokens(
    mut tts: &[TokenTree],
) -> Option<Punctuated<NestedMetaItem, Token![,]>> {
    let mut nested_meta_items = Punctuated::new();
    let mut first = true;

    while !tts.is_empty() {
        let prev_comma = if first {
            first = false;
            None
        } else if let TokenNode::Op(',', Spacing::Alone) = tts[0].kind {
            let tok = Token![,]([tts[0].span]);
            tts = &tts[1..];
            if tts.is_empty() {
                break;
            }
            Some(tok)
        } else {
            return None;
        };
        let (nested, rest) = match nested_meta_item_from_tokens(tts) {
            Some(pair) => pair,
            None => return None,
        };
        if let Some(comma) = prev_comma {
            nested_meta_items.push_punct(comma);
        }
        nested_meta_items.push_value(nested);
        tts = rest;
    }

    Some(nested_meta_items)
}

ast_enum! {
    /// Distinguishes between Attributes that decorate items and Attributes that
    /// are contained as statements within items.
    ///
    /// # Outer attributes
    ///
    /// - `#[repr(transparent)]`
    /// - `/// # Example`
    /// - `/** Please file an issue */`
    ///
    /// # Inner attributes
    ///
    /// - `#![feature(proc_macro)]`
    /// - `//! # Example`
    /// - `/*! Please file an issue */`
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum AttrStyle {
        Outer,
        Inner(Token![!]),
    }
}

ast_enum_of_structs! {
    /// A compile-time attribute item.
    ///
    /// E.g. `#[test]`, `#[derive(..)]` or `#[feature = "foo"]`
    pub enum MetaItem {
        /// Term meta item.
        ///
        /// E.g. `test` as in `#[test]`
        pub Term(Ident),

        /// List meta item.
        ///
        /// E.g. `derive(..)` as in `#[derive(..)]`
        pub List(MetaItemList {
            /// Name of this attribute.
            ///
            /// E.g. `derive` in `#[derive(..)]`
            pub ident: Ident,

            pub paren_token: token::Paren,

            /// Arguments to this attribute
            ///
            /// E.g. `..` in `#[derive(..)]`
            pub nested: Punctuated<NestedMetaItem, Token![,]>,
        }),

        /// Name-value meta item.
        ///
        /// E.g. `feature = "foo"` as in `#[feature = "foo"]`
        pub NameValue(MetaNameValue {
            /// Name of this attribute.
            ///
            /// E.g. `feature` in `#[feature = "foo"]`
            pub ident: Ident,

            pub eq_token: Token![=],

            /// Arguments to this attribute
            ///
            /// E.g. `"foo"` in `#[feature = "foo"]`
            pub lit: Lit,
        }),
    }
}

impl MetaItem {
    /// Name of the item.
    ///
    /// E.g. `test` as in `#[test]`, `derive` as in `#[derive(..)]`, and
    /// `feature` as in `#[feature = "foo"]`.
    pub fn name(&self) -> &str {
        match *self {
            MetaItem::Term(ref name) => name.as_ref(),
            MetaItem::NameValue(ref pair) => pair.ident.as_ref(),
            MetaItem::List(ref list) => list.ident.as_ref(),
        }
    }
}

ast_enum_of_structs! {
    /// Possible values inside of compile-time attribute lists.
    ///
    /// E.g. the '..' in `#[name(..)]`.
    pub enum NestedMetaItem {
        /// A full `MetaItem`.
        ///
        /// E.g. `Copy` in `#[derive(Copy)]` would be a `MetaItem::Term(Ident::from("Copy"))`.
        pub MetaItem(MetaItem),

        /// A Rust literal.
        ///
        /// E.g. `"name"` in `#[rename("name")]`.
        pub Literal(Lit),
    }
}

pub trait FilterAttrs<'a> {
    type Ret: Iterator<Item = &'a Attribute>;

    fn outer(self) -> Self::Ret;
    fn inner(self) -> Self::Ret;
}

impl<'a, T> FilterAttrs<'a> for T
where
    T: IntoIterator<Item = &'a Attribute>,
{
    type Ret = iter::Filter<T::IntoIter, fn(&&Attribute) -> bool>;

    fn outer(self) -> Self::Ret {
        fn is_outer(attr: &&Attribute) -> bool {
            match attr.style {
                AttrStyle::Outer => true,
                _ => false,
            }
        }
        self.into_iter().filter(is_outer)
    }

    fn inner(self) -> Self::Ret {
        fn is_inner(attr: &&Attribute) -> bool {
            match attr.style {
                AttrStyle::Inner(_) => true,
                _ => false,
            }
        }
        self.into_iter().filter(is_inner)
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use buffer::Cursor;
    use parse_error;
    use synom::PResult;
    use proc_macro2::{Literal, Spacing, Span, TokenNode, TokenTree};

    fn eq(span: Span) -> TokenTree {
        TokenTree {
            span: span,
            kind: TokenNode::Op('=', Spacing::Alone),
        }
    }

    impl Attribute {
        named!(pub parse_inner -> Self, alt!(
            do_parse!(
                pound: punct!(#) >>
                bang: punct!(!) >>
                path_and_tts: brackets!(tuple!(
                    call!(Path::parse_mod_style),
                    syn!(TokenStream)
                )) >>
                ({
                    let (bracket, (path, tts)) = path_and_tts;

                    Attribute {
                        style: AttrStyle::Inner(bang),
                        path: path,
                        tts: tts,
                        is_sugared_doc: false,
                        pound_token: pound,
                        bracket_token: bracket,
                    }
                })
            )
            |
            map!(
                call!(lit_doc_comment, Comment::Inner),
                |lit| {
                    let span = lit.span;
                    Attribute {
                        style: AttrStyle::Inner(<Token![!]>::new(span)),
                        path: Ident::new("doc", span).into(),
                        tts: vec![
                            eq(span),
                            lit,
                        ].into_iter().collect(),
                        is_sugared_doc: true,
                        pound_token: <Token![#]>::new(span),
                        bracket_token: token::Bracket(span),
                    }
                }
            )
        ));

        named!(pub parse_outer -> Self, alt!(
            do_parse!(
                pound: punct!(#) >>
                path_and_tts: brackets!(tuple!(
                    call!(Path::parse_mod_style),
                    syn!(TokenStream)
                )) >>
                ({
                    let (bracket, (path, tts)) = path_and_tts;

                    Attribute {
                        style: AttrStyle::Outer,
                        path: path,
                        tts: tts,
                        is_sugared_doc: false,
                        pound_token: pound,
                        bracket_token: bracket,
                    }
                })
            )
            |
            map!(
                call!(lit_doc_comment, Comment::Outer),
                |lit| {
                    let span = lit.span;
                    Attribute {
                        style: AttrStyle::Outer,
                        path: Ident::new("doc", span).into(),
                        tts: vec![
                            eq(span),
                            lit,
                        ].into_iter().collect(),
                        is_sugared_doc: true,
                        pound_token: <Token![#]>::new(span),
                        bracket_token: token::Bracket(span),
                    }
                }
            )
        ));
    }

    enum Comment {
        Inner,
        Outer,
    }

    fn lit_doc_comment(input: Cursor, style: Comment) -> PResult<TokenTree> {
        match input.literal() {
            Some((span, lit, rest)) => {
                let string = lit.to_string();
                let ok = match style {
                    Comment::Inner => string.starts_with("//!") || string.starts_with("/*!"),
                    Comment::Outer => string.starts_with("///") || string.starts_with("/**"),
                };
                if ok {
                    Ok((
                        TokenTree {
                            span: span,
                            kind: TokenNode::Literal(Literal::string(&string)),
                        },
                        rest,
                    ))
                } else {
                    parse_error()
                }
            }
            _ => parse_error(),
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{ToTokens, Tokens};
    use proc_macro2::Literal;

    impl ToTokens for Attribute {
        fn to_tokens(&self, tokens: &mut Tokens) {
            // If this was a sugared doc, emit it in its original form instead of `#[doc = "..."]`
            if self.is_sugared_doc {
                if let Some(MetaItem::NameValue(ref pair)) = self.meta_item() {
                    if pair.ident == "doc" {
                        if let Lit::Str(ref comment) = pair.lit {
                            tokens.append(TokenTree {
                                span: comment.span,
                                kind: TokenNode::Literal(Literal::doccomment(&comment.value())),
                            });
                            return;
                        }
                    }
                }
            }

            self.pound_token.to_tokens(tokens);
            if let AttrStyle::Inner(ref b) = self.style {
                b.to_tokens(tokens);
            }
            self.bracket_token.surround(tokens, |tokens| {
                self.path.to_tokens(tokens);
                self.tts.to_tokens(tokens);
            });
        }
    }

    impl ToTokens for MetaItemList {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.ident.to_tokens(tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.nested.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for MetaNameValue {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.ident.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.lit.to_tokens(tokens);
        }
    }
}
