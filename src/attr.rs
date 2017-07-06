use super::*;
use delimited::Delimited;

use std::iter;

use proc_macro2::{self, Delimiter, TokenNode, Spacing};

ast_struct! {
    /// Doc-comments are promoted to attributes that have `is_sugared_doc` = true
    pub struct Attribute {
        pub style: AttrStyle,
        pub pound_token: tokens::Pound,
        pub bracket_token: tokens::Bracket,

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
}

impl Attribute {
    /// Parses the tokens after the path as a [`MetaItem`](enum.MetaItem.html) if possible.
    pub fn meta_item(&self) -> Option<MetaItem> {
        let name = if self.path.segments.len() == 1 {
            &self.path.segments.get(0).item().ident
        } else {
            return None;
        };

        if self.tts.is_empty() {
            return Some(MetaItem::Term(name.clone()));
        }

        if self.tts.len() == 1 {
            if let TokenNode::Group(Delimiter::Parenthesis, ref ts) = self.tts[0].0.kind {
                let tokens = ts.clone().into_iter().collect::<Vec<_>>();
                if let Some(nested_meta_items) = list_of_nested_meta_items_from_tokens(&tokens) {
                    return Some(MetaItem::List(MetaItemList {
                        paren_token: tokens::Paren(Span(self.tts[0].0.span)),
                        ident: name.clone(),
                        nested: nested_meta_items,
                    }));
                }
            }
        }

        if self.tts.len() == 2 {
            if let TokenNode::Op('=', Spacing::Alone) = self.tts[0].0.kind {
                if let TokenNode::Literal(ref lit) = self.tts[1].0.kind {
                    return Some(MetaItem::NameValue(MetaNameValue {
                        ident: name.clone(),
                        eq_token: tokens::Eq([Span(self.tts[0].0.span)]),
                        lit: Lit {
                            value: LitKind::Other(lit.clone()),
                            span: Span(self.tts[1].0.span),
                        },
                    }));
                }
            }
        }

        None
    }
}

fn nested_meta_item_from_tokens(tts: &[proc_macro2::TokenTree])
    -> Option<(NestedMetaItem, &[proc_macro2::TokenTree])>
{
    assert!(!tts.is_empty());

    match tts[0].kind {
        TokenNode::Literal(ref lit) => {
            let lit = Lit {
                value: LitKind::Other(lit.clone()),
                span: Span(tts[0].span),
            };
            Some((NestedMetaItem::Literal(lit), &tts[1..]))
        }

        TokenNode::Term(sym) => {
            let ident = Ident::new(sym, Span(tts[0].span));
            if tts.len() >= 3 {
                if let TokenNode::Op('=', Spacing::Alone) = tts[1].kind {
                    if let TokenNode::Literal(ref lit) = tts[2].kind {
                        let pair = MetaNameValue {
                            ident: Ident::new(sym, Span(tts[0].span)),
                            eq_token: tokens::Eq([Span(tts[1].span)]),
                            lit: Lit {
                                value: LitKind::Other(lit.clone()),
                                span: Span(tts[2].span),
                            },
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
                                paren_token: tokens::Paren(Span(tts[1].span)),
                                nested: nested_meta_items,
                            };
                            Some((MetaItem::List(list).into(), &tts[2..]))
                        }

                        None => None
                    };
                }
            }

            Some((MetaItem::Term(ident).into(), &tts[1..]))
        }

        _ => None
    }
}

fn list_of_nested_meta_items_from_tokens(mut tts: &[proc_macro2::TokenTree])
    -> Option<Delimited<NestedMetaItem, tokens::Comma>>
{
    let mut delimited = Delimited::new();
    let mut first = true;

    while !tts.is_empty() {
        let prev_comma = if first {
            first = false;
            None
        } else if let TokenNode::Op(',', Spacing::Alone) = tts[0].kind {
            let tok = tokens::Comma([Span(tts[0].span)]);
            tts = &tts[1..];
            if tts.is_empty() {
                break
            }
            Some(tok)
        } else {
            return None
        };
        let (nested, rest) = match nested_meta_item_from_tokens(tts) {
            Some(pair) => pair,
            None => return None,
        };
        match prev_comma {
            Some(comma) => delimited.push_next(nested, comma),
            None => delimited.push_first(nested),
        }
        tts = rest;
    }

    Some(delimited)
}


ast_enum! {
    /// Distinguishes between Attributes that decorate items and Attributes that
    /// are contained as statements within items. These two cases need to be
    /// distinguished for pretty-printing.
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum AttrStyle {
        /// Attribute of the form `#[...]`.
        Outer,

        /// Attribute of the form `#![...]`.
        Inner(tokens::Bang),
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

            pub paren_token: tokens::Paren,

            /// Arguments to this attribute
            ///
            /// E.g. `..` in `#[derive(..)]`
            pub nested: Delimited<NestedMetaItem, tokens::Comma>,
        }),

        /// Name-value meta item.
        ///
        /// E.g. `feature = "foo"` as in `#[feature = "foo"]`
        pub NameValue(MetaNameValue {
            /// Name of this attribute.
            ///
            /// E.g. `feature` in `#[feature = "foo"]`
            pub ident: Ident,

            pub eq_token: tokens::Eq,

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
    where T: IntoIterator<Item = &'a Attribute>
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
    use synom::{PResult, Cursor, parse_error};
    use synom::tokens::*;
    use proc_macro2::{TokenNode, Spacing, TokenTree};

    fn eq() -> TokenTree {
        TokenTree {
            span: Default::default(),
            kind: TokenNode::Op('=', Spacing::Alone),
        }
    }

    impl Attribute {
        #[cfg(feature = "full")]
        named!(pub parse_inner -> Self, alt!(
            do_parse!(
                pound: syn!(Pound) >>
                bang: syn!(Bang) >>
                path_and_tts: brackets!(tuple!(
                    call!(::Path::parse_mod_style),
                    call!(::TokenTree::parse_list)
                )) >>
                ({
                    let ((path, tts), bracket) = path_and_tts;

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
                lit_doc_comment,
                |lit| Attribute {
                    style: AttrStyle::Inner(tokens::Bang::default()),
                    path: "doc".into(),
                    tts: vec![
                        ::TokenTree(eq()),
                        ::TokenTree(lit),
                    ],
                    is_sugared_doc: true,
                    pound_token: tokens::Pound::default(),
                    bracket_token: tokens::Bracket::default(),
                }
            )
        ));

        named!(pub parse_outer -> Self, alt!(
            do_parse!(
                pound: syn!(Pound) >>
                path_and_tts: brackets!(tuple!(
                    call!(::Path::parse_mod_style),
                    call!(::TokenTree::parse_list)
                )) >>
                ({
                    let ((path, tts), bracket) = path_and_tts;

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
                lit_doc_comment,
                |lit| Attribute {
                    style: AttrStyle::Outer,
                    path: "doc".into(),
                    tts: vec![
                        ::TokenTree(eq()),
                        ::TokenTree(lit),
                    ],
                    is_sugared_doc: true,
                    pound_token: tokens::Pound::default(),
                    bracket_token: tokens::Bracket::default(),
                }
            )
        ));
    }

    fn lit_doc_comment(input: Cursor) -> PResult<TokenTree> {
        match input.literal() {
            Some((rest, span, lit)) => {
                let literal = lit.to_string();
                if literal.starts_with("//") || literal.starts_with("/*") {
                    Ok((rest, TokenTree {
                        span: span,
                        kind: TokenNode::Literal(lit)
                    }))
                } else {
                    parse_error()
                }
            }
            _ => parse_error()
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};

    impl ToTokens for Attribute {
        fn to_tokens(&self, tokens: &mut Tokens) {
            // If this was a sugared doc, emit it in its original form instead of `#[doc = "..."]`
            if self.is_sugared_doc {
                if let Some(MetaItem::NameValue(ref pair)) = self.meta_item() {
                    if pair.ident == "doc" {
                        let value = pair.lit.value.to_string();
                        if value.starts_with('/') {
                            pair.lit.to_tokens(tokens);
                            return
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
                tokens.append_all(&self.tts);
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
