use super::*;

use std::iter;

ast_struct! {
    /// Doc-comments are promoted to attributes that have `is_sugared_doc` = true
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
                                        let pair = MetaNameValue {
                                            ident: ident.clone(),
                                            lit: lit.clone(),
                                        };
                                        return Some((NestedMetaItem::MetaItem(MetaItem::NameValue(pair)), &tts[3..]));
                                    }
                                }
                            }

                            if tts.len() >= 2 {
                                if let TokenTree::Delimited(Delimited { delim: DelimToken::Paren, tts: ref inner_tts }) = tts[1] {
                                    return match list_of_nested_meta_items_from_tokens(vec![], inner_tts) {
                                        Some(nested_meta_items) => {
                                            let list = MetaItemList {
                                                ident: ident.clone(),
                                                nested: nested_meta_items,
                                            };
                                            Some((NestedMetaItem::MetaItem(MetaItem::List(list)), &tts[2..]))
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
                    return Some(MetaItem::List(MetaItemList {
                        ident: name.clone(),
                        nested: nested_meta_items,
                    }));
                }
            }
        }

        if self.tts.len() == 2 {
            if let TokenTree::Token(Token::Eq) = self.tts[0] {
                if let TokenTree::Token(Token::Literal(ref lit)) = self.tts[1] {
                    return Some(MetaItem::NameValue(MetaNameValue {
                        ident: name.clone(),
                        lit: lit.clone(),
                    }));
                }
            }
        }

        None
    }
}

ast_enum! {
    /// Distinguishes between Attributes that decorate items and Attributes that
    /// are contained as statements within items. These two cases need to be
    /// distinguished for pretty-printing.
    #[cfg_attr(feature = "clone-impls", derive(Copy))]
    pub enum AttrStyle {
        /// Attribute of the form `#![...]`.
        Outer,

        /// Attribute of the form `#[...]`.
        Inner,
    }
}

ast_enum_of_structs! {
    /// A compile-time attribute item.
    ///
    /// E.g. `#[test]`, `#[derive(..)]` or `#[feature = "foo"]`
    pub enum MetaItem {
        /// Word meta item.
        ///
        /// E.g. `test` as in `#[test]`
        pub Word(Ident),

        /// List meta item.
        ///
        /// E.g. `derive(..)` as in `#[derive(..)]`
        pub List(MetaItemList {
            /// Name of this attribute.
            ///
            /// E.g. `derive` in `#[derive(..)]`
            pub ident: Ident,

            /// Arguments to this attribute
            ///
            /// E.g. `..` in `#[derive(..)]`
            pub nested: Vec<NestedMetaItem>,
        }),

        /// Name-value meta item.
        ///
        /// E.g. `feature = "foo"` as in `#[feature = "foo"]`
        pub NameValue(MetaNameValue {
            /// Name of this attribute.
            ///
            /// E.g. `feature` in `#[feature = "foo"]`
            pub ident: Ident,

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
            MetaItem::Word(ref name) => name.as_ref(),
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
        /// E.g. `Copy` in `#[derive(Copy)]` would be a `MetaItem::Word(Ident::from("Copy"))`.
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
                AttrStyle::Inner => true,
                _ => false,
            }
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
    use ty::parsing::mod_style_path;

    #[cfg(feature = "full")]
    named!(pub inner_attr -> Attribute, alt!(
        do_parse!(
            punct!("#") >>
            punct!("!") >>
            path_and_tts: delimited!(
                punct!("["),
                tuple!(mod_style_path, token_trees),
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
                tuple!(mod_style_path, token_trees),
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
    use ty::Path;

    impl ToTokens for Attribute {
        fn to_tokens(&self, tokens: &mut Tokens) {
            // If this was a sugared doc, emit it in its original form instead of `#[doc = "..."]`
            match *self {
                Attribute {
                    ref style,
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
                            match *style {
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

    impl ToTokens for MetaItemList {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.ident.to_tokens(tokens);
            tokens.append("(");
            tokens.append_separated(&self.nested, ",");
            tokens.append(")");
        }
    }

    impl ToTokens for MetaNameValue {
        fn to_tokens(&self, tokens: &mut Tokens) {
            self.ident.to_tokens(tokens);
            tokens.append("=");
            self.lit.to_tokens(tokens);
        }
    }
}
