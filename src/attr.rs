use super::*;
use punctuated::Punctuated;

use std::iter;

use proc_macro2::TokenStream;
#[cfg(not(feature = "parsing"))]
use proc_macro2::{Delimiter, Spacing, TokenTree};

#[cfg(feature = "parsing")]
use parse::{ParseStream, Result};
#[cfg(feature = "extra-traits")]
use std::hash::{Hash, Hasher};
#[cfg(feature = "extra-traits")]
use tt::TokenStreamHelper;

ast_struct! {
    /// An attribute like `#[repr(transparent)]`.
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
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
    /// attributes, as this is how they are processed by the compiler and by
    /// `macro_rules!` macros.
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
    /// Use the [`parse_meta`] method to try parsing the tokens of an attribute
    /// into the structured representation that is used by convention across
    /// most Rust libraries.
    ///
    /// [`parse_meta`]: #method.parse_meta
    ///
    /// # Parsing
    ///
    /// This type does not implement the [`Parse`] trait and thus cannot be
    /// parsed directly by [`ParseStream::parse`]. Instead use
    /// [`ParseStream::call`] with one of the two parser functions
    /// [`Attribute::parse_outer`] or [`Attribute::parse_inner`] depending on
    /// which you intend to parse.
    ///
    /// [`Parse`]: parse/trait.Parse.html
    /// [`ParseStream::parse`]: parse/struct.ParseBuffer.html#method.parse
    /// [`ParseStream::call`]: parse/struct.ParseBuffer.html#method.call
    /// [`Attribute::parse_outer`]: #method.parse_outer
    /// [`Attribute::parse_inner`]: #method.parse_inner
    ///
    /// ```edition2018
    /// use syn::{Attribute, Ident, Result, Token};
    /// use syn::parse::{Parse, ParseStream};
    ///
    /// // Parses a unit struct with attributes.
    /// //
    /// //     #[path = "s.tmpl"]
    /// //     struct S;
    /// struct UnitStruct {
    ///     attrs: Vec<Attribute>,
    ///     struct_token: Token![struct],
    ///     name: Ident,
    ///     semi_token: Token![;],
    /// }
    ///
    /// impl Parse for UnitStruct {
    ///     fn parse(input: ParseStream) -> Result<Self> {
    ///         Ok(UnitStruct {
    ///             attrs: input.call(Attribute::parse_outer)?,
    ///             struct_token: input.parse()?,
    ///             name: input.parse()?,
    ///             semi_token: input.parse()?,
    ///         })
    ///     }
    /// }
    /// ```
    pub struct Attribute #manual_extra_traits {
        pub pound_token: Token![#],
        pub style: AttrStyle,
        pub bracket_token: token::Bracket,
        pub path: Path,
        pub tts: TokenStream,
    }
}

#[cfg(feature = "extra-traits")]
impl Eq for Attribute {}

#[cfg(feature = "extra-traits")]
impl PartialEq for Attribute {
    fn eq(&self, other: &Self) -> bool {
        self.style == other.style
            && self.pound_token == other.pound_token
            && self.bracket_token == other.bracket_token
            && self.path == other.path
            && TokenStreamHelper(&self.tts) == TokenStreamHelper(&other.tts)
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
    }
}

impl Attribute {
    /// Parses the tokens after the path as a [`Meta`](enum.Meta.html) if
    /// possible.
    ///
    /// Deprecated; use `parse_meta` instead.
    #[doc(hidden)]
    pub fn interpret_meta(&self) -> Option<Meta> {
        #[cfg(feature = "parsing")]
        {
            self.parse_meta().ok()
        }

        #[cfg(not(feature = "parsing"))]
        {
            let path = Self::clone_ident_path(&self.path);
            if path.is_none() {
                return None;
            }
            let path = path.unwrap();

            if self.tts.is_empty() {
                return Some(Meta::Path(path));
            }

            let tts = self.tts.clone().into_iter().collect::<Vec<_>>();

            if tts.len() == 1 {
                if let Some(meta) = Attribute::extract_meta_list(path, &tts[0]) {
                    return Some(meta);
                }
            } else if tts.len() == 2 {
                if let Some(meta) = Attribute::extract_name_value(path, &tts[0], &tts[1]) {
                    return Some(meta);
                }
            }

            None
        }
    }

    /// Parses the tokens after the path as a [`Meta`](enum.Meta.html) if
    /// possible.
    #[cfg(all(feature = "parsing", feature = "clone-impls"))]
    pub fn parse_meta(&self) -> Result<Meta> {
        let parser = |input: ParseStream| parsing::parse_meta_after_path(self.path.clone(), input);
        parse::Parser::parse2(parser, self.tts.clone())
    }

    /// Parses the tokens after the path as a [`Meta`](enum.Meta.html) if
    /// possible.
    #[cfg(all(feature = "parsing", not(feature = "clone-impls")))]
    pub fn parse_meta(&self) -> Result<Meta> {
        let path = Self::clone_ident_path(&self.path).ok_or_else(|| {
            #[cfg(all(feature = "parsing", feature = "printing"))]
            let span = {
                use spanned::Spanned;
                self.path.span()
            };
            #[cfg(all(feature = "parsing", not(feature = "printing")))]
            let span = proc_macro2::Span::call_site();

            Error::new(span, "path contained more than one segment")
        })?;

        let parser = |input: ParseStream| parsing::parse_meta_after_path(path, input);
        parse::Parser::parse2(parser, self.tts.clone())
    }

    /// Parses zero or more outer attributes from the stream.
    ///
    /// *This function is available if Syn is built with the `"parsing"`
    /// feature.*
    #[cfg(feature = "parsing")]
    pub fn parse_outer(input: ParseStream) -> Result<Vec<Self>> {
        let mut attrs = Vec::new();
        while input.peek(Token![#]) {
            attrs.push(input.call(parsing::single_parse_outer)?);
        }
        Ok(attrs)
    }

    /// Parses zero or more inner attributes from the stream.
    ///
    /// *This function is available if Syn is built with the `"parsing"`
    /// feature.*
    #[cfg(feature = "parsing")]
    pub fn parse_inner(input: ParseStream) -> Result<Vec<Self>> {
        let mut attrs = Vec::new();
        while input.peek(Token![#]) && input.peek2(Token![!]) {
            attrs.push(input.call(parsing::single_parse_inner)?);
        }
        Ok(attrs)
    }

    #[cfg(not(feature = "parsing"))]
    fn extract_meta_list(path: Path, tt: &TokenTree) -> Option<Meta> {
        let g = match *tt {
            TokenTree::Group(ref g) => g,
            _ => return None,
        };
        if g.delimiter() != Delimiter::Parenthesis {
            return None;
        }
        let tokens = g.stream().clone().into_iter().collect::<Vec<_>>();
        let nested = match list_of_nested_meta_items_from_tokens(&tokens) {
            Some(n) => n,
            None => return None,
        };
        Some(Meta::List(MetaList {
            paren_token: token::Paren(g.span()),
            path: path,
            nested: nested,
        }))
    }

    #[cfg(not(feature = "parsing"))]
    fn extract_name_value(path: Path, a: &TokenTree, b: &TokenTree) -> Option<Meta> {
        let a = match *a {
            TokenTree::Punct(ref o) => o,
            _ => return None,
        };
        if a.spacing() != Spacing::Alone {
            return None;
        }
        if a.as_char() != '=' {
            return None;
        }

        match *b {
            TokenTree::Literal(ref l) if !l.to_string().starts_with('/') => {
                Some(Meta::NameValue(MetaNameValue {
                    path: path,
                    eq_token: Token![=]([a.span()]),
                    lit: Lit::new(l.clone()),
                }))
            }
            TokenTree::Ident(ref v) => match &v.to_string()[..] {
                v @ "true" | v @ "false" => Some(Meta::NameValue(MetaNameValue {
                    path: path,
                    eq_token: Token![=]([a.span()]),
                    lit: Lit::Bool(LitBool {
                        value: v == "true",
                        span: b.span(),
                    }),
                })),
                _ => None,
            },
            _ => None,
        }
    }

    /// Returns an owned `Path` if the given `Path` comprises of a single `Ident`.
    #[cfg(any(
        all(feature = "parsing", not(feature = "clone-impls")),
        not(feature = "parsing")
    ))]
    fn clone_ident_path(path: &Path) -> Option<Path> {
        if path.segments.len() == 1 {
            path.segments
                .first()
                .map(|pair| pair.value().ident.clone().into())
        } else {
            return None;
        }
    }
}

#[cfg(not(feature = "parsing"))]
fn nested_meta_item_from_tokens(tts: &[TokenTree]) -> Option<(NestedMeta, &[TokenTree])> {
    assert!(!tts.is_empty());

    match tts[0] {
        TokenTree::Literal(ref lit) => {
            if lit.to_string().starts_with('/') {
                None
            } else {
                let lit = Lit::new(lit.clone());
                Some((NestedMeta::Literal(lit), &tts[1..]))
            }
        }

        // This function is only called by the deprecated `interpret_meta(..)` function.
        // It does not cater for path Meta items with more than one segment.
        TokenTree::Ident(ref ident) => {
            if tts.len() >= 3 {
                if let Some(meta) =
                    Attribute::extract_name_value(ident.clone().into(), &tts[1], &tts[2])
                {
                    return Some((NestedMeta::Meta(meta), &tts[3..]));
                }
            }

            if tts.len() >= 2 {
                if let Some(meta) = Attribute::extract_meta_list(ident.clone().into(), &tts[1]) {
                    return Some((NestedMeta::Meta(meta), &tts[2..]));
                }
            }

            let nested_meta = if ident == "true" || ident == "false" {
                NestedMeta::Literal(Lit::Bool(LitBool {
                    value: ident == "true",
                    span: ident.span(),
                }))
            } else {
                NestedMeta::Meta(Meta::Path(ident.clone().into()))
            };
            Some((nested_meta, &tts[1..]))
        }

        _ => None,
    }
}

#[cfg(not(feature = "parsing"))]
fn list_of_nested_meta_items_from_tokens(
    mut tts: &[TokenTree],
) -> Option<Punctuated<NestedMeta, Token![,]>> {
    let mut nested_meta_items = Punctuated::new();
    let mut first = true;

    while !tts.is_empty() {
        let prev_comma = if first {
            first = false;
            None
        } else if let TokenTree::Punct(ref op) = tts[0] {
            if op.spacing() != Spacing::Alone {
                return None;
            }
            if op.as_char() != ',' {
                return None;
            }
            let tok = Token![,]([op.span()]);
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
    /// Distinguishes between attributes that decorate an item and attributes
    /// that are contained within an item.
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
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
    /// Content of a compile-time structured attribute.
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    ///
    /// ## Word
    ///
    /// A meta word is like the `test` in `#[test]`.
    ///
    /// ## List
    ///
    /// A meta list is like the `derive(Copy)` in `#[derive(Copy)]`.
    ///
    /// ## NameValue
    ///
    /// A name-value meta is like the `path = "..."` in `#[path =
    /// "sys/windows.rs"]`.
    ///
    /// # Syntax tree enum
    ///
    /// This type is a [syntax tree enum].
    ///
    /// [syntax tree enum]: enum.Expr.html#syntax-tree-enums
    pub enum Meta {
        /// A path within an attribute, like `serde::Serialize`.
        pub Path(Path),
        /// A structured list within an attribute, like `derive(Copy, Clone)`.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub List(MetaList {
            pub path: Path,
            pub paren_token: token::Paren,
            pub nested: Punctuated<NestedMeta, Token![,]>,
        }),
        /// A name-value pair within an attribute, like `feature = "nightly"`.
        ///
        /// *This type is available if Syn is built with the `"derive"` or
        /// `"full"` feature.*
        pub NameValue(MetaNameValue {
            pub path: Path,
            pub eq_token: Token![=],
            pub lit: Lit,
        }),
    }
}

impl Meta {
    /// Returns the identifier that begins this structured meta item.
    ///
    /// For example this would return:
    ///
    /// * `test` in `#[test]`
    /// * `derive` in `#[derive(Copy)]`
    /// * `Copy` in `Copy`
    /// * `serde::Serialize` in `serde::Serialize`
    /// * `path` in `#[path = "sys/windows.rs"]`.
    ///
    /// *This type is available if Syn is built with the `"clone-impls"`
    /// feature.*
    #[cfg(feature = "clone-impls")]
    pub fn name(&self) -> Path {
        match *self {
            Meta::Path(ref path) => path.clone(),
            Meta::List(ref meta) => meta.path.clone(),
            Meta::NameValue(ref meta) => meta.path.clone(),
        }
    }
}

ast_enum_of_structs! {
    /// Element of a compile-time attribute list.
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    pub enum NestedMeta {
        /// A structured meta item, like the `Copy` in `#[derive(Copy)]` which
        /// would be a nested `Meta::Path`, and `serde::Serialize` in
        /// `#[derive(serde::Serialize)]` would be a nested `Meta::Path`.
        pub Meta(Meta),

        /// A Rust literal, like the `"new_name"` in `#[rename("new_name")]`.
        pub Literal(Lit),
    }
}

/// Conventional argument type associated with an invocation of an attribute
/// macro.
///
/// For example if we are developing an attribute macro that is intended to be
/// invoked on function items as follows:
///
/// ```edition2018
/// # const IGNORE: &str = stringify! {
/// #[my_attribute(path = "/v1/refresh")]
/// # };
/// pub fn refresh() {
///     /* ... */
/// }
/// ```
///
/// The implementation of this macro would want to parse its attribute arguments
/// as type `AttributeArgs`.
///
/// ```edition2018
/// extern crate proc_macro;
///
/// use proc_macro::TokenStream;
/// use syn::{parse_macro_input, AttributeArgs, ItemFn};
///
/// # const IGNORE: &str = stringify! {
/// #[proc_macro_attribute]
/// # };
/// pub fn my_attribute(args: TokenStream, input: TokenStream) -> TokenStream {
///     let args = parse_macro_input!(args as AttributeArgs);
///     let input = parse_macro_input!(input as ItemFn);
///
///     /* ... */
/// #   "".parse().unwrap()
/// }
/// ```
pub type AttributeArgs = Vec<NestedMeta>;

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
        #[cfg_attr(feature = "cargo-clippy", allow(trivially_copy_pass_by_ref))]
        fn is_outer(attr: &&Attribute) -> bool {
            match attr.style {
                AttrStyle::Outer => true,
                _ => false,
            }
        }
        self.into_iter().filter(is_outer)
    }

    fn inner(self) -> Self::Ret {
        #[cfg_attr(feature = "cargo-clippy", allow(trivially_copy_pass_by_ref))]
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

    use ext::IdentExt;
    use parse::{Parse, ParseStream, Result};
    #[cfg(feature = "full")]
    use private;

    pub fn single_parse_inner(input: ParseStream) -> Result<Attribute> {
        let content;
        Ok(Attribute {
            pound_token: input.parse()?,
            style: AttrStyle::Inner(input.parse()?),
            bracket_token: bracketed!(content in input),
            path: content.call(parse_meta_path)?,
            tts: content.parse()?,
        })
    }

    pub fn single_parse_outer(input: ParseStream) -> Result<Attribute> {
        let content;
        Ok(Attribute {
            pound_token: input.parse()?,
            style: AttrStyle::Outer,
            bracket_token: bracketed!(content in input),
            path: content.call(parse_meta_path)?,
            tts: content.parse()?,
        })
    }

    #[cfg(feature = "full")]
    impl private {
        pub fn attrs(outer: Vec<Attribute>, inner: Vec<Attribute>) -> Vec<Attribute> {
            let mut attrs = outer;
            attrs.extend(inner);
            attrs
        }
    }

    impl Parse for Meta {
        /// Attempts to parse tokens as a `Meta`.
        ///
        /// Historically this assumed the first token encountered is an `Ident`.
        /// However, derive attributes such as `#[derive(serde::Serialize)]`
        /// have been valid since Rust 1.30:
        ///
        /// <https://github.com/rust-lang/rust/blob/stable/RELEASES.md#version-1300-2018-10-25>
        ///
        /// Therefore, we need to accept `Path`s as well, while erring on
        /// invalid usages of `Path`s.
        fn parse(input: ParseStream) -> Result<Self> {
            let path = input.call(parse_meta_path)?;
            parse_meta_after_path(path, input)
        }
    }

    impl Parse for MetaList {
        fn parse(input: ParseStream) -> Result<Self> {
            let path = input.call(parse_meta_path)?;
            parse_meta_list_after_path(path, input)
        }
    }

    impl Parse for MetaNameValue {
        fn parse(input: ParseStream) -> Result<Self> {
            let path = input.call(parse_meta_path)?;
            parse_meta_name_value_after_path(path, input)
        }
    }

    impl Parse for NestedMeta {
        fn parse(input: ParseStream) -> Result<Self> {
            let ahead = input.fork();

            if ahead.peek(Lit) && !(ahead.peek(LitBool) && ahead.peek2(Token![=])) {
                input.parse().map(NestedMeta::Literal)
            } else if ahead.call(parse_meta_path).is_ok() {
                input.parse().map(NestedMeta::Meta)
            } else {
                Err(input.error("expected path or literal"))
            }
        }
    }

    pub fn parse_meta_after_path(path: Path, input: ParseStream) -> Result<Meta> {
        if input.peek(token::Paren) {
            parse_meta_list_after_path(path, input).map(Meta::List)
        } else if input.peek(Token![=]) {
            parse_meta_name_value_after_path(path, input).map(Meta::NameValue)
        } else {
            Ok(Meta::Path(path))
        }
    }

    fn parse_meta_list_after_path(path: Path, input: ParseStream) -> Result<MetaList> {
        let content;
        Ok(MetaList {
            path: path,
            paren_token: parenthesized!(content in input),
            nested: content.parse_terminated(NestedMeta::parse)?,
        })
    }

    fn parse_meta_name_value_after_path(path: Path, input: ParseStream) -> Result<MetaNameValue> {
        Ok(MetaNameValue {
            path: path,
            eq_token: input.parse()?,
            lit: input.parse()?,
        })
    }

    /// Parse a `Path` in mod style, while accepting keywords.
    fn parse_meta_path(input: ParseStream) -> Result<Path> {
        Ok(Path {
            leading_colon: input.parse()?,
            segments: {
                let mut segments = Punctuated::new();
                loop {
                    if !Ident::peek_any(input) {
                        break;
                    }
                    let ident = Ident::parse_any(input)?;
                    segments.push_value(PathSegment::from(ident));
                    if !input.peek(Token![::]) {
                        break;
                    }
                    let punct = input.parse()?;
                    segments.push_punct(punct);
                }
                if segments.is_empty() {
                    return Err(input.error("expected path"));
                } else if segments.trailing_punct() {
                    return Err(input.error("expected path segment"));
                }
                segments
            },
        })
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use proc_macro2::TokenStream;
    use quote::ToTokens;

    impl ToTokens for Attribute {
        fn to_tokens(&self, tokens: &mut TokenStream) {
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

    impl ToTokens for MetaList {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.path.to_tokens(tokens);
            self.paren_token.surround(tokens, |tokens| {
                self.nested.to_tokens(tokens);
            })
        }
    }

    impl ToTokens for MetaNameValue {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.path.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.lit.to_tokens(tokens);
        }
    }
}
