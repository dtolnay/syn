use super::*;
use crate::punctuated::Punctuated;

use std::iter;

use proc_macro2::TokenStream;

#[cfg(feature = "parsing")]
use crate::parse::{ParseStream, Result};
#[cfg(feature = "parsing")]
use crate::punctuated::Pair;
#[cfg(feature = "extra-traits")]
use crate::tt::TokenStreamHelper;
#[cfg(feature = "extra-traits")]
use std::hash::{Hash, Hasher};

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
    /// [`parse_meta`]: Attribute::parse_meta
    ///
    /// # Parsing
    ///
    /// This type does not implement the [`Parse`] trait and thus cannot be
    /// parsed directly by [`ParseStream::parse`]. Instead use
    /// [`ParseStream::call`] with one of the two parser functions
    /// [`Attribute::parse_outer`] or [`Attribute::parse_inner`] depending on
    /// which you intend to parse.
    ///
    /// [`Parse`]: parse::Parse
    /// [`ParseStream::parse`]: parse::ParseBuffer::parse
    /// [`ParseStream::call`]: parse::ParseBuffer::call
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
        pub tokens: TokenStream,
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
            && TokenStreamHelper(&self.tokens) == TokenStreamHelper(&other.tokens)
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
        TokenStreamHelper(&self.tokens).hash(state);
    }
}

impl Attribute {
    /// Parses the content of the attribute, consisting of the path and tts, as
    /// a [`Meta`] if possible.
    #[cfg(feature = "parsing")]
    pub fn parse_meta(&self) -> Result<Meta> {
        fn clone_ident_segment(segment: &PathSegment) -> PathSegment {
            PathSegment {
                ident: segment.ident.clone(),
                arguments: PathArguments::None,
            }
        }

        let path = Path {
            leading_colon: self
                .path
                .leading_colon
                .as_ref()
                .map(|colon| Token![::](colon.spans)),
            segments: self
                .path
                .segments
                .pairs()
                .map(|pair| match pair {
                    Pair::Punctuated(seg, punct) => {
                        Pair::Punctuated(clone_ident_segment(seg), Token![::](punct.spans))
                    }
                    Pair::End(seg) => Pair::End(clone_ident_segment(seg)),
                })
                .collect(),
        };

        let parser = |input: ParseStream| parsing::parse_meta_after_path(path, input);
        parse::Parser::parse2(parser, self.tokens.clone())
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
    /// ## Path
    ///
    /// A meta path is like the `test` in `#[test]`.
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
    //
    // TODO: change syntax-tree-enum link to an intra rustdoc link, currently
    // blocked on https://github.com/rust-lang/rust/issues/62833
    pub enum Meta {
        Path(Path),

        /// A structured list within an attribute, like `derive(Copy, Clone)`.
        List(MetaList),

        /// A name-value pair within an attribute, like `feature = "nightly"`.
        NameValue(MetaNameValue),
    }
}

ast_struct! {
    /// A structured list within an attribute, like `derive(Copy, Clone)`.
    ///
    /// *This type is available if Syn is built with the `"derive"` or
    /// `"full"` feature.*
    pub struct MetaList {
        pub path: Path,
        pub paren_token: token::Paren,
        pub nested: Punctuated<NestedMeta, Token![,]>,
    }
}

ast_struct! {
    /// A name-value pair within an attribute, like `feature = "nightly"`.
    ///
    /// *This type is available if Syn is built with the `"derive"` or
    /// `"full"` feature.*
    pub struct MetaNameValue {
        pub path: Path,
        pub eq_token: Token![=],
        pub lit: Lit,
    }
}

impl Meta {
    /// Returns the identifier that begins this structured meta item.
    ///
    /// For example this would return the `test` in `#[test]`, the `derive` in
    /// `#[derive(Copy)]`, and the `path` in `#[path = "sys/windows.rs"]`.
    pub fn path(&self) -> &Path {
        match self {
            Meta::Path(path) => path,
            Meta::List(meta) => &meta.path,
            Meta::NameValue(meta) => &meta.path,
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
        /// would be a nested `Meta::Path`.
        Meta(Meta),

        /// A Rust literal, like the `"new_name"` in `#[rename("new_name")]`.
        Literal(Lit),
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

    use crate::ext::IdentExt;
    use crate::parse::{Parse, ParseStream, Result};
    #[cfg(feature = "full")]
    use crate::private;

    pub fn single_parse_inner(input: ParseStream) -> Result<Attribute> {
        let content;
        Ok(Attribute {
            pound_token: input.parse()?,
            style: AttrStyle::Inner(input.parse()?),
            bracket_token: bracketed!(content in input),
            path: content.call(Path::parse_mod_style)?,
            tokens: content.parse()?,
        })
    }

    pub fn single_parse_outer(input: ParseStream) -> Result<Attribute> {
        let content;
        Ok(Attribute {
            pound_token: input.parse()?,
            style: AttrStyle::Outer,
            bracket_token: bracketed!(content in input),
            path: content.call(Path::parse_mod_style)?,
            tokens: content.parse()?,
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

    // Like Path::parse_mod_style but accepts keywords in the path.
    fn parse_meta_path(input: ParseStream) -> Result<Path> {
        Ok(Path {
            leading_colon: input.parse()?,
            segments: {
                let mut segments = Punctuated::new();
                while input.peek(Ident::peek_any) {
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

    impl Parse for Meta {
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
            if input.peek(Lit) && !(input.peek(LitBool) && input.peek2(Token![=])) {
                input.parse().map(NestedMeta::Literal)
            } else if input.peek(Ident::peek_any) {
                input.parse().map(NestedMeta::Meta)
            } else {
                Err(input.error("expected identifier or literal"))
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
            path,
            paren_token: parenthesized!(content in input),
            nested: content.parse_terminated(NestedMeta::parse)?,
        })
    }

    fn parse_meta_name_value_after_path(path: Path, input: ParseStream) -> Result<MetaNameValue> {
        Ok(MetaNameValue {
            path,
            eq_token: input.parse()?,
            lit: input.parse()?,
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
            if let AttrStyle::Inner(b) = &self.style {
                b.to_tokens(tokens);
            }
            self.bracket_token.surround(tokens, |tokens| {
                self.path.to_tokens(tokens);
                self.tokens.to_tokens(tokens);
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
