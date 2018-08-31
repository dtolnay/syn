// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use super::*;
use punctuated::Punctuated;

ast_struct! {
    /// A path at which a named item is exported: `std::collections::HashMap`.
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    pub struct Path {
        pub leading_colon: Option<Token![::]>,
        pub segments: Punctuated<PathSegment, Token![::]>,
    }
}

impl Path {
    pub fn global(&self) -> bool {
        self.leading_colon.is_some()
    }
}

/// A helper for printing a self-type qualified path as tokens.
///
/// ```rust
/// extern crate syn;
/// extern crate quote;
/// extern crate proc_macro2;
///
/// use syn::{QSelf, Path, PathTokens};
/// use proc_macro2::TokenStream;
/// use quote::ToTokens;
///
/// struct MyNode {
///     qself: Option<QSelf>,
///     path: Path,
/// }
///
/// impl ToTokens for MyNode {
///     fn to_tokens(&self, tokens: &mut TokenStream) {
///         PathTokens(&self.qself, &self.path).to_tokens(tokens);
///     }
/// }
/// #
/// # fn main() {}
/// ```
///
/// *This type is available if Syn is built with the `"derive"` or `"full"`
/// feature and the `"printing"` feature.*
#[cfg(feature = "printing")]
#[cfg_attr(feature = "extra-traits", derive(Debug, Eq, PartialEq, Hash))]
#[cfg_attr(feature = "clone-impls", derive(Clone))]
pub struct PathTokens<'a>(pub &'a Option<QSelf>, pub &'a Path);

impl<T> From<T> for Path
where
    T: Into<PathSegment>,
{
    fn from(segment: T) -> Self {
        let mut path = Path {
            leading_colon: None,
            segments: Punctuated::new(),
        };
        path.segments.push_value(segment.into());
        path
    }
}

ast_struct! {
    /// A segment of a path together with any path arguments on that segment.
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    pub struct PathSegment {
        pub ident: Ident,
        pub arguments: PathArguments,
    }
}

impl<T> From<T> for PathSegment
where
    T: Into<Ident>,
{
    fn from(ident: T) -> Self {
        PathSegment {
            ident: ident.into(),
            arguments: PathArguments::None,
        }
    }
}

ast_enum! {
    /// Angle bracketed or parenthesized arguments of a path segment.
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    ///
    /// ## Angle bracketed
    ///
    /// The `<'a, T>` in `std::slice::iter<'a, T>`.
    ///
    /// ## Parenthesized
    ///
    /// The `(A, B) -> C` in `Fn(A, B) -> C`.
    pub enum PathArguments {
        None,
        /// The `<'a, T>` in `std::slice::iter<'a, T>`.
        AngleBracketed(AngleBracketedGenericArguments),
        /// The `(A, B) -> C` in `Fn(A, B) -> C`.
        Parenthesized(ParenthesizedGenericArguments),
    }
}

impl Default for PathArguments {
    fn default() -> Self {
        PathArguments::None
    }
}

impl PathArguments {
    pub fn is_empty(&self) -> bool {
        match *self {
            PathArguments::None => true,
            PathArguments::AngleBracketed(ref bracketed) => bracketed.args.is_empty(),
            PathArguments::Parenthesized(_) => false,
        }
    }
}

ast_enum! {
    /// An individual generic argument, like `'a`, `T`, or `Item = T`.
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    pub enum GenericArgument {
        /// A lifetime argument.
        Lifetime(Lifetime),
        /// A type argument.
        Type(Type),
        /// A binding (equality constraint) on an associated type: the `Item =
        /// u8` in `Iterator<Item = u8>`.
        Binding(Binding),
        /// A const expression. Must be inside of a block.
        ///
        /// NOTE: Identity expressions are represented as Type arguments, as
        /// they are indistinguishable syntactically.
        Const(Expr),
    }
}

ast_struct! {
    /// Angle bracketed arguments of a path segment: the `<K, V>` in `HashMap<K,
    /// V>`.
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    pub struct AngleBracketedGenericArguments {
        pub colon2_token: Option<Token![::]>,
        pub lt_token: Token![<],
        pub args: Punctuated<GenericArgument, Token![,]>,
        pub gt_token: Token![>],
    }
}

ast_struct! {
    /// A binding (equality constraint) on an associated type: `Item = u8`.
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    pub struct Binding {
        pub ident: Ident,
        pub eq_token: Token![=],
        pub ty: Type,
    }
}

ast_struct! {
    /// Arguments of a function path segment: the `(A, B) -> C` in `Fn(A,B) ->
    /// C`.
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    pub struct ParenthesizedGenericArguments {
        pub paren_token: token::Paren,
        /// `(A, B)`
        pub inputs: Punctuated<Type, Token![,]>,
        /// `C`
        pub output: ReturnType,
    }
}

ast_struct! {
    /// The explicit Self type in a qualified path: the `T` in `<T as
    /// Display>::fmt`.
    ///
    /// The actual path, including the trait and the associated item, is stored
    /// separately. The `position` field represents the index of the associated
    /// item qualified with this Self type.
    ///
    /// ```text
    /// <Vec<T> as a::b::Trait>::AssociatedItem
    ///  ^~~~~~    ~~~~~~~~~~~~~~^
    ///  ty        position = 3
    ///
    /// <Vec<T>>::AssociatedItem
    ///  ^~~~~~   ^
    ///  ty       position = 0
    /// ```
    ///
    /// *This type is available if Syn is built with the `"derive"` or `"full"`
    /// feature.*
    pub struct QSelf {
        pub lt_token: Token![<],
        pub ty: Box<Type>,
        pub position: usize,
        pub as_token: Option<Token![as]>,
        pub gt_token: Token![>],
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use parse::{Parse, ParseStream, Result};
    use synom::ext::IdentExt;
    #[cfg(feature = "full")]
    use expr;

    impl Parse for Path {
        fn parse(input: ParseStream) -> Result<Self> {
            Self::parse_helper(input, false)
        }
    }

    impl Parse for GenericArgument {
        fn parse(input: ParseStream) -> Result<Self> {
            if input.peek(Lifetime) && !input.peek3(Token![+]) {
                return Ok(GenericArgument::Lifetime(input.parse()?));
            }

            if input.peek(Ident) && input.peek2(Token![=]) {
                return Ok(GenericArgument::Binding(input.parse()?));
            }

            #[cfg(feature = "full")]
            {
                if input.peek(Lit) {
                    let lit = input.call(expr::parsing::expr_lit)?;
                    return Ok(GenericArgument::Const(Expr::Lit(lit)));
                }

                if input.peek(token::Brace) {
                    let block = input.call(expr::parsing::expr_block)?;
                    return Ok(GenericArgument::Const(Expr::Block(block)));
                }
            }

            input.parse().map(GenericArgument::Type)
        }
    }

    impl Parse for AngleBracketedGenericArguments {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(AngleBracketedGenericArguments {
                colon2_token: input.parse()?,
                lt_token: input.parse()?,
                args: {
                    let mut args = Punctuated::new();
                    loop {
                        if input.peek(Token![>]) {
                            break;
                        }
                        let value = input.parse()?;
                        args.push_value(value);
                        if input.peek(Token![>]) {
                            break;
                        }
                        let punct = input.parse()?;
                        args.push_punct(punct);
                    }
                    args
                },
                gt_token: input.parse()?,
            })
        }
    }

    impl Parse for ParenthesizedGenericArguments {
        fn parse(input: ParseStream) -> Result<Self> {
            let content;
            Ok(ParenthesizedGenericArguments {
                paren_token: parenthesized!(content in input),
                inputs: content.parse_synom(Punctuated::parse_terminated)?,
                output: input.call(ReturnType::without_plus)?,
            })
        }
    }

    impl Parse for PathSegment {
        fn parse(input: ParseStream) -> Result<Self> {
            Self::parse_helper(input, false)
        }
    }

    impl PathSegment {
        fn parse_helper(input: ParseStream, expr_style: bool) -> Result<Self> {
            if input.peek(Token![super])
                || input.peek(Token![self])
                || input.peek(Token![Self])
                || input.peek(Token![crate])
                || input.peek(Token![extern])
            {
                let ident = input.parse_synom(Ident::parse_any)?;
                return Ok(PathSegment::from(ident));
            }

            let ident = input.parse()?;
            if !expr_style && input.peek(Token![<]) && !input.peek(Token![<=])
                || input.peek(Token![::]) && input.peek3(Token![<])
            {
                Ok(PathSegment {
                    ident: ident,
                    arguments: PathArguments::AngleBracketed(input.parse()?),
                })
            } else {
                Ok(PathSegment::from(ident))
            }
        }
    }

    impl Parse for Binding {
        fn parse(input: ParseStream) -> Result<Self> {
            Ok(Binding {
                ident: input.parse()?,
                eq_token: input.parse()?,
                ty: input.parse()?,
            })
        }
    }

    impl Path {
        pub fn parse_mod_style(input: ParseStream) -> Result<Self> {
            Ok(Path {
                leading_colon: input.parse()?,
                segments: {
                    let mut segments = Punctuated::new();
                    loop {
                        if !input.peek(Ident)
                            && !input.peek(Token![super])
                            && !input.peek(Token![self])
                            && !input.peek(Token![Self])
                            && !input.peek(Token![crate])
                            && !input.peek(Token![extern])
                        {
                            break;
                        }
                        let ident = Ident::parse_any2(input)?;
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

        fn parse_helper(input: ParseStream, expr_style: bool) -> Result<Self> {
            if input.peek(Token![dyn]) {
                return Err(input.error("expected path"));
            }

            Ok(Path {
                leading_colon: input.parse()?,
                segments: {
                    let mut segments = Punctuated::new();
                    let value = PathSegment::parse_helper(input, expr_style)?;
                    segments.push_value(value);
                    while input.peek(Token![::]) {
                        let punct: Token![::] = input.parse()?;
                        segments.push_punct(punct);
                        let value = PathSegment::parse_helper(input, expr_style)?;
                        segments.push_value(value);
                    }
                    segments
                },
            })
        }
    }

    pub fn qpath(input: ParseStream, expr_style: bool) -> Result<(Option<QSelf>, Path)> {
        if input.peek(Token![<]) {
            let lt_token: Token![<] = input.parse()?;
            let this: Type = input.parse()?;
            let path = if input.peek(Token![as]) {
                let as_token: Token![as] = input.parse()?;
                let path: Path = input.parse()?;
                Some((as_token, path))
            } else {
                None
            };
            let gt_token: Token![>] = input.parse()?;
            let colon2_token: Token![::] = input.parse()?;
            let mut rest = Punctuated::new();
            loop {
                let path = PathSegment::parse_helper(input, expr_style)?;
                rest.push_value(path);
                if !input.peek(Token![::]) {
                    break;
                }
                let punct: Token![::] = input.parse()?;
                rest.push_punct(punct);
            }
            let (position, as_token, path) = match path {
                Some((as_token, mut path)) => {
                    let pos = path.segments.len();
                    path.segments.push_punct(colon2_token);
                    path.segments.extend(rest.into_pairs());
                    (pos, Some(as_token), path)
                }
                None => {
                    let path = Path {
                        leading_colon: Some(colon2_token),
                        segments: rest,
                    };
                    (0, None, path)
                }
            };
            let qself = QSelf {
                lt_token: lt_token,
                ty: Box::new(this),
                position: position,
                as_token: as_token,
                gt_token: gt_token,
            };
            Ok((Some(qself), path))
        } else {
            let path = Path::parse_helper(input, expr_style)?;
            Ok((None, path))
        }
    }
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use proc_macro2::TokenStream;
    use quote::ToTokens;

    impl ToTokens for Path {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.leading_colon.to_tokens(tokens);
            self.segments.to_tokens(tokens);
        }
    }

    impl ToTokens for PathSegment {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.ident.to_tokens(tokens);
            self.arguments.to_tokens(tokens);
        }
    }

    impl ToTokens for PathArguments {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match *self {
                PathArguments::None => {}
                PathArguments::AngleBracketed(ref arguments) => {
                    arguments.to_tokens(tokens);
                }
                PathArguments::Parenthesized(ref arguments) => {
                    arguments.to_tokens(tokens);
                }
            }
        }
    }

    impl ToTokens for GenericArgument {
        #[cfg_attr(feature = "cargo-clippy", allow(match_same_arms))]
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match *self {
                GenericArgument::Lifetime(ref lt) => lt.to_tokens(tokens),
                GenericArgument::Type(ref ty) => ty.to_tokens(tokens),
                GenericArgument::Binding(ref tb) => tb.to_tokens(tokens),
                GenericArgument::Const(ref e) => match *e {
                    Expr::Lit(_) => e.to_tokens(tokens),

                    // NOTE: We should probably support parsing blocks with only
                    // expressions in them without the full feature for const
                    // generics.
                    #[cfg(feature = "full")]
                    Expr::Block(_) => e.to_tokens(tokens),

                    // ERROR CORRECTION: Add braces to make sure that the
                    // generated code is valid.
                    _ => token::Brace::default().surround(tokens, |tokens| {
                        e.to_tokens(tokens);
                    }),
                },
            }
        }
    }

    impl ToTokens for AngleBracketedGenericArguments {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.colon2_token.to_tokens(tokens);
            self.lt_token.to_tokens(tokens);

            // Print lifetimes before types and consts, all before bindings,
            // regardless of their order in self.args.
            //
            // TODO: ordering rules for const arguments vs type arguments have
            // not been settled yet. https://github.com/rust-lang/rust/issues/44580
            let mut trailing_or_empty = true;
            for param in self.args.pairs() {
                if let GenericArgument::Lifetime(_) = **param.value() {
                    param.to_tokens(tokens);
                    trailing_or_empty = param.punct().is_some();
                }
            }
            for param in self.args.pairs() {
                match **param.value() {
                    GenericArgument::Type(_) | GenericArgument::Const(_) => {
                        if !trailing_or_empty {
                            <Token![,]>::default().to_tokens(tokens);
                        }
                        param.to_tokens(tokens);
                        trailing_or_empty = param.punct().is_some();
                    }
                    GenericArgument::Lifetime(_) | GenericArgument::Binding(_) => {}
                }
            }
            for param in self.args.pairs() {
                if let GenericArgument::Binding(_) = **param.value() {
                    if !trailing_or_empty {
                        <Token![,]>::default().to_tokens(tokens);
                        trailing_or_empty = true;
                    }
                    param.to_tokens(tokens);
                }
            }

            self.gt_token.to_tokens(tokens);
        }
    }

    impl ToTokens for Binding {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.ident.to_tokens(tokens);
            self.eq_token.to_tokens(tokens);
            self.ty.to_tokens(tokens);
        }
    }

    impl ToTokens for ParenthesizedGenericArguments {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            self.paren_token.surround(tokens, |tokens| {
                self.inputs.to_tokens(tokens);
            });
            self.output.to_tokens(tokens);
        }
    }

    impl<'a> ToTokens for PathTokens<'a> {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let qself = match *self.0 {
                Some(ref qself) => qself,
                None => return self.1.to_tokens(tokens),
            };
            qself.lt_token.to_tokens(tokens);
            qself.ty.to_tokens(tokens);

            // XXX: Gross.
            let pos = if qself.position > 0 && qself.position >= self.1.segments.len() {
                self.1.segments.len() - 1
            } else {
                qself.position
            };
            let mut segments = self.1.segments.pairs();
            if pos > 0 {
                TokensOrDefault(&qself.as_token).to_tokens(tokens);
                self.1.leading_colon.to_tokens(tokens);
                for (i, segment) in segments.by_ref().take(pos).enumerate() {
                    if i + 1 == pos {
                        segment.value().to_tokens(tokens);
                        qself.gt_token.to_tokens(tokens);
                        segment.punct().to_tokens(tokens);
                    } else {
                        segment.to_tokens(tokens);
                    }
                }
            } else {
                qself.gt_token.to_tokens(tokens);
                self.1.leading_colon.to_tokens(tokens);
            }
            for segment in segments {
                segment.to_tokens(tokens);
            }
        }
    }
}
