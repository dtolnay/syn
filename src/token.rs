// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Tokens representing Rust punctuation, keywords, and delimiters.
//!
//! The type names in this module can be difficult to keep straight, so we
//! prefer to use the [`Token!`] macro instead. This is a type-macro that
//! expands to the token type of the given token.
//!
//! [`Token!`]: ../macro.Token.html
//!
//! # Example
//!
//! The [`ItemStatic`] syntax tree node is defined like this.
//!
//! [`ItemStatic`]: ../struct.ItemStatic.html
//!
//! ```
//! # #[macro_use]
//! # extern crate syn;
//! #
//! # use syn::{Attribute, Visibility, Ident, Type, Expr};
//! #
//! pub struct ItemStatic {
//!     pub attrs: Vec<Attribute>,
//!     pub vis: Visibility,
//!     pub static_token: Token![static],
//!     pub mutability: Option<Token![mut]>,
//!     pub ident: Ident,
//!     pub colon_token: Token![:],
//!     pub ty: Box<Type>,
//!     pub eq_token: Token![=],
//!     pub expr: Box<Expr>,
//!     pub semi_token: Token![;],
//! }
//! #
//! # fn main() {}
//! ```
//!
//! # Parsing
//!
//! These tokens can be parsed using the [`Synom`] trait and the parser
//! combinator macros [`punct!`], [`keyword!`], [`parens!`], [`braces!`], and
//! [`brackets!`].
//!
//! [`Synom`]: ../synom/trait.Synom.html
//! [`punct!`]: ../macro.punct.html
//! [`keyword!`]: ../macro.keyword.html
//! [`parens!`]: ../macro.parens.html
//! [`braces!`]: ../macro.braces.html
//! [`brackets!`]: ../macro.brackets.html
//!
//! ```
//! #[macro_use]
//! extern crate syn;
//!
//! use syn::synom::Synom;
//! use syn::{Attribute, Visibility, Ident, Type, Expr};
//! #
//! # struct ItemStatic;
//! # use syn::ItemStatic as SynItemStatic;
//!
//! // Parse the ItemStatic struct shown above.
//! impl Synom for ItemStatic {
//!     named!(parse -> Self, do_parse!(
//! #       (ItemStatic)
//! #   ));
//! # }
//! #
//! # mod example {
//! #   use super::*;
//! #   use super::SynItemStatic as ItemStatic;
//! #
//! #   named!(parse -> ItemStatic, do_parse!(
//!         attrs: many0!(Attribute::parse_outer) >>
//!         vis: syn!(Visibility) >>
//!         static_token: keyword!(static) >>
//!         mutability: option!(keyword!(mut)) >>
//!         ident: syn!(Ident) >>
//!         colon_token: punct!(:) >>
//!         ty: syn!(Type) >>
//!         eq_token: punct!(=) >>
//!         expr: syn!(Expr) >>
//!         semi_token: punct!(;) >>
//!         (ItemStatic {
//!             attrs, vis, static_token, mutability, ident, colon_token,
//!             ty: Box::new(ty), eq_token, expr: Box::new(expr), semi_token,
//!         })
//!     ));
//! }
//! #
//! # fn main() {}
//! ```

use std;
#[cfg(feature = "extra-traits")]
use std::cmp;
#[cfg(feature = "extra-traits")]
use std::fmt::{self, Debug};
#[cfg(feature = "extra-traits")]
use std::hash::{Hash, Hasher};

#[cfg(any(feature = "printing", feature = "parsing"))]
use proc_macro2::Spacing;
use proc_macro2::{Ident, Span};
#[cfg(feature = "printing")]
use proc_macro2::{Punct, TokenStream};
#[cfg(feature = "printing")]
use quote::{ToTokens, TokenStreamExt};

#[cfg(feature = "parsing")]
use error::Result;
#[cfg(feature = "parsing")]
use lookahead;
#[cfg(feature = "parsing")]
use parse::{Lookahead1, Parse, ParseStream};
use span::IntoSpans;

/// Marker trait for types that represent single tokens.
///
/// This trait is sealed and cannot be implemented for types outside of Syn.
#[cfg(feature = "parsing")]
pub trait Token: private::Sealed {
    // Not public API.
    #[doc(hidden)]
    fn peek(lookahead: &Lookahead1) -> bool;

    // Not public API.
    #[doc(hidden)]
    fn display() -> String;
}

#[cfg(feature = "parsing")]
mod private {
    pub trait Sealed {}
}

macro_rules! impl_token {
    ($token:tt $name:ident) => {
        #[cfg(feature = "parsing")]
        impl Token for $name {
            fn peek(lookahead: &Lookahead1) -> bool {
                lookahead::is_token(lookahead, $token)
            }

            fn display() -> String {
                concat!("`", $token, "`").to_owned()
            }
        }

        #[cfg(feature = "parsing")]
        impl private::Sealed for $name {}
    };
}

macro_rules! define_keywords {
    ($($token:tt pub struct $name:ident #[$doc:meta])*) => {
        $(
            #[cfg_attr(feature = "clone-impls", derive(Copy, Clone))]
            #[$doc]
            ///
            /// Don't try to remember the name of this type -- use the [`Token!`]
            /// macro instead.
            ///
            /// [`Token!`]: index.html
            pub struct $name {
                pub span: Span,
            }

            #[doc(hidden)]
            #[allow(non_snake_case)]
            pub fn $name<S: IntoSpans<[Span; 1]>>(span: S) -> $name {
                $name {
                    span: span.into_spans()[0],
                }
            }

            impl_token!($token $name);

            impl std::default::Default for $name {
                fn default() -> Self {
                    $name(Span::call_site())
                }
            }

            #[cfg(feature = "extra-traits")]
            impl Debug for $name {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    f.write_str(stringify!($name))
                }
            }

            #[cfg(feature = "extra-traits")]
            impl cmp::Eq for $name {}

            #[cfg(feature = "extra-traits")]
            impl PartialEq for $name {
                fn eq(&self, _other: &$name) -> bool {
                    true
                }
            }

            #[cfg(feature = "extra-traits")]
            impl Hash for $name {
                fn hash<H: Hasher>(&self, _state: &mut H) {}
            }

            #[cfg(feature = "printing")]
            impl ToTokens for $name {
                fn to_tokens(&self, tokens: &mut TokenStream) {
                    printing::keyword($token, &self.span, tokens);
                }
            }

            #[cfg(feature = "parsing")]
            impl Parse for $name {
                fn parse(input: ParseStream) -> Result<Self> {
                    parsing::keyword(input, $token).map($name)
                }
            }
        )*
    };
}

macro_rules! define_punctuation_structs {
    ($($token:tt pub struct $name:ident/$len:tt #[$doc:meta])*) => {
        $(
            #[cfg_attr(feature = "clone-impls", derive(Copy, Clone))]
            #[$doc]
            ///
            /// Don't try to remember the name of this type -- use the [`Token!`]
            /// macro instead.
            ///
            /// [`Token!`]: index.html
            pub struct $name {
                pub spans: [Span; $len],
            }

            #[doc(hidden)]
            #[allow(non_snake_case)]
            pub fn $name<S: IntoSpans<[Span; $len]>>(spans: S) -> $name {
                $name {
                    spans: spans.into_spans(),
                }
            }

            impl_token!($token $name);

            impl std::default::Default for $name {
                fn default() -> Self {
                    $name([Span::call_site(); $len])
                }
            }

            #[cfg(feature = "extra-traits")]
            impl Debug for $name {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    f.write_str(stringify!($name))
                }
            }

            #[cfg(feature = "extra-traits")]
            impl cmp::Eq for $name {}

            #[cfg(feature = "extra-traits")]
            impl PartialEq for $name {
                fn eq(&self, _other: &$name) -> bool {
                    true
                }
            }

            #[cfg(feature = "extra-traits")]
            impl Hash for $name {
                fn hash<H: Hasher>(&self, _state: &mut H) {}
            }
        )*
    };
}

macro_rules! define_punctuation {
    ($($token:tt pub struct $name:ident/$len:tt #[$doc:meta])*) => {
        $(
            define_punctuation_structs! {
                $token pub struct $name/$len #[$doc]
            }

            #[cfg(feature = "printing")]
            impl ToTokens for $name {
                fn to_tokens(&self, tokens: &mut TokenStream) {
                    printing::punct($token, &self.spans, tokens);
                }
            }

            #[cfg(feature = "parsing")]
            impl Parse for $name {
                fn parse(input: ParseStream) -> Result<Self> {
                    parsing::punct(input, $token).map($name::<[Span; $len]>)
                }
            }
        )*
    };
}

macro_rules! define_delimiters {
    ($($token:tt pub struct $name:ident #[$doc:meta])*) => {
        $(
            #[cfg_attr(feature = "clone-impls", derive(Copy, Clone))]
            #[$doc]
            pub struct $name {
                pub span: Span,
            }

            #[doc(hidden)]
            #[allow(non_snake_case)]
            pub fn $name<S: IntoSpans<[Span; 1]>>(span: S) -> $name {
                $name {
                    span: span.into_spans()[0],
                }
            }

            impl std::default::Default for $name {
                fn default() -> Self {
                    $name(Span::call_site())
                }
            }

            #[cfg(feature = "extra-traits")]
            impl Debug for $name {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    f.write_str(stringify!($name))
                }
            }

            #[cfg(feature = "extra-traits")]
            impl cmp::Eq for $name {}

            #[cfg(feature = "extra-traits")]
            impl PartialEq for $name {
                fn eq(&self, _other: &$name) -> bool {
                    true
                }
            }

            #[cfg(feature = "extra-traits")]
            impl Hash for $name {
                fn hash<H: Hasher>(&self, _state: &mut H) {}
            }

            impl $name {
                #[cfg(feature = "printing")]
                pub fn surround<F>(&self, tokens: &mut TokenStream, f: F)
                where
                    F: FnOnce(&mut TokenStream),
                {
                    printing::delim($token, &self.span, tokens, f);
                }

                #[cfg(feature = "parsing")]
                pub fn parse<F, R>(
                    tokens: $crate::buffer::Cursor,
                    f: F,
                ) -> $crate::synom::PResult<($name, R)>
                where
                    F: FnOnce($crate::buffer::Cursor) -> $crate::synom::PResult<R>,
                {
                    parsing::delim($token, tokens, $name, f)
                }
            }
        )*
    };
}

define_punctuation_structs! {
    "'" pub struct Apostrophe/1 /// `'`
    "_" pub struct Underscore/1 /// `_`
}

// Implement Clone anyway because it is required for cloning Lifetime.
#[cfg(not(feature = "clone-impls"))]
impl Clone for Apostrophe {
    fn clone(&self) -> Self {
        Apostrophe(self.spans)
    }
}

#[cfg(feature = "printing")]
impl ToTokens for Apostrophe {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut token = Punct::new('\'', Spacing::Joint);
        token.set_span(self.spans[0]);
        tokens.append(token);
    }
}

#[cfg(feature = "parsing")]
impl Parse for Apostrophe {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step_cursor(|cursor| {
            if let Some((punct, rest)) = cursor.punct() {
                if punct.as_char() == '\'' && punct.spacing() == Spacing::Joint {
                    return Ok((Apostrophe(punct.span()), rest));
                }
            }
            Err(cursor.error("expected `'`"))
        })
    }
}

#[cfg(feature = "printing")]
impl ToTokens for Underscore {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(Ident::new("_", self.spans[0]));
    }
}

#[cfg(feature = "parsing")]
impl Parse for Underscore {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step_cursor(|cursor| {
            if let Some((ident, rest)) = cursor.ident() {
                if ident == "_" {
                    return Ok((Underscore(ident.span()), rest));
                }
            }
            if let Some((punct, rest)) = cursor.punct() {
                if punct.as_char() == '_' {
                    return Ok((Underscore(punct.span()), rest));
                }
            }
            Err(cursor.error("expected `_`"))
        })
    }
}

define_keywords! {
    "as"          pub struct As           /// `as`
    "async"       pub struct Async        /// `async`
    "auto"        pub struct Auto         /// `auto`
    "box"         pub struct Box          /// `box`
    "break"       pub struct Break        /// `break`
    "Self"        pub struct CapSelf      /// `Self`
    "const"       pub struct Const        /// `const`
    "continue"    pub struct Continue     /// `continue`
    "crate"       pub struct Crate        /// `crate`
    "default"     pub struct Default      /// `default`
    "dyn"         pub struct Dyn          /// `dyn`
    "else"        pub struct Else         /// `else`
    "enum"        pub struct Enum         /// `enum`
    "existential" pub struct Existential  /// `existential`
    "extern"      pub struct Extern       /// `extern`
    "fn"          pub struct Fn           /// `fn`
    "for"         pub struct For          /// `for`
    "if"          pub struct If           /// `if`
    "impl"        pub struct Impl         /// `impl`
    "in"          pub struct In           /// `in`
    "let"         pub struct Let          /// `let`
    "loop"        pub struct Loop         /// `loop`
    "macro"       pub struct Macro        /// `macro`
    "match"       pub struct Match        /// `match`
    "mod"         pub struct Mod          /// `mod`
    "move"        pub struct Move         /// `move`
    "mut"         pub struct Mut          /// `mut`
    "pub"         pub struct Pub          /// `pub`
    "ref"         pub struct Ref          /// `ref`
    "return"      pub struct Return       /// `return`
    "self"        pub struct Self_        /// `self`
    "static"      pub struct Static       /// `static`
    "struct"      pub struct Struct       /// `struct`
    "super"       pub struct Super        /// `super`
    "trait"       pub struct Trait        /// `trait`
    "try"         pub struct Try          /// `try`
    "type"        pub struct Type         /// `type`
    "union"       pub struct Union        /// `union`
    "unsafe"      pub struct Unsafe       /// `unsafe`
    "use"         pub struct Use          /// `use`
    "where"       pub struct Where        /// `where`
    "while"       pub struct While        /// `while`
    "yield"       pub struct Yield        /// `yield`
}

define_punctuation! {
    "+"           pub struct Add/1        /// `+`
    "+="          pub struct AddEq/2      /// `+=`
    "&"           pub struct And/1        /// `&`
    "&&"          pub struct AndAnd/2     /// `&&`
    "&="          pub struct AndEq/2      /// `&=`
    "@"           pub struct At/1         /// `@`
    "!"           pub struct Bang/1       /// `!`
    "^"           pub struct Caret/1      /// `^`
    "^="          pub struct CaretEq/2    /// `^=`
    ":"           pub struct Colon/1      /// `:`
    "::"          pub struct Colon2/2     /// `::`
    ","           pub struct Comma/1      /// `,`
    "/"           pub struct Div/1        /// `/`
    "/="          pub struct DivEq/2      /// `/=`
    "$"           pub struct Dollar/1     /// `$`
    "."           pub struct Dot/1        /// `.`
    ".."          pub struct Dot2/2       /// `..`
    "..."         pub struct Dot3/3       /// `...`
    "..="         pub struct DotDotEq/3   /// `..=`
    "="           pub struct Eq/1         /// `=`
    "=="          pub struct EqEq/2       /// `==`
    ">="          pub struct Ge/2         /// `>=`
    ">"           pub struct Gt/1         /// `>`
    "<="          pub struct Le/2         /// `<=`
    "<"           pub struct Lt/1         /// `<`
    "*="          pub struct MulEq/2      /// `*=`
    "!="          pub struct Ne/2         /// `!=`
    "|"           pub struct Or/1         /// `|`
    "|="          pub struct OrEq/2       /// `|=`
    "||"          pub struct OrOr/2       /// `||`
    "#"           pub struct Pound/1      /// `#`
    "?"           pub struct Question/1   /// `?`
    "->"          pub struct RArrow/2     /// `->`
    "<-"          pub struct LArrow/2     /// `<-`
    "%"           pub struct Rem/1        /// `%`
    "%="          pub struct RemEq/2      /// `%=`
    "=>"          pub struct FatArrow/2   /// `=>`
    ";"           pub struct Semi/1       /// `;`
    "<<"          pub struct Shl/2        /// `<<`
    "<<="         pub struct ShlEq/3      /// `<<=`
    ">>"          pub struct Shr/2        /// `>>`
    ">>="         pub struct ShrEq/3      /// `>>=`
    "*"           pub struct Star/1       /// `*`
    "-"           pub struct Sub/1        /// `-`
    "-="          pub struct SubEq/2      /// `-=`
}

define_delimiters! {
    "{"           pub struct Brace        /// `{...}`
    "["           pub struct Bracket      /// `[...]`
    "("           pub struct Paren        /// `(...)`
    " "           pub struct Group        /// None-delimited group
}

/// A type-macro that expands to the name of the Rust type representation of a
/// given token.
///
/// See the [token module] documentation for details and examples.
///
/// [token module]: token/index.html
// Unfortunate duplication due to a rustdoc bug.
// https://github.com/rust-lang/rust/issues/45939
#[macro_export]
#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! Token {
    (as)          => { $crate::token::As };
    (async)       => { $crate::token::Async };
    (auto)        => { $crate::token::Auto };
    (box)         => { $crate::token::Box };
    (break)       => { $crate::token::Break };
    (Self)        => { $crate::token::CapSelf };
    (const)       => { $crate::token::Const };
    (continue)    => { $crate::token::Continue };
    (crate)       => { $crate::token::Crate };
    (default)     => { $crate::token::Default };
    (dyn)         => { $crate::token::Dyn };
    (else)        => { $crate::token::Else };
    (enum)        => { $crate::token::Enum };
    (existential) => { $crate::token::Existential };
    (extern)      => { $crate::token::Extern };
    (fn)          => { $crate::token::Fn };
    (for)         => { $crate::token::For };
    (if)          => { $crate::token::If };
    (impl)        => { $crate::token::Impl };
    (in)          => { $crate::token::In };
    (let)         => { $crate::token::Let };
    (loop)        => { $crate::token::Loop };
    (macro)       => { $crate::token::Macro };
    (match)       => { $crate::token::Match };
    (mod)         => { $crate::token::Mod };
    (move)        => { $crate::token::Move };
    (mut)         => { $crate::token::Mut };
    (pub)         => { $crate::token::Pub };
    (ref)         => { $crate::token::Ref };
    (return)      => { $crate::token::Return };
    (self)        => { $crate::token::Self_ };
    (static)      => { $crate::token::Static };
    (struct)      => { $crate::token::Struct };
    (super)       => { $crate::token::Super };
    (trait)       => { $crate::token::Trait };
    (try)         => { $crate::token::Try };
    (type)        => { $crate::token::Type };
    (union)       => { $crate::token::Union };
    (unsafe)      => { $crate::token::Unsafe };
    (use)         => { $crate::token::Use };
    (where)       => { $crate::token::Where };
    (while)       => { $crate::token::While };
    (yield)       => { $crate::token::Yield };
    (+)           => { $crate::token::Add };
    (+=)          => { $crate::token::AddEq };
    (&)           => { $crate::token::And };
    (&&)          => { $crate::token::AndAnd };
    (&=)          => { $crate::token::AndEq };
    (@)           => { $crate::token::At };
    (!)           => { $crate::token::Bang };
    (^)           => { $crate::token::Caret };
    (^=)          => { $crate::token::CaretEq };
    (:)           => { $crate::token::Colon };
    (::)          => { $crate::token::Colon2 };
    (,)           => { $crate::token::Comma };
    (/)           => { $crate::token::Div };
    (/=)          => { $crate::token::DivEq };
    (.)           => { $crate::token::Dot };
    (..)          => { $crate::token::Dot2 };
    (...)         => { $crate::token::Dot3 };
    (..=)         => { $crate::token::DotDotEq };
    (=)           => { $crate::token::Eq };
    (==)          => { $crate::token::EqEq };
    (>=)          => { $crate::token::Ge };
    (>)           => { $crate::token::Gt };
    (<=)          => { $crate::token::Le };
    (<)           => { $crate::token::Lt };
    (*=)          => { $crate::token::MulEq };
    (!=)          => { $crate::token::Ne };
    (|)           => { $crate::token::Or };
    (|=)          => { $crate::token::OrEq };
    (||)          => { $crate::token::OrOr };
    (#)           => { $crate::token::Pound };
    (?)           => { $crate::token::Question };
    (->)          => { $crate::token::RArrow };
    (<-)          => { $crate::token::LArrow };
    (%)           => { $crate::token::Rem };
    (%=)          => { $crate::token::RemEq };
    (=>)          => { $crate::token::FatArrow };
    (;)           => { $crate::token::Semi };
    (<<)          => { $crate::token::Shl };
    (<<=)         => { $crate::token::ShlEq };
    (>>)          => { $crate::token::Shr };
    (>>=)         => { $crate::token::ShrEq };
    (*)           => { $crate::token::Star };
    (-)           => { $crate::token::Sub };
    (-=)          => { $crate::token::SubEq };
    (_)           => { $crate::token::Underscore };
}

/// Parse a single Rust punctuation token.
///
/// See the [token module] documentation for details and examples.
///
/// [token module]: token/index.html
///
/// *This macro is available if Syn is built with the `"parsing"` feature.*
#[cfg(feature = "parsing")]
#[macro_export]
#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! punct {
    ($i:expr, +)   => { call!($i, <$crate::token::Add as $crate::synom::Synom>::parse) };
    ($i:expr, +=)  => { call!($i, <$crate::token::AddEq as $crate::synom::Synom>::parse) };
    ($i:expr, &)   => { call!($i, <$crate::token::And as $crate::synom::Synom>::parse) };
    ($i:expr, &&)  => { call!($i, <$crate::token::AndAnd as $crate::synom::Synom>::parse) };
    ($i:expr, &=)  => { call!($i, <$crate::token::AndEq as $crate::synom::Synom>::parse) };
    ($i:expr, @)   => { call!($i, <$crate::token::At as $crate::synom::Synom>::parse) };
    ($i:expr, !)   => { call!($i, <$crate::token::Bang as $crate::synom::Synom>::parse) };
    ($i:expr, ^)   => { call!($i, <$crate::token::Caret as $crate::synom::Synom>::parse) };
    ($i:expr, ^=)  => { call!($i, <$crate::token::CaretEq as $crate::synom::Synom>::parse) };
    ($i:expr, :)   => { call!($i, <$crate::token::Colon as $crate::synom::Synom>::parse) };
    ($i:expr, ::)  => { call!($i, <$crate::token::Colon2 as $crate::synom::Synom>::parse) };
    ($i:expr, ,)   => { call!($i, <$crate::token::Comma as $crate::synom::Synom>::parse) };
    ($i:expr, /)   => { call!($i, <$crate::token::Div as $crate::synom::Synom>::parse) };
    ($i:expr, /=)  => { call!($i, <$crate::token::DivEq as $crate::synom::Synom>::parse) };
    ($i:expr, .)   => { call!($i, <$crate::token::Dot as $crate::synom::Synom>::parse) };
    ($i:expr, ..)  => { call!($i, <$crate::token::Dot2 as $crate::synom::Synom>::parse) };
    ($i:expr, ...) => { call!($i, <$crate::token::Dot3 as $crate::synom::Synom>::parse) };
    ($i:expr, ..=) => { call!($i, <$crate::token::DotDotEq as $crate::synom::Synom>::parse) };
    ($i:expr, =)   => { call!($i, <$crate::token::Eq as $crate::synom::Synom>::parse) };
    ($i:expr, ==)  => { call!($i, <$crate::token::EqEq as $crate::synom::Synom>::parse) };
    ($i:expr, >=)  => { call!($i, <$crate::token::Ge as $crate::synom::Synom>::parse) };
    ($i:expr, >)   => { call!($i, <$crate::token::Gt as $crate::synom::Synom>::parse) };
    ($i:expr, <=)  => { call!($i, <$crate::token::Le as $crate::synom::Synom>::parse) };
    ($i:expr, <)   => { call!($i, <$crate::token::Lt as $crate::synom::Synom>::parse) };
    ($i:expr, *=)  => { call!($i, <$crate::token::MulEq as $crate::synom::Synom>::parse) };
    ($i:expr, !=)  => { call!($i, <$crate::token::Ne as $crate::synom::Synom>::parse) };
    ($i:expr, |)   => { call!($i, <$crate::token::Or as $crate::synom::Synom>::parse) };
    ($i:expr, |=)  => { call!($i, <$crate::token::OrEq as $crate::synom::Synom>::parse) };
    ($i:expr, ||)  => { call!($i, <$crate::token::OrOr as $crate::synom::Synom>::parse) };
    ($i:expr, #)   => { call!($i, <$crate::token::Pound as $crate::synom::Synom>::parse) };
    ($i:expr, ?)   => { call!($i, <$crate::token::Question as $crate::synom::Synom>::parse) };
    ($i:expr, ->)  => { call!($i, <$crate::token::RArrow as $crate::synom::Synom>::parse) };
    ($i:expr, <-)  => { call!($i, <$crate::token::LArrow as $crate::synom::Synom>::parse) };
    ($i:expr, %)   => { call!($i, <$crate::token::Rem as $crate::synom::Synom>::parse) };
    ($i:expr, %=)  => { call!($i, <$crate::token::RemEq as $crate::synom::Synom>::parse) };
    ($i:expr, =>)  => { call!($i, <$crate::token::FatArrow as $crate::synom::Synom>::parse) };
    ($i:expr, ;)   => { call!($i, <$crate::token::Semi as $crate::synom::Synom>::parse) };
    ($i:expr, <<)  => { call!($i, <$crate::token::Shl as $crate::synom::Synom>::parse) };
    ($i:expr, <<=) => { call!($i, <$crate::token::ShlEq as $crate::synom::Synom>::parse) };
    ($i:expr, >>)  => { call!($i, <$crate::token::Shr as $crate::synom::Synom>::parse) };
    ($i:expr, >>=) => { call!($i, <$crate::token::ShrEq as $crate::synom::Synom>::parse) };
    ($i:expr, *)   => { call!($i, <$crate::token::Star as $crate::synom::Synom>::parse) };
    ($i:expr, -)   => { call!($i, <$crate::token::Sub as $crate::synom::Synom>::parse) };
    ($i:expr, -=)  => { call!($i, <$crate::token::SubEq as $crate::synom::Synom>::parse) };
    ($i:expr, _)   => { call!($i, <$crate::token::Underscore as $crate::synom::Synom>::parse) };
}

/// Parse a single Rust keyword token.
///
/// See the [token module] documentation for details and examples.
///
/// [token module]: token/index.html
///
/// *This macro is available if Syn is built with the `"parsing"` feature.*
#[cfg(feature = "parsing")]
#[macro_export]
#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! keyword {
    ($i:expr, as)          => { call!($i, <$crate::token::As as $crate::synom::Synom>::parse) };
    ($i:expr, async)       => { call!($i, <$crate::token::Async as $crate::synom::Synom>::parse) };
    ($i:expr, auto)        => { call!($i, <$crate::token::Auto as $crate::synom::Synom>::parse) };
    ($i:expr, box)         => { call!($i, <$crate::token::Box as $crate::synom::Synom>::parse) };
    ($i:expr, break)       => { call!($i, <$crate::token::Break as $crate::synom::Synom>::parse) };
    ($i:expr, Self)        => { call!($i, <$crate::token::CapSelf as $crate::synom::Synom>::parse) };
    ($i:expr, const)       => { call!($i, <$crate::token::Const as $crate::synom::Synom>::parse) };
    ($i:expr, continue)    => { call!($i, <$crate::token::Continue as $crate::synom::Synom>::parse) };
    ($i:expr, crate)       => { call!($i, <$crate::token::Crate as $crate::synom::Synom>::parse) };
    ($i:expr, default)     => { call!($i, <$crate::token::Default as $crate::synom::Synom>::parse) };
    ($i:expr, dyn)         => { call!($i, <$crate::token::Dyn as $crate::synom::Synom>::parse) };
    ($i:expr, else)        => { call!($i, <$crate::token::Else as $crate::synom::Synom>::parse) };
    ($i:expr, enum)        => { call!($i, <$crate::token::Enum as $crate::synom::Synom>::parse) };
    ($i:expr, extern)      => { call!($i, <$crate::token::Extern as $crate::synom::Synom>::parse) };
    ($i:expr, existential) => { call!($i, <$crate::token::Existential as $crate::synom::Synom>::parse) };
    ($i:expr, fn)          => { call!($i, <$crate::token::Fn as $crate::synom::Synom>::parse) };
    ($i:expr, for)         => { call!($i, <$crate::token::For as $crate::synom::Synom>::parse) };
    ($i:expr, if)          => { call!($i, <$crate::token::If as $crate::synom::Synom>::parse) };
    ($i:expr, impl)        => { call!($i, <$crate::token::Impl as $crate::synom::Synom>::parse) };
    ($i:expr, in)          => { call!($i, <$crate::token::In as $crate::synom::Synom>::parse) };
    ($i:expr, let)         => { call!($i, <$crate::token::Let as $crate::synom::Synom>::parse) };
    ($i:expr, loop)        => { call!($i, <$crate::token::Loop as $crate::synom::Synom>::parse) };
    ($i:expr, macro)       => { call!($i, <$crate::token::Macro as $crate::synom::Synom>::parse) };
    ($i:expr, match)       => { call!($i, <$crate::token::Match as $crate::synom::Synom>::parse) };
    ($i:expr, mod)         => { call!($i, <$crate::token::Mod as $crate::synom::Synom>::parse) };
    ($i:expr, move)        => { call!($i, <$crate::token::Move as $crate::synom::Synom>::parse) };
    ($i:expr, mut)         => { call!($i, <$crate::token::Mut as $crate::synom::Synom>::parse) };
    ($i:expr, pub)         => { call!($i, <$crate::token::Pub as $crate::synom::Synom>::parse) };
    ($i:expr, ref)         => { call!($i, <$crate::token::Ref as $crate::synom::Synom>::parse) };
    ($i:expr, return)      => { call!($i, <$crate::token::Return as $crate::synom::Synom>::parse) };
    ($i:expr, self)        => { call!($i, <$crate::token::Self_ as $crate::synom::Synom>::parse) };
    ($i:expr, static)      => { call!($i, <$crate::token::Static as $crate::synom::Synom>::parse) };
    ($i:expr, struct)      => { call!($i, <$crate::token::Struct as $crate::synom::Synom>::parse) };
    ($i:expr, super)       => { call!($i, <$crate::token::Super as $crate::synom::Synom>::parse) };
    ($i:expr, trait)       => { call!($i, <$crate::token::Trait as $crate::synom::Synom>::parse) };
    ($i:expr, try)         => { call!($i, <$crate::token::Try as $crate::synom::Synom>::parse) };
    ($i:expr, type)        => { call!($i, <$crate::token::Type as $crate::synom::Synom>::parse) };
    ($i:expr, union)       => { call!($i, <$crate::token::Union as $crate::synom::Synom>::parse) };
    ($i:expr, unsafe)      => { call!($i, <$crate::token::Unsafe as $crate::synom::Synom>::parse) };
    ($i:expr, use)         => { call!($i, <$crate::token::Use as $crate::synom::Synom>::parse) };
    ($i:expr, where)       => { call!($i, <$crate::token::Where as $crate::synom::Synom>::parse) };
    ($i:expr, while)       => { call!($i, <$crate::token::While as $crate::synom::Synom>::parse) };
    ($i:expr, yield)       => { call!($i, <$crate::token::Yield as $crate::synom::Synom>::parse) };
}

macro_rules! ident_from_token {
    ($token:ident) => {
        impl From<Token![$token]> for Ident {
            fn from(token: Token![$token]) -> Ident {
                Ident::new(stringify!($token), token.span)
            }
        }
    };
}

ident_from_token!(self);
ident_from_token!(Self);
ident_from_token!(super);
ident_from_token!(crate);
ident_from_token!(extern);

#[cfg(feature = "parsing")]
mod parsing {
    use proc_macro2::{Delimiter, Spacing, Span};

    use buffer::Cursor;
    use error::{Error, Result};
    use parse::ParseStream;
    use parse_error;
    use span::FromSpans;
    use synom::PResult;

    pub fn keyword(input: ParseStream, token: &str) -> Result<Span> {
        input.step_cursor(|cursor| {
            if let Some((ident, rest)) = cursor.ident() {
                if ident == token {
                    return Ok((ident.span(), rest));
                }
            }
            Err(cursor.error(format!("expected `{}`", token)))
        })
    }

    pub fn punct<S: FromSpans>(input: ParseStream, token: &str) -> Result<S> {
        input.step_cursor(|cursor| {
            let mut cursor = *cursor;
            let mut spans = [cursor.span(); 3];
            assert!(token.len() <= spans.len());

            for (i, ch) in token.chars().enumerate() {
                match cursor.punct() {
                    Some((punct, rest)) => {
                        spans[i] = punct.span();
                        if punct.as_char() != ch {
                            break;
                        } else if i == token.len() - 1 {
                            return Ok((S::from_spans(&spans), rest));
                        } else if punct.spacing() != Spacing::Joint {
                            break;
                        }
                        cursor = rest;
                    }
                    None => break,
                }
            }

            Err(Error::new(spans[0], format!("expected `{}`", token)))
        })
    }

    pub fn delim<'a, F, R, T>(
        delim: &str,
        tokens: Cursor<'a>,
        new: fn(Span) -> T,
        f: F,
    ) -> PResult<'a, (T, R)>
    where
        F: FnOnce(Cursor) -> PResult<R>,
    {
        // NOTE: We should support none-delimited sequences here.
        let delim = match delim {
            "(" => Delimiter::Parenthesis,
            "{" => Delimiter::Brace,
            "[" => Delimiter::Bracket,
            " " => Delimiter::None,
            _ => panic!("unknown delimiter: {}", delim),
        };

        if let Some((inside, span, rest)) = tokens.group(delim) {
            match f(inside) {
                Ok((ret, remaining)) => {
                    if remaining.eof() {
                        return Ok(((new(span), ret), rest));
                    }
                }
                Err(err) => return Err(err),
            }
        }
        parse_error()
    }
}

#[cfg(feature = "printing")]
mod printing {
    use proc_macro2::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream};
    use quote::TokenStreamExt;

    pub fn punct(s: &str, spans: &[Span], tokens: &mut TokenStream) {
        assert_eq!(s.len(), spans.len());

        let mut chars = s.chars();
        let mut spans = spans.iter();
        let ch = chars.next_back().unwrap();
        let span = spans.next_back().unwrap();
        for (ch, span) in chars.zip(spans) {
            let mut op = Punct::new(ch, Spacing::Joint);
            op.set_span(*span);
            tokens.append(op);
        }

        let mut op = Punct::new(ch, Spacing::Alone);
        op.set_span(*span);
        tokens.append(op);
    }

    pub fn keyword(s: &str, span: &Span, tokens: &mut TokenStream) {
        tokens.append(Ident::new(s, *span));
    }

    pub fn delim<F>(s: &str, span: &Span, tokens: &mut TokenStream, f: F)
    where
        F: FnOnce(&mut TokenStream),
    {
        let delim = match s {
            "(" => Delimiter::Parenthesis,
            "[" => Delimiter::Bracket,
            "{" => Delimiter::Brace,
            " " => Delimiter::None,
            _ => panic!("unknown delimiter: {}", s),
        };
        let mut inner = TokenStream::new();
        f(&mut inner);
        let mut g = Group::new(delim, inner);
        g.set_span(*span);
        tokens.append(g);
    }
}
