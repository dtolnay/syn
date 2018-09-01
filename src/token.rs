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
//! # extern crate syn;
//! #
//! # use syn::{Attribute, Expr, Ident, Token, Type, Visibility};
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
//! Keywords and punctuation can be parsed through the [`ParseStream::parse`]
//! method. Delimiter tokens are parsed using the [`parenthesized!`],
//! [`bracketed!`] and [`braced!`] macros.
//!
//! [`ParseStream::parse`]: ../parse/struct.ParseBuffer.html#method.parse
//! [`parenthesized!`]: ../macro.parenthesized.html
//! [`bracketed!`]: ../macro.bracketed.html
//! [`braced!`]: ../macro.braced.html
//!
//! ```
//! # extern crate syn;
//! #
//! use syn::Attribute;
//! use syn::parse::{Parse, ParseStream, Result};
//! #
//! # enum ItemStatic {}
//!
//! // Parse the ItemStatic struct shown above.
//! impl Parse for ItemStatic {
//!     fn parse(input: ParseStream) -> Result<Self> {
//!         # use syn::ItemStatic;
//!         # fn parse(input: ParseStream) -> Result<ItemStatic> {
//!         Ok(ItemStatic {
//!             attrs: input.call(Attribute::parse_outer)?,
//!             vis: input.parse()?,
//!             static_token: input.parse()?,
//!             mutability: input.parse()?,
//!             ident: input.parse()?,
//!             colon_token: input.parse()?,
//!             ty: input.parse()?,
//!             eq_token: input.parse()?,
//!             expr: input.parse()?,
//!             semi_token: input.parse()?,
//!         })
//!         # }
//!         # unimplemented!()
//!     }
//! }
//! #
//! # fn main() {}
//! ```

use std;
#[cfg(feature = "parsing")]
use std::cell::Cell;
#[cfg(feature = "extra-traits")]
use std::cmp;
#[cfg(feature = "extra-traits")]
use std::fmt::{self, Debug};
#[cfg(feature = "extra-traits")]
use std::hash::{Hash, Hasher};
#[cfg(feature = "parsing")]
use std::rc::Rc;

#[cfg(feature = "parsing")]
use proc_macro2::Delimiter;
#[cfg(feature = "printing")]
use proc_macro2::TokenStream;
use proc_macro2::{Ident, Span};
#[cfg(feature = "printing")]
use quote::{ToTokens, TokenStreamExt};

#[cfg(feature = "parsing")]
use error::Result;
#[cfg(any(feature = "full", feature = "derive"))]
#[cfg(feature = "parsing")]
use lifetime::Lifetime;
#[cfg(any(feature = "full", feature = "derive"))]
#[cfg(feature = "parsing")]
use lit::{Lit, LitBool, LitByte, LitByteStr, LitChar, LitFloat, LitInt, LitStr};
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
    ($name:ident $display:expr) => {
        #[cfg(feature = "parsing")]
        impl Token for $name {
            fn peek(lookahead: &Lookahead1) -> bool {
                // TODO factor out in a way that can be compiled just once
                let scope = Span::call_site();
                let cursor = lookahead.cursor();
                let unexpected = Rc::new(Cell::new(None));
                ::private::new_parse_buffer(scope, cursor, unexpected)
                    .parse::<Self>()
                    .is_ok()
            }

            fn display() -> String {
                $display.to_owned()
            }
        }

        #[cfg(feature = "parsing")]
        impl private::Sealed for $name {}
    };
}

impl_token!(Ident "identifier");
#[cfg(any(feature = "full", feature = "derive"))]
impl_token!(Lifetime "lifetime");
#[cfg(any(feature = "full", feature = "derive"))]
impl_token!(Lit "literal");
#[cfg(any(feature = "full", feature = "derive"))]
impl_token!(LitStr "string literal");
#[cfg(any(feature = "full", feature = "derive"))]
impl_token!(LitByteStr "byte string literal");
#[cfg(any(feature = "full", feature = "derive"))]
impl_token!(LitByte "byte literal");
#[cfg(any(feature = "full", feature = "derive"))]
impl_token!(LitChar "character literal");
#[cfg(any(feature = "full", feature = "derive"))]
impl_token!(LitInt "integer literal");
#[cfg(any(feature = "full", feature = "derive"))]
impl_token!(LitFloat "floating point literal");
#[cfg(any(feature = "full", feature = "derive"))]
impl_token!(LitBool "boolean literal");

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

            impl_token!($name concat!("`", $token, "`"));

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

            impl_token!($name concat!("`", $token, "`"));

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
            }

            #[cfg(feature = "parsing")]
            impl private::Sealed for $name {}
        )*
    };
}

define_punctuation_structs! {
    "_" pub struct Underscore/1 /// `_`
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
        input.step(|cursor| {
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

#[cfg(feature = "parsing")]
impl Token for Paren {
    fn peek(lookahead: &Lookahead1) -> bool {
        lookahead::is_delimiter(lookahead, Delimiter::Parenthesis)
    }

    fn display() -> String {
        "parentheses".to_owned()
    }
}

#[cfg(feature = "parsing")]
impl Token for Brace {
    fn peek(lookahead: &Lookahead1) -> bool {
        lookahead::is_delimiter(lookahead, Delimiter::Brace)
    }

    fn display() -> String {
        "curly braces".to_owned()
    }
}

#[cfg(feature = "parsing")]
impl Token for Bracket {
    fn peek(lookahead: &Lookahead1) -> bool {
        lookahead::is_delimiter(lookahead, Delimiter::Bracket)
    }

    fn display() -> String {
        "square brackets".to_owned()
    }
}

#[cfg(feature = "parsing")]
impl Token for Group {
    fn peek(lookahead: &Lookahead1) -> bool {
        lookahead::is_delimiter(lookahead, Delimiter::None)
    }

    fn display() -> String {
        "invisible group".to_owned()
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
    use proc_macro2::{Spacing, Span};

    use error::{Error, Result};
    use parse::ParseStream;
    use span::FromSpans;

    pub fn keyword(input: ParseStream, token: &str) -> Result<Span> {
        input.step(|cursor| {
            if let Some((ident, rest)) = cursor.ident() {
                if ident == token {
                    return Ok((ident.span(), rest));
                }
            }
            Err(cursor.error(format!("expected `{}`", token)))
        })
    }

    pub fn punct<S: FromSpans>(input: ParseStream, token: &str) -> Result<S> {
        input.step(|cursor| {
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
