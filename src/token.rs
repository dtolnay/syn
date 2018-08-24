//! Tokens representing Rust punctuation, keywords, and delimiters.

use std::ops::{Deref, DerefMut};

use proc_macro2::{Spacing, Span};

use error::Error;
use lookahead;
use parse::{Lookahead1, Parse, ParseStream, Result};
use span::{FromSpans, IntoSpans};

/// Marker trait for types that represent single tokens.
///
/// This trait is sealed and cannot be implemented for types outside of Syn.
pub trait Token: private::Sealed {
    // Not public API.
    #[doc(hidden)]
    fn peek(lookahead: &Lookahead1) -> bool;

    // Not public API.
    #[doc(hidden)]
    fn display() -> String;
}

mod private {
    pub trait Sealed {}
}

/// A type-macro that expands to the name of the Rust type representation of a
/// given token.
#[macro_export]
#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! Token {
    (struct) => { $crate::token::Struct };
    (enum)   => { $crate::token::Enum };
    (:)      => { $crate::token::Colon };
    (,)      => { $crate::token::Comma };
    (..)     => { $crate::token::Dot2 };
}

macro_rules! impl_token {
    ($token:tt $name:ident #[$doc:meta]) => {
        impl Token for $name {
            fn peek(lookahead: &Lookahead1) -> bool {
                lookahead::is_token(lookahead, $token)
            }

            fn display() -> String {
                concat!("`", $token, "`").to_owned()
            }
        }

        impl private::Sealed for $name {}
    };
}

macro_rules! define_keywords {
    ($($token:tt pub struct $name:ident #[$doc:meta])*) => {
        $(
            #[$doc]
            #[derive(Debug)]
            pub struct $name {
                pub span: Span,
            }

            #[doc(hidden)]
            #[allow(non_snake_case)]
            pub fn $name<T: IntoSpans<[Span; 1]>>(span: T) -> $name {
                $name {
                    span: span.into_spans()[0],
                }
            }

            impl_token!($token $name #[$doc]);

            impl Parse for $name {
                fn parse(input: ParseStream) -> Result<Self> {
                    parse_keyword(input, $token).map($name)
                }
            }
        )*
    };
}

macro_rules! define_punctuation {
    ($($token:tt pub struct $name:ident/$len:tt #[$doc:meta])*) => {
        $(
            #[$doc]
            #[derive(Debug)]
            pub struct $name {
                pub spans: [Span; $len],
            }

            impl Deref for $name {
                type Target = [Span; $len];

                fn deref(&self) -> &Self::Target {
                    &self.spans
                }
            }

            impl DerefMut for $name {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.spans
                }
            }

            #[doc(hidden)]
            #[allow(non_snake_case)]
            pub fn $name<T: IntoSpans<[Span; $len]>>(spans: T) -> $name {
                $name {
                    spans: spans.into_spans(),
                }
            }

            impl_token!($token $name #[$doc]);

            impl Parse for $name {
                fn parse(input: ParseStream) -> Result<Self> {
                    parse_punctuation(input, $token).map($name::<[Span; $len]>)
                }
            }
        )*
    };
}

define_keywords! {
    "struct" pub struct Struct /// `struct`
    "enum"   pub struct Enum   /// `enum`
}

define_punctuation! {
    ":"  pub struct Colon/1 /// `:`
    ","  pub struct Comma/1 /// `,`
    ".." pub struct Dot2/2  /// `..`
}

/// `{...}`
#[derive(Debug)]
pub struct Brace(pub Span);

fn parse_keyword(input: ParseStream, token: &str) -> Result<Span> {
    input.step_cursor(|cursor| {
        if let Some((ident, rest)) = cursor.ident() {
            if ident == token {
                return Ok((ident.span(), rest));
            }
        }
        Err(cursor.error(format!("expected `{}`", token)))
    })
}

fn parse_punctuation<S: FromSpans>(input: ParseStream, token: &str) -> Result<S> {
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
