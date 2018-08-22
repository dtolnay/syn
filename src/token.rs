//! Tokens representing Rust punctuation, keywords, and delimiters.

use proc_macro2::Span;

use parse::{Lookahead1, Parse, ParseStream, Result};

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
}

macro_rules! define_token {
    ($token:tt $name:ident #[$doc:meta]) => {
        #[$doc]
        #[derive(Debug)]
        pub struct $name(pub Span);

        impl Token for $name {
            fn peek(lookahead: &Lookahead1) -> bool {
                ::lookahead::is_token(lookahead, $token)
            }

            fn display() -> String {
                concat!("`", $token, "`").to_owned()
            }
        }

        impl private::Sealed for $name {}
    };
}

macro_rules! define_keywords {
    ($($token:tt $name:ident #[$doc:meta])*) => {
        $(
            define_token!($token $name #[$doc]);

            impl Parse for $name {
                fn parse(input: ParseStream) -> Result<Self> {
                    parse_keyword(input, $token).map($name)
                }
            }
        )*
    };
}

macro_rules! define_punctuation {
    ($($token:tt $name:ident #[$doc:meta])*) => {
        $(
            define_token!($token $name #[$doc]);

            impl Parse for $name {
                fn parse(input: ParseStream) -> Result<Self> {
                    parse_punctuation(input, $token).map($name)
                }
            }
        )*
    };
}

define_keywords! {
    "struct" Struct /// `struct`
    "enum"   Enum   /// `enum`
}

define_punctuation! {
    ":" Colon /// `:`
    "," Comma /// `,`
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

fn parse_punctuation(input: ParseStream, token: &str) -> Result<Span> {
    input.step_cursor(|cursor| {
        // TODO: support multi-character punctuation
        if let Some((punct, rest)) = cursor.punct() {
            if punct.as_char() == token.chars().next().unwrap() {
                return Ok((punct.span(), rest));
            }
        }
        Err(cursor.error(format!("expected `{}`", token)))
    })
}
