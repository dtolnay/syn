// Not public API, but #[macro_export] _is_ documented
#![doc(hidden)]

#[cfg(feature = "parsing")]
use proc_macro2::{Span, TokenStream};
#[cfg(feature = "parsing")]
use parse::{ParseStream, Result};
#[cfg(feature = "parsing")]
use token;
#[cfg(feature = "parsing")]
use buffer::Cursor;
#[cfg(feature = "parsing")]
use span::FromSpans;

#[cfg(feature = "parsing")]
pub trait CustomToken {
    fn peek(cursor: Cursor) -> bool;
    fn display() -> &'static str;
}

#[cfg(feature = "parsing")]
impl<T: CustomToken> token::private::Sealed for T {}
#[cfg(feature = "parsing")]
impl<T: CustomToken> token::Token for T {
    fn peek(cursor: Cursor) -> bool {
        <Self as CustomToken>::peek(cursor)
    }

    fn display() -> &'static str {
        <Self as CustomToken>::display()
    }
}

#[cfg(feature = "parsing")]
pub mod punct {
    use super::*;

    pub fn peek(cursor: Cursor, token: &str) -> bool {
        token::parsing::peek_punct(cursor, token)
    }

    pub fn parse<S: FromSpans>(input: ParseStream, token: &str) -> Result<S> {
        token::parsing::punct(input, token)
    }

    #[cfg(feature = "printing")]
    pub fn print(tokens: &mut TokenStream, token: &str, spans: &[Span]) {
        token::printing::punct(token, spans, tokens)
    }
}

// Not public API.
#[doc(hidden)]
#[macro_export]
macro_rules! punct_len {
    ()       => ( 0usize );
    (+)      => ( 1usize );
    (+=)     => ( 2usize );
    (&)      => ( 1usize );
    (&&)     => ( 2usize );
    (&=)     => ( 2usize );
    (@)      => ( 1usize );
    (!)      => ( 1usize );
    (^)      => ( 1usize );
    (^=)     => ( 2usize );
    (:)      => ( 1usize );
    (::)     => ( 2usize );
    (,)      => ( 1usize );
    (/)      => ( 1usize );
    (/=)     => ( 2usize );
    (.)      => ( 1usize );
    (..)     => ( 2usize );
    (...)    => ( 3usize );
    (..=)    => ( 3usize );
    (=)      => ( 1usize );
    (==)     => ( 2usize );
    (>=)     => ( 2usize );
    (>)      => ( 1usize );
    (<=)     => ( 2usize );
    (<)      => ( 1usize );
    (*=)     => ( 2usize );
    (!=)     => ( 2usize );
    (|)      => ( 1usize );
    (|=)     => ( 2usize );
    (||)     => ( 2usize );
    (#)      => ( 1usize );
    (?)      => ( 1usize );
    (->)     => ( 2usize );
    (<-)     => ( 2usize );
    (%)      => ( 1usize );
    (%=)     => ( 2usize );
    (=>)     => ( 2usize );
    (;)      => ( 1usize );
    (<<)     => ( 2usize );
    (<<=)    => ( 3usize );
    (>>)     => ( 2usize );
    (>>=)    => ( 3usize );
    (*)      => ( 1usize );
    (-)      => ( 1usize );
    (-=)     => ( 2usize );
    (~)      => ( 1usize );
    (_)      => ( 1usize );
    ($tt:tt) => ( compile_error!("Punctuation `{}` is not supported", stringify!($tt)) );
    ($head:tt $($tail:tt)+) => ( punct_len!($head) + punct_len!($($tail)+) );
}

// Not public API.
#[doc(hidden)]
#[macro_export]
macro_rules! stringify_punct {
    ($($tt:tt)*) => ( concat!($(stringify!($tt)),*) );
}

///////////////////////////////////////////////////////////////////////////////

/// Define a type that supports parsing and printing a given identifier as if it
/// were a keyword.
///
/// # Usage
///
/// As a convention, it is recommended that this macro be invoked within a
/// module called `kw` or `keyword` and that the resulting parser be invoked
/// with a `kw::` or `keyword::` prefix.
///
/// ```
/// # #[macro_use]
/// # extern crate syn;
/// #
/// mod kw {
///     custom_keyword!(whatever);
/// }
/// #
/// # fn main() {}
/// ```
///
/// The generated syntax tree node supports the following operations just like
/// any built-in keyword token.
///
/// - [Peeking] — `input.peek(kw::whatever)`
///
/// - [Parsing] — `input.parse::<kw::whatever>()?`
///
/// - [Printing] — `quote!( ... #whatever_token ... )`
///
/// - Construction from a [`Span`] — `let whatever_token = kw::whatever(sp)`
///
/// - Field access to its span — `let sp = whatever_token.span`
///
/// [Peeking]: parse/struct.ParseBuffer.html#method.peek
/// [Parsing]: parse/struct.ParseBuffer.html#method.parse
/// [Printing]: https://docs.rs/quote/0.6/quote/trait.ToTokens.html
/// [`Span`]: struct.Span.html
///
/// # Example
///
/// This example parses input that looks like `bool = true` or `str = "value"`.
/// The key must be either the identifier `bool` or the identifier `str`. If
/// `bool`, the value may be either `true` or `false`. If `str`, the value may
/// be any string literal.
///
/// The symbols `bool` and `str` are not reserved keywords in Rust so these are
/// not considered keywords in the `syn::token` module. Like any other
/// identifier that is not a keyword, these can be declared as custom keywords
/// by crates that need to use them as such.
///
/// ```
/// #[macro_use]
/// extern crate syn;
///
/// use syn::{LitBool, LitStr};
/// use syn::parse::{Parse, ParseStream, Result};
///
/// mod kw {
///     custom_keyword!(bool);
///     custom_keyword!(str);
/// }
///
/// enum Argument {
///     Bool {
///         bool_token: kw::bool,
///         eq_token: Token![=],
///         value: LitBool,
///     },
///     Str {
///         str_token: kw::str,
///         eq_token: Token![=],
///         value: LitStr,
///     },
/// }
///
/// impl Parse for Argument {
///     fn parse(input: ParseStream) -> Result<Self> {
///         let lookahead = input.lookahead1();
///         if lookahead.peek(kw::bool) {
///             Ok(Argument::Bool {
///                 bool_token: input.parse::<kw::bool>()?,
///                 eq_token: input.parse()?,
///                 value: input.parse()?,
///             })
///         } else if lookahead.peek(kw::str) {
///             Ok(Argument::Str {
///                 str_token: input.parse::<kw::str>()?,
///                 eq_token: input.parse()?,
///                 value: input.parse()?,
///             })
///         } else {
///             Err(lookahead.error())
///         }
///     }
/// }
/// #
/// # fn main() {}
/// ```
#[macro_export(local_inner_macros)]
macro_rules! custom_keyword {
    ($ident:ident) => {
        pub struct $ident {
            pub span: $crate::export::Span,
        }

        #[doc(hidden)]
        #[allow(non_snake_case)]
        pub fn $ident<__S: $crate::export::IntoSpans<[$crate::export::Span; 1]>>(
            span: __S,
        ) -> $ident {
            $ident {
                span: $crate::export::IntoSpans::into_spans(span)[0],
            }
        }

        impl $crate::export::Default for $ident {
            fn default() -> Self {
                $ident($crate::export::Span::call_site())
            }
        }

        impl_parsing_for_custom_keyword!($ident);
        impl_printing_for_custom_keyword!($ident);
        impl_clone_for_custom_keyword!($ident);
        impl_extra_traits_for_custom_keyword!($ident);
    };
}

// Not public API.
#[cfg(feature = "parsing")]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_parsing_for_custom_keyword {
    ($ident:ident) => {
        impl $crate::custom_token::CustomToken for $ident {
            fn peek(cursor: $crate::buffer::Cursor) -> $crate::export::bool {
                if let Some((ident, _rest)) = cursor.ident() {
                    ident == stringify!($ident)
                } else {
                    false
                }
            }

            fn display() -> &'static $crate::export::str {
                concat!("`", stringify!($ident), "`")
            }
        }

        impl $crate::parse::Parse for $ident {
            fn parse(input: $crate::parse::ParseStream) -> $crate::parse::Result<$ident> {
                input.step(|cursor| {
                    if let $crate::export::Some((ident, rest)) = cursor.ident() {
                        if ident == stringify!($ident) {
                            return $crate::export::Ok(($ident { span: ident.span() }, rest));
                        }
                    }
                    $crate::export::Err(cursor.error(concat!(
                        "expected `",
                        stringify!($ident),
                        "`"
                    )))
                })
            }
        }
    };
}

// Not public API.
#[cfg(not(feature = "parsing"))]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_parsing_for_custom_keyword {
    ($ident:ident) => {};
}

// Not public API.
#[cfg(feature = "printing")]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_printing_for_custom_keyword {
    ($ident:ident) => {
        impl $crate::export::ToTokens for $ident {
            fn to_tokens(&self, tokens: &mut $crate::export::TokenStream2) {
                let ident = $crate::Ident::new(stringify!($ident), self.span);
                $crate::export::TokenStreamExt::append(tokens, ident);
            }
        }
    };
}

// Not public API.
#[cfg(not(feature = "printing"))]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_printing_for_custom_keyword {
    ($ident:ident) => {};
}

// Not public API.
#[cfg(feature = "clone-impls")]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_clone_for_custom_keyword {
    ($ident:ident) => {
        impl $crate::export::Copy for $ident {}

        impl $crate::export::Clone for $ident {
            fn clone(&self) -> Self {
                *self
            }
        }
    };
}

// Not public API.
#[cfg(not(feature = "clone-impls"))]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_clone_for_custom_keyword {
    ($ident:ident) => {};
}

// Not public API.
#[cfg(feature = "extra-traits")]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_extra_traits_for_custom_keyword {
    ($ident:ident) => {
        impl $crate::export::Debug for $ident {
            fn fmt(&self, f: &mut $crate::export::Formatter) -> $crate::export::fmt::Result {
                $crate::export::Formatter::write_str(f, stringify!($ident))
            }
        }

        impl $crate::export::Eq for $ident {}

        impl $crate::export::PartialEq for $ident {
            fn eq(&self, _other: &Self) -> $crate::export::bool {
                true
            }
        }

        impl $crate::export::Hash for $ident {
            fn hash<__H: $crate::export::Hasher>(&self, _state: &mut __H) {}
        }
    };
}

// Not public API.
#[cfg(not(feature = "extra-traits"))]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_extra_traits_for_custom_keyword {
    ($ident:ident) => {};
}

///////////////////////////////////////////////////////////////////////////////

/// Define a type that supports parsing and printing a multi-character symbol
/// as if it were a token.
///
/// # Usage
///
/// As a convention, it is recommended that this macro be invoked within a
/// module called `punct` or `punctuation` and that the resulting parser be invoked
/// with a `punct::` or `punctuation::` prefix.
///
/// ```
/// # #[macro_use]
/// # extern crate syn;
/// #
/// mod punct {
///     custom_punctuation!(LeftRightArrow, <=>);
/// }
/// #
/// # fn main() {}
/// ```
///
/// The generated syntax tree node supports the following operations just like
/// any built-in punctuation token.
///
/// - [Peeking] — `input.peek(punct::LeftRightArrow)`
///
/// - [Parsing] — `input.parse::<punct::LeftRightArrow>()?`
///
/// - [Printing] — `quote!( ... #path_separator_token ... )`
///
/// - Construction from (a) [`Span`]\(s) — `let path_separator_token = punct::LeftRightArrow(sp)`
///
/// - Field access to its spans — `let spans = path_separator_token.spans`
///
/// [Peeking]: parse/struct.ParseBuffer.html#method.peek
/// [Parsing]: parse/struct.ParseBuffer.html#method.parse
/// [Printing]: https://docs.rs/quote/0.6/quote/trait.ToTokens.html
/// [`Span`]: struct.Span.html
///
/// # Example
// TODO: actual example
#[macro_export(local_inner_macros)]
macro_rules! custom_punctuation {
    ($ident:ident, $($tt:tt)*) => {
        pub struct $ident {
            pub spans: [$crate::export::Span; punct_len!($($tt)*)],
        }

        #[doc(hidden)]
        #[allow(non_snake_case)]
        pub fn $ident<__S: $crate::export::IntoSpans<[$crate::export::Span; punct_len!($($tt)*)]>>(
            spans: __S,
        ) -> $ident {
            $ident {
                spans: $crate::export::IntoSpans::into_spans(spans)
            }
        }

        impl $crate::export::Default for $ident {
            fn default() -> Self {
                $ident($crate::export::Span::call_site())
            }
        }

        impl_parsing_for_custom_punctuation!($ident, $($tt)*);
        impl_printing_for_custom_punctuation!($ident, $($tt)*);
        impl_clone_for_custom_punctuation!($ident, $($tt)*);
        impl_extra_traits_for_custom_punctuation!($ident, $($tt)*);
    };
}

// Not public API.
#[cfg(feature = "parsing")]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_parsing_for_custom_punctuation {
    ($ident: ident, $($tt:tt)*) => {
        impl $crate::custom_token::CustomToken for $ident {
            fn peek(cursor: $crate::buffer::Cursor) -> bool {
                $crate::custom_token::punct::peek(cursor, stringify_punct!($($tt)*))
            }

            fn display() -> &'static $crate::export::str {
                concat!("`", stringify_punct!($($tt)*), "`")
            }
        }

        impl $crate::parse::Parse for $ident {
            fn parse(input: $crate::parse::ParseStream) -> $crate::parse::Result<$ident> {
                let spans: [$crate::export::Span; punct_len!($($tt)*)] =
                    $crate::custom_token::punct::parse(input, stringify_punct!($($tt)*))?;
                Ok($ident(spans))
            }
        }
    };
}

// Not public API.
#[cfg(not(feature = "parsing"))]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_parsing_for_custom_punctuation {
    ($ident: ident, $($tt:tt)*) => {};
}

// Not public API.
#[cfg(feature = "printing")]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_printing_for_custom_punctuation {
    ($ident: ident, $($tt:tt)*) => {
        impl $crate::export::ToTokens for $ident {
            fn to_tokens(&self, tokens: &mut $crate::export::TokenStream2) {
                $crate::custom_token::punct::print(tokens, stringify_punct!($($tt)*), &self.spans)
            }
        }
    };
}

// Not public API.
#[cfg(not(feature = "printing"))]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_printing_for_custom_punctuation {
    ($ident: ident, $($tt:tt)*) => {};
}

// Not public API.
#[cfg(feature = "clone-impls")]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_clone_for_custom_punctuation {
    ($ident: ident, $($tt:tt)*) => {
        impl $crate::export::Copy for $ident {}

        impl $crate::export::Clone for $ident {
            fn clone(&self) -> Self {
                *self
            }
        }
    };
}

// Not public API.
#[cfg(not(feature = "clone-impls"))]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_clone_for_custom_punctuation {
    ($ident: ident, $($tt:tt)*) => {};
}

// Not public API.
#[cfg(feature = "extra-traits")]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_extra_traits_for_custom_punctuation {
    ($ident: ident, $($tt:tt)*) => {
        impl $crate::export::Debug for $ident {
            fn fmt(&self, f: &mut $crate::export::Formatter) -> $crate::export::fmt::Result {
                $crate::export::Formatter::write_str(f, stringify_punct!($($tt)*))
            }
        }

        impl $crate::export::Eq for $ident {}

        impl $crate::export::PartialEq for $ident {
            fn eq(&self, _other: &Self) -> $crate::export::bool {
                true
            }
        }

        impl $crate::export::Hash for $ident {
            fn hash<__H: $crate::export::Hasher>(&self, _state: &mut __H) {}
        }
    };
}

// Not public API.
#[cfg(not(feature = "extra-traits"))]
#[doc(hidden)]
#[macro_export]
macro_rules! impl_extra_traits_for_custom_punctuation {
    ($ident: ident, $($tt:tt)*) => {};
}
