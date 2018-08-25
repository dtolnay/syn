use proc_macro2::{Delimiter, Span};

use error::Result;
use parse::ParseBuffer;
use token;

pub struct Parens<'a> {
    pub token: token::Paren,
    pub content: ParseBuffer<'a>,
}

pub struct Braces<'a> {
    pub token: token::Brace,
    pub content: ParseBuffer<'a>,
}

pub struct Brackets<'a> {
    pub token: token::Bracket,
    pub content: ParseBuffer<'a>,
}

impl<'a> ParseBuffer<'a> {
    fn parse_group(&self, delimiter: Delimiter) -> Result<(Span, ParseBuffer<'a>)> {
        self.step_cursor(|cursor| {
            if let Some((content, span, rest)) = cursor.group(delimiter) {
                let content = ParseBuffer::new(span, cursor.advance(content));
                Ok(((span, content), rest))
            } else {
                let message = match delimiter {
                    Delimiter::Parenthesis => "expected parentheses",
                    Delimiter::Brace => "expected curly braces",
                    Delimiter::Bracket => "expected square brackets",
                    Delimiter::None => unreachable!(),
                };
                Err(cursor.error(message))
            }
        })
    }

    // Not public API.
    #[doc(hidden)]
    pub fn parse_parens(&self) -> Result<Parens<'a>> {
        self.parse_group(Delimiter::Parenthesis).map(|(span, content)| Parens {
            token: token::Paren(span),
            content: content,
        })
    }

    // Not public API.
    #[doc(hidden)]
    pub fn parse_braces(&self) -> Result<Braces<'a>> {
        self.parse_group(Delimiter::Brace).map(|(span, content)| Braces {
            token: token::Brace(span),
            content: content,
        })
    }

    // Not public API.
    #[doc(hidden)]
    pub fn parse_brackets(&self) -> Result<Brackets<'a>> {
        self.parse_group(Delimiter::Bracket).map(|(span, content)| Brackets {
            token: token::Bracket(span),
            content: content,
        })
    }
}

/// Parse a set of parentheses and expose their content to subsequent parsers.
#[macro_export]
macro_rules! parenthesized {
    ($content:ident in $cursor:expr) => {
        match $crate::parse::ParseBuffer::parse_parens(&$cursor) {
            $crate::export::Ok(parens) => {
                $content = parens.content;
                parens.token
            }
            $crate::export::Err(error) => {
                return $crate::export::Err(error);
            }
        }
    };
}

/// Parse a set of curly braces and expose their content to subsequent parsers.
///
/// ```rust
/// # extern crate syn;
/// #
/// use syn::{braced, token, Ident, Token};
/// use syn::parse::{Parse, ParseStream, Result};
/// #
/// # mod example {
/// #     use super::{syn, braced, token, Ident, Parse, ParseStream, Result};
/// #
/// #     macro_rules! Token {
/// #         (struct) => {
/// #             syn::token::Struct
/// #         };
/// #     }
/// #
/// #     type Field = Ident;
///
/// // Parse a simplified struct syntax like:
/// //
/// //     struct S {
/// //         a: A,
/// //         b: B,
/// //     }
/// struct Struct {
///     pub struct_token: Token![struct],
///     pub ident: Ident,
///     pub brace_token: token::Brace,
///     pub fields: Vec<Field>,
/// }
///
/// impl Parse for Struct {
///     fn parse(input: ParseStream) -> Result<Self> {
///         let content;
///         Ok(Struct {
///             struct_token: input.parse()?,
///             ident: input.parse()?,
///             brace_token: braced!(content in input),
///             fields: content.parse()?,
///         })
///     }
/// }
/// # }
/// #
/// # fn main() {}
/// ```
#[macro_export]
macro_rules! braced {
    ($content:ident in $cursor:expr) => {
        match $crate::parse::ParseBuffer::parse_braces(&$cursor) {
            $crate::export::Ok(braces) => {
                $content = braces.content;
                braces.token
            }
            $crate::export::Err(error) => {
                return $crate::export::Err(error);
            }
        }
    };
}

/// Parse a set of square brackets and expose their content to subsequent
/// parsers.
#[macro_export]
macro_rules! bracketed {
    ($content:ident in $cursor:expr) => {
        match $crate::parse::ParseBuffer::parse_brackets(&$cursor) {
            $crate::export::Ok(brackets) => {
                $content = brackets.content;
                brackets.token
            }
            $crate::export::Err(error) => {
                return $crate::export::Err(error);
            }
        }
    };
}
