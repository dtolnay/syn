use proc_macro2::Delimiter;

use super::parse::{ParseBuffer, Result};
use token;

pub struct Braces<'a> {
    pub token: token::Brace,
    pub content: ParseBuffer<'a>,
}

impl<'a> ParseBuffer<'a> {
    // Not public API.
    #[doc(hidden)]
    pub fn parse_braces(&self) -> Result<Braces<'a>> {
        self.step_cursor(|cursor| {
            if let Some((content, span, rest)) = cursor.group(Delimiter::Brace) {
                let braces = Braces {
                    token: token::Brace(span),
                    content: ParseBuffer::new(span, cursor.advance(content)),
                };
                Ok((braces, rest))
            } else {
                Err(cursor.error("expected curly braces"))
            }
        })
    }
}

/// Parse a set of curly braces and expose their content to subsequent parsers.
///
/// ```rust
/// # extern crate syn;
/// #
/// use syn::{braced, Token};
/// use syn::next::{token, Ident};
/// use syn::next::parse::{Parse, ParseStream, Result};
/// #
/// # mod example {
/// #     use super::{syn, braced, token, Ident, Parse, ParseStream, Result};
/// #
/// #     macro_rules! Token {
/// #         (struct) => {
/// #             syn::next::token::Struct
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
        match $crate::next::parse::ParseBuffer::parse_braces(&$cursor) {
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
