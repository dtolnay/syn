use proc_macro2::Delimiter;

use parse::{ParseBuffer, Result};
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
/// # extern crate syn_error_experiment;
/// #
/// use syn_error_experiment::{braced, token, Ident, Token};
/// use syn_error_experiment::parse::{Parse, ParseStream, Result};
/// # use syn_error_experiment::Field;
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
