// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std;
use std::fmt::{self, Display};
use std::iter::FromIterator;

use proc_macro2::{
    Delimiter, Group, Ident, LexError, Literal, Punct, Spacing, Span, TokenStream, TokenTree,
};

use buffer::Cursor;
use thread::ThreadBound;

/// The result of a Syn parser.
pub type Result<T> = std::result::Result<T, Error>;

/// Error returned when a Syn parser cannot parse the input tokens.
///
/// Refer to the [module documentation] for details about parsing in Syn.
///
/// [module documentation]: index.html
///
/// *This type is available if Syn is built with the `"parsing"` feature.*
#[derive(Debug)]
pub struct Error {
    // Span is implemented as an index into a thread-local interner to keep the
    // size small. It is not safe to access from a different thread. We want
    // errors to be Send and Sync to play nicely with the Failure crate, so pin
    // the span we're given to its original thread and assume it is
    // Span::call_site if accessed from any other thread.
    span: ThreadBound<Span>,
    message: String,
}

#[cfg(test)]
struct _Test where Error: Send + Sync;

impl Error {
    /// Usually the [`ParseStream::error`] method will be used instead, which
    /// automatically uses the correct span from the current position of the
    /// parse stream.
    ///
    /// Use `Error::new` when the error needs to be triggered on some span other
    /// than where the parse stream is currently positioned.
    ///
    /// [`ParseStream::error`]: struct.ParseBuffer.html#method.error
    ///
    /// # Example
    ///
    /// ```
    /// #[macro_use]
    /// extern crate syn;
    ///
    /// use syn::{Ident, LitStr};
    /// use syn::parse::{Error, ParseStream, Result};
    ///
    /// // Parses input that looks like `name = "string"` where the key must be
    /// // the identifier `name` and the value may be any string literal.
    /// // Returns the string literal.
    /// fn parse_name(input: ParseStream) -> Result<LitStr> {
    ///     let name_token: Ident = input.parse()?;
    ///     if name_token != "name" {
    ///         // Trigger an error not on the current position of the stream,
    ///         // but on the position of the unexpected identifier.
    ///         return Err(Error::new(name_token.span(), "expected `name`"));
    ///     }
    ///     input.parse::<Token![=]>()?;
    ///     let s: LitStr = input.parse()?;
    ///     Ok(s)
    /// }
    /// #
    /// # fn main() {}
    /// ```
    pub fn new<T: Display>(span: Span, message: T) -> Self {
        Error {
            span: ThreadBound::new(span),
            message: message.to_string(),
        }
    }

    pub fn span(&self) -> Span {
        match self.span.get() {
            Some(span) => *span,
            None => Span::call_site(),
        }
    }

    /// Render the error as an invocation of [`compile_error!`].
    ///
    /// The [`parse_macro_input!`] macro provides a convenient way to invoke
    /// this method correctly in a procedural macro.
    ///
    /// [`compile_error!`]: https://doc.rust-lang.org/std/macro.compile_error.html
    /// [`parse_macro_input!`]: ../macro.parse_macro_input.html
    pub fn to_compile_error(&self) -> TokenStream {
        let span = self.span();

        // compile_error!($message)
        TokenStream::from_iter(vec![
            TokenTree::Ident(Ident::new("compile_error", span)),
            TokenTree::Punct({
                let mut punct = Punct::new('!', Spacing::Alone);
                punct.set_span(span);
                punct
            }),
            TokenTree::Group({
                let mut group = Group::new(Delimiter::Brace, {
                    TokenStream::from_iter(vec![TokenTree::Literal({
                        let mut string = Literal::string(&self.message);
                        string.set_span(span);
                        string
                    })])
                });
                group.set_span(span);
                group
            }),
        ])
    }
}

pub fn new_at<T: Display>(scope: Span, cursor: Cursor, message: T) -> Error {
    if cursor.eof() {
        Error::new(scope, format!("unexpected end of input, {}", message))
    } else {
        Error::new(cursor.span(), message)
    }
}

impl Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(&self.message)
    }
}

impl Clone for Error {
    fn clone(&self) -> Self {
        Error {
            span: ThreadBound::new(self.span()),
            message: self.message.clone(),
        }
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        "parse error"
    }
}

impl From<LexError> for Error {
    fn from(err: LexError) -> Self {
        Error::new(Span::call_site(), format!("{:?}", err))
    }
}
