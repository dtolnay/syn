// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Parsing interface for parsing a token stream into a syntax tree node.
//!
//! Parsing in Syn is built on parser functions that take in a [`ParseStream`]
//! and produce a [`Result<T>`] where `T` is some syntax tree node. Underlying
//! these parser functions is a lower level mechanism built around the
//! [`Cursor`] type. `Cursor` is a cheaply copyable cursor over a range of
//! tokens in a token stream.
//!
//! [`ParseStream`]: type.ParseStream.html
//! [`Result<T>`]: type.Result.html
//! [`Cursor`]: ../buffer/index.html
//!
//! The `ParseStream`-based interface is convenient for parser implementations,
//! but not necessarily when you just have some tokens that you want to parse.
//! For that we expose the following two entry points.
//!
//! ## The `syn::parse*` functions
//!
//! The [`syn::parse`], [`syn::parse2`], and [`syn::parse_str`] functions serve
//! as an entry point for parsing syntax tree nodes that can be parsed in an
//! obvious default way. These functions can return any syntax tree node that
//! implements the [`Parse`] trait, which includes most types in Syn.
//!
//! [`syn::parse`]: ../fn.parse.html
//! [`syn::parse2`]: ../fn.parse2.html
//! [`syn::parse_str`]: ../fn.parse_str.html
//! [`Parse`]: trait.Parse.html
//!
//! ```
//! use syn::Type;
//!
//! # fn run_parser() -> Result<(), syn::parse::Error> {
//! let t: Type = syn::parse_str("std::collections::HashMap<String, Value>")?;
//! #     Ok(())
//! # }
//! #
//! # fn main() {
//! #     run_parser().unwrap();
//! # }
//! ```
//!
//! The [`parse_quote!`] macro also uses this approach.
//!
//! [`parse_quote!`]: ../macro.parse_quote.html
//!
//! ## The `Parser` trait
//!
//! Some types can be parsed in several ways depending on context. For example
//! an [`Attribute`] can be either "outer" like `#[...]` or "inner" like
//! `#![...]` and parsing the wrong one would be a bug. Similarly [`Punctuated`]
//! may or may not allow trailing punctuation, and parsing it the wrong way
//! would either reject valid input or accept invalid input.
//!
//! [`Attribute`]: ../struct.Attribute.html
//! [`Punctuated`]: ../punctuated/index.html
//!
//! The `Parse` trait is not implemented in these cases because there is no good
//! behavior to consider the default.
//!
//! ```ignore
//! // Can't parse `Punctuated` without knowing whether trailing punctuation
//! // should be allowed in this context.
//! let path: Punctuated<PathSegment, Token![::]> = syn::parse(tokens)?;
//! ```
//!
//! In these cases the types provide a choice of parser functions rather than a
//! single `Parse` implementation, and those parser functions can be invoked
//! through the [`Parser`] trait.
//!
//! [`Parser`]: trait.Parser.html
//!
//! ```
//! # extern crate syn;
//! #
//! # extern crate proc_macro2;
//! # use proc_macro2::TokenStream;
//! #
//! use syn::parse::Parser;
//! use syn::punctuated::Punctuated;
//! use syn::{Attribute, Expr, PathSegment, Token};
//!
//! # fn run_parsers() -> Result<(), syn::parse::Error> {
//! #     let tokens = TokenStream::new().into();
//! // Parse a nonempty sequence of path segments separated by `::` punctuation
//! // with no trailing punctuation.
//! let parser = Punctuated::<PathSegment, Token![::]>::parse_separated_nonempty;
//! let path = parser.parse(tokens)?;
//!
//! #     let tokens = TokenStream::new().into();
//! // Parse a possibly empty sequence of expressions terminated by commas with
//! // an optional trailing punctuation.
//! let parser = Punctuated::<Expr, Token![,]>::parse_terminated;
//! let args = parser.parse(tokens)?;
//!
//! #     let tokens = TokenStream::new().into();
//! // Parse zero or more outer attributes but not inner attributes.
//! let parser = Attribute::parse_outer;
//! let attrs = parser.parse(tokens)?;
//! #
//! #     Ok(())
//! # }
//! #
//! # fn main() {}
//! ```
//!
//! ---
//!
//! *This module is available if Syn is built with the `"parsing"` feature.*

use std::cell::Cell;
use std::fmt::Display;
use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;
use std::rc::Rc;
use std::str::FromStr;

#[cfg(all(
    not(all(target_arch = "wasm32", target_os = "unknown")),
    feature = "proc-macro"
))]
use proc_macro;
use proc_macro2::{self, Delimiter, Group, Literal, Punct, Span, TokenStream, TokenTree};

use buffer::{Cursor, TokenBuffer};
use error;
use lookahead;
use private;
use punctuated::Punctuated;
use token::Token;

pub use error::{Error, Result};
pub use lookahead::{Lookahead1, Peek};

/// Parsing interface implemented by all types that can be parsed in a default
/// way from a token stream.
pub trait Parse: Sized {
    fn parse(input: ParseStream) -> Result<Self>;
}

/// Input to a Syn parser function.
///
/// See the methods of this type under the documentation of [`ParseBuffer`]. For
/// an overview of parsing in Syn, refer to the [module documentation].
///
/// [module documentation]: index.html
pub type ParseStream<'a> = &'a ParseBuffer<'a>;

/// Cursor position within a buffered token stream.
pub struct ParseBuffer<'a> {
    scope: Span,
    cell: Cell<Cursor<'static>>,
    marker: PhantomData<Cursor<'a>>,
    unexpected: Rc<Cell<Option<Span>>>,
}

impl<'a> Drop for ParseBuffer<'a> {
    fn drop(&mut self) {
        if !self.is_empty() && self.unexpected.get().is_none() {
            self.unexpected.set(Some(self.cursor().span()));
        }
    }
}

/// Cursor state associated with speculative parsing.
///
/// This type is the input of the closure provided to [`ParseStream::step`].
///
/// [`ParseStream::step`]: struct.ParseBuffer.html#method.step
///
/// # Example
///
/// ```
/// # extern crate proc_macro2;
/// # extern crate syn;
/// #
/// use proc_macro2::TokenTree;
/// use syn::parse::{ParseStream, Result};
///
/// // This function advances the stream past the next occurrence of `@`. If
/// // no `@` is present in the stream, the stream position is unchanged and
/// // an error is returned.
/// fn skip_past_next_at(input: ParseStream) -> Result<()> {
///     input.step(|cursor| {
///         let mut rest = *cursor;
///         while let Some((tt, next)) = cursor.token_tree() {
///             match tt {
///                 TokenTree::Punct(ref punct) if punct.as_char() == '@' => {
///                     return Ok(((), next));
///                 }
///                 _ => rest = next,
///             }
///         }
///         Err(cursor.error("no `@` was found after this point"))
///     })
/// }
/// #
/// # fn main() {}
/// ```
#[derive(Copy, Clone)]
pub struct StepCursor<'c, 'a> {
    scope: Span,
    cursor: Cursor<'c>,
    marker: PhantomData<fn(Cursor<'c>) -> Cursor<'a>>,
}

impl<'c, 'a> Deref for StepCursor<'c, 'a> {
    type Target = Cursor<'c>;

    fn deref(&self) -> &Self::Target {
        &self.cursor
    }
}

impl<'c, 'a> StepCursor<'c, 'a> {
    /// Triggers an error at the current position of the parse stream.
    ///
    /// The `ParseStream::step` invocation will return this same error without
    /// advancing the stream state.
    pub fn error<T: Display>(self, message: T) -> Error {
        error::new_at(self.scope, self.cursor, message)
    }
}

impl private {
    pub fn advance_step_cursor<'c, 'a>(proof: StepCursor<'c, 'a>, to: Cursor<'c>) -> Cursor<'a> {
        let _ = proof;
        unsafe { mem::transmute::<Cursor<'c>, Cursor<'a>>(to) }
    }
}

fn skip(input: ParseStream) -> bool {
    input
        .step(|cursor| {
            if let Some((_lifetime, rest)) = cursor.lifetime() {
                Ok((true, rest))
            } else if let Some((_token, rest)) = cursor.token_tree() {
                Ok((true, rest))
            } else {
                Ok((false, *cursor))
            }
        }).unwrap()
}

impl private {
    pub fn new_parse_buffer(
        scope: Span,
        cursor: Cursor,
        unexpected: Rc<Cell<Option<Span>>>,
    ) -> ParseBuffer {
        let extend = unsafe { mem::transmute::<Cursor, Cursor<'static>>(cursor) };
        ParseBuffer {
            scope: scope,
            cell: Cell::new(extend),
            marker: PhantomData,
            unexpected: unexpected,
        }
    }

    pub fn get_unexpected(buffer: &ParseBuffer) -> Rc<Cell<Option<Span>>> {
        buffer.unexpected.clone()
    }
}

impl<'a> ParseBuffer<'a> {
    /// Parses a syntax tree node of type `T`, advancing the position of our
    /// parse stream past it.
    pub fn parse<T: Parse>(&self) -> Result<T> {
        T::parse(self)
    }

    /// Calls the given parser function to parse a syntax tree node of type `T`
    /// from this stream.
    pub fn call<T>(&self, function: fn(ParseStream) -> Result<T>) -> Result<T> {
        function(self)
    }

    /// Looks at the next token in the parse stream to determine whether it
    /// matches the requested type of token.
    ///
    /// Does not advance the position of the parse stream.
    pub fn peek<T: Peek>(&self, token: T) -> bool {
        let _ = token;
        T::Token::peek(self.cursor())
    }

    /// Looks at the second-next token in the parse stream.
    pub fn peek2<T: Peek>(&self, token: T) -> bool {
        let ahead = self.fork();
        skip(&ahead) && ahead.peek(token)
    }

    /// Looks at the third-next token in the parse stream.
    pub fn peek3<T: Peek>(&self, token: T) -> bool {
        let ahead = self.fork();
        skip(&ahead) && skip(&ahead) && ahead.peek(token)
    }

    /// Parses zero or more occurrences of `T` separated by punctuation of type
    /// `P`, with optional trailing punctuation.
    ///
    /// Parsing continues until the end of this parse stream. The entire content
    /// of this parse stream must consist of `T` and `P`.
    pub fn parse_terminated<T, P: Parse>(
        &self,
        parser: fn(ParseStream) -> Result<T>,
    ) -> Result<Punctuated<T, P>> {
        Punctuated::parse_terminated_with(self, parser)
    }

    /// Returns whether there are tokens remaining in this stream.
    ///
    /// This method returns true at the end of the content of a set of
    /// delimiters, as well as at the very end of the complete macro input.
    pub fn is_empty(&self) -> bool {
        self.cursor().eof()
    }

    /// Constructs a helper for peeking at the next token in this stream and
    /// building an error message if it is not one of a set of expected tokens.
    pub fn lookahead1(&self) -> Lookahead1<'a> {
        lookahead::new(self.scope, self.cursor())
    }

    /// Forks a parse stream so that parsing tokens out of either the original
    /// or the fork does not advance the position of the other.
    ///
    /// # Performance
    ///
    /// Forking a parse stream is a cheap fixed amount of work and does not
    /// involve copying token buffers. Where you might hit performance problems
    /// is if your macro ends up parsing a large amount of content more than
    /// once.
    ///
    /// ```
    /// # use syn::Expr;
    /// # use syn::parse::{ParseStream, Result};
    /// #
    /// # fn bad(input: ParseStream) -> Result<Expr> {
    /// // Do not do this.
    /// if input.fork().parse::<Expr>().is_ok() {
    ///     return input.parse::<Expr>();
    /// }
    /// # unimplemented!()
    /// # }
    /// ```
    ///
    /// As a rule, avoid parsing an unbounded amount of tokens out of a forked
    /// parse stream. Only use a fork when the amount of work performed against
    /// the fork is small and bounded.
    ///
    /// For a lower level but generally more performant way to perform
    /// speculative parsing, consider using [`ParseStream::step`] instead.
    ///
    /// [`ParseStream::step`]: #method.step
    pub fn fork(&self) -> Self {
        ParseBuffer {
            scope: self.scope,
            cell: self.cell.clone(),
            marker: PhantomData,
            // Not the parent's unexpected. Nothing cares whether the clone
            // parses all the way.
            unexpected: Rc::new(Cell::new(None)),
        }
    }

    /// Triggers an error at the current position of the parse stream.
    pub fn error<T: Display>(&self, message: T) -> Error {
        error::new_at(self.scope, self.cursor(), message)
    }

    /// Speculatively parses tokens from this parse stream, advancing the
    /// position of this stream only if parsing succeeds.
    ///
    /// # Example
    ///
    /// ```
    /// # extern crate proc_macro2;
    /// # extern crate syn;
    /// #
    /// use proc_macro2::TokenTree;
    /// use syn::parse::{ParseStream, Result};
    ///
    /// // This function advances the stream past the next occurrence of `@`. If
    /// // no `@` is present in the stream, the stream position is unchanged and
    /// // an error is returned.
    /// fn skip_past_next_at(input: ParseStream) -> Result<()> {
    ///     input.step(|cursor| {
    ///         let mut rest = *cursor;
    ///         while let Some((tt, next)) = cursor.token_tree() {
    ///             match tt {
    ///                 TokenTree::Punct(ref punct) if punct.as_char() == '@' => {
    ///                     return Ok(((), next));
    ///                 }
    ///                 _ => rest = next,
    ///             }
    ///         }
    ///         Err(cursor.error("no `@` was found after this point"))
    ///     })
    /// }
    /// #
    /// # fn main() {}
    /// ```
    pub fn step<F, R>(&self, function: F) -> Result<R>
    where
        F: for<'c> FnOnce(StepCursor<'c, 'a>) -> Result<(R, Cursor<'c>)>,
    {
        let (node, rest) = function(StepCursor {
            scope: self.scope,
            cursor: self.cell.get(),
            marker: PhantomData,
        })?;
        self.cell.set(rest);
        Ok(node)
    }

    /// Provides low-level access to the token representation underlying this
    /// parse stream.
    ///
    /// Cursors are immutable so no operations you perform against the cursor
    /// will affect the state of this parse stream.
    pub fn cursor(&self) -> Cursor<'a> {
        self.cell.get()
    }

    fn check_unexpected(&self) -> Result<()> {
        match self.unexpected.get() {
            Some(span) => Err(Error::new(span, "unexpected token")),
            None => Ok(()),
        }
    }
}

impl<T: Parse> Parse for Box<T> {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse().map(Box::new)
    }
}

impl<T: Parse + Token> Parse for Option<T> {
    fn parse(input: ParseStream) -> Result<Self> {
        if T::peek(input.cursor()) {
            Ok(Some(input.parse()?))
        } else {
            Ok(None)
        }
    }
}

impl Parse for TokenStream {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| Ok((cursor.token_stream(), Cursor::empty())))
    }
}

impl Parse for TokenTree {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| match cursor.token_tree() {
            Some((tt, rest)) => Ok((tt, rest)),
            None => Err(cursor.error("expected token tree")),
        })
    }
}

impl Parse for Group {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| {
            for delim in &[Delimiter::Parenthesis, Delimiter::Brace, Delimiter::Bracket] {
                if let Some((inside, span, rest)) = cursor.group(*delim) {
                    let mut group = Group::new(*delim, inside.token_stream());
                    group.set_span(span);
                    return Ok((group, rest));
                }
            }
            Err(cursor.error("expected group token"))
        })
    }
}

impl Parse for Punct {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| match cursor.punct() {
            Some((punct, rest)) => Ok((punct, rest)),
            None => Err(cursor.error("expected punctuation token")),
        })
    }
}

impl Parse for Literal {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| match cursor.literal() {
            Some((literal, rest)) => Ok((literal, rest)),
            None => Err(cursor.error("expected literal token")),
        })
    }
}

/// Parser that can parse Rust tokens into a particular syntax tree node.
///
/// Refer to the [module documentation] for details about parsing in Syn.
///
/// [module documentation]: index.html
///
/// *This trait is available if Syn is built with the `"parsing"` feature.*
pub trait Parser: Sized {
    type Output;

    /// Parse a proc-macro2 token stream into the chosen syntax tree node.
    fn parse2(self, tokens: TokenStream) -> Result<Self::Output>;

    /// Parse tokens of source code into the chosen syntax tree node.
    ///
    /// *This method is available if Syn is built with both the `"parsing"` and
    /// `"proc-macro"` features.*
    #[cfg(all(
        not(all(target_arch = "wasm32", target_os = "unknown")),
        feature = "proc-macro"
    ))]
    fn parse(self, tokens: proc_macro::TokenStream) -> Result<Self::Output> {
        self.parse2(proc_macro2::TokenStream::from(tokens))
    }

    /// Parse a string of Rust code into the chosen syntax tree node.
    ///
    /// # Hygiene
    ///
    /// Every span in the resulting syntax tree will be set to resolve at the
    /// macro call site.
    fn parse_str(self, s: &str) -> Result<Self::Output> {
        self.parse2(proc_macro2::TokenStream::from_str(s)?)
    }
}

fn tokens_to_parse_buffer(tokens: &TokenBuffer) -> ParseBuffer {
    let scope = Span::call_site();
    let cursor = tokens.begin();
    let unexpected = Rc::new(Cell::new(None));
    private::new_parse_buffer(scope, cursor, unexpected)
}

impl<F, T> Parser for F
where
    F: FnOnce(ParseStream) -> Result<T>,
{
    type Output = T;

    fn parse2(self, tokens: TokenStream) -> Result<T> {
        let buf = TokenBuffer::new2(tokens);
        let state = tokens_to_parse_buffer(&buf);
        let node = self(&state)?;
        state.check_unexpected()?;
        if state.is_empty() {
            Ok(node)
        } else {
            Err(state.error("unexpected token"))
        }
    }
}
