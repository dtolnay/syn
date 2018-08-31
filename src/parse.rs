// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Parsing interface for parsing a token stream into a syntax tree node.
//!
//! Parsing in Syn is built on parser functions that take in a [`Cursor`] and
//! produce a [`PResult<T>`] where `T` is some syntax tree node. `Cursor` is a
//! cheaply copyable cursor over a range of tokens in a token stream, and
//! `PResult` is a result that packages together a parsed syntax tree node `T`
//! with a stream of remaining unparsed tokens after `T` represented as another
//! `Cursor`, or an [`Error`] if parsing failed.
//!
//! [`Cursor`]: ../buffer/index.html
//! [`PResult<T>`]: type.PResult.html
//! [`Error`]: struct.Error.html
//!
//! This `Cursor`- and `PResult`-based interface is convenient for parser
//! combinators and parser implementations, but not necessarily when you just
//! have some tokens that you want to parse. For that we expose the following
//! two entry points.
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
//! The `Synom` trait is not implemented in these cases because there is no good
//! behavior to consider the default.
//!
//! ```ignore
//! // Can't parse `Punctuated` without knowing whether trailing punctuation
//! // should be allowed in this context.
//! let path: Punctuated<PathSegment, Token![::]> = syn::parse(tokens)?;
//! ```
//!
//! In these cases the types provide a choice of parser functions rather than a
//! single `Synom` implementation, and those parser functions can be invoked
//! through the [`Parser`] trait.
//!
//! [`Parser`]: trait.Parser.html
//!
//! ```
//! # #[macro_use]
//! # extern crate syn;
//! #
//! # extern crate proc_macro2;
//! # use proc_macro2::TokenStream;
//! #
//! use syn::parse::Parser;
//! use syn::punctuated::Punctuated;
//! use syn::{PathSegment, Expr, Attribute};
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
//! # Implementing a parser function
//!
//! Parser functions are usually implemented using the [`nom`]-style parser
//! combinator macros provided by Syn, but may also be implemented without
//! macros be using the low-level [`Cursor`] API directly.
//!
//! [`nom`]: https://github.com/Geal/nom
//!
//! The following parser combinator macros are available and a `Synom` parsing
//! example is provided for each one.
//!
//! - [`alt!`](../macro.alt.html)
//! - [`braces!`](../macro.braces.html)
//! - [`brackets!`](../macro.brackets.html)
//! - [`call!`](../macro.call.html)
//! - [`cond!`](../macro.cond.html)
//! - [`cond_reduce!`](../macro.cond_reduce.html)
//! - [`custom_keyword!`](../macro.custom_keyword.html)
//! - [`do_parse!`](../macro.do_parse.html)
//! - [`epsilon!`](../macro.epsilon.html)
//! - [`input_end!`](../macro.input_end.html)
//! - [`keyword!`](../macro.keyword.html)
//! - [`many0!`](../macro.many0.html)
//! - [`map!`](../macro.map.html)
//! - [`not!`](../macro.not.html)
//! - [`option!`](../macro.option.html)
//! - [`parens!`](../macro.parens.html)
//! - [`punct!`](../macro.punct.html)
//! - [`reject!`](../macro.reject.html)
//! - [`switch!`](../macro.switch.html)
//! - [`syn!`](../macro.syn.html)
//! - [`tuple!`](../macro.tuple.html)
//! - [`value!`](../macro.value.html)
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
use proc_macro2::{self, Delimiter, Group, Ident, Literal, Punct, Span, TokenStream, TokenTree};

use buffer::{Cursor, TokenBuffer};
use error;
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

// Not public API.
#[doc(hidden)]
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
    // Not public API.
    #[doc(hidden)]
    pub fn advance(self, other: Cursor<'c>) -> Cursor<'a> {
        unsafe { mem::transmute::<Cursor<'c>, Cursor<'a>>(other) }
    }

    pub fn error<T: Display>(self, message: T) -> Error {
        error::new_at(self.scope, self.cursor, message)
    }
}

fn skip(input: ParseStream) -> bool {
    input.step(|cursor| {
        if let Some((_lifetime, rest)) = cursor.lifetime() {
            Ok((true, rest))
        } else if let Some((_token, rest)) = cursor.token_tree() {
            Ok((true, rest))
        } else {
            Ok((false, *cursor))
        }
    }).unwrap()
}

impl<'a> ParseBuffer<'a> {
    // Not public API.
    #[doc(hidden)]
    pub fn new(scope: Span, cursor: Cursor<'a>, unexpected: Rc<Cell<Option<Span>>>) -> Self {
        let extend = unsafe { mem::transmute::<Cursor<'a>, Cursor<'static>>(cursor) };
        ParseBuffer {
            scope: scope,
            cell: Cell::new(extend),
            marker: PhantomData,
            unexpected: unexpected,
        }
    }

    pub fn cursor(&self) -> Cursor<'a> {
        self.cell.get()
    }

    pub fn is_empty(&self) -> bool {
        self.cursor().eof()
    }

    pub fn lookahead1(&self) -> Lookahead1<'a> {
        Lookahead1::new(self.scope, self.cursor())
    }

    pub fn parse<T: Parse>(&self) -> Result<T> {
        self.check_unexpected()?;
        T::parse(self)
    }

    pub fn call<T>(&self, function: fn(ParseStream) -> Result<T>) -> Result<T> {
        function(self)
    }

    pub fn peek<T: Peek>(&self, token: T) -> bool {
        self.lookahead1().peek(token)
    }

    pub fn peek2<T: Peek>(&self, token: T) -> bool {
        let ahead = self.fork();
        skip(&ahead) && ahead.peek(token)
    }

    pub fn peek3<T: Peek>(&self, token: T) -> bool {
        let ahead = self.fork();
        skip(&ahead) && skip(&ahead) && ahead.peek(token)
    }

    pub fn parse_terminated<T, P: Parse>(
        &self,
        parser: fn(ParseStream) -> Result<T>,
    ) -> Result<Punctuated<T, P>> {
        Punctuated::parse_terminated_with(self, parser)
    }

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

    pub fn error<T: Display>(&self, message: T) -> Error {
        error::new_at(self.scope, self.cursor(), message)
    }

    pub fn step<F, R>(&self, function: F) -> Result<R>
    where
        F: for<'c> FnOnce(StepCursor<'c, 'a>) -> Result<(R, Cursor<'c>)>,
    {
        self.check_unexpected()?;
        match function(StepCursor {
            scope: self.scope,
            cursor: self.cell.get(),
            marker: PhantomData,
        }) {
            Ok((ret, cursor)) => {
                self.cell.set(cursor);
                Ok(ret)
            }
            Err(err) => Err(err),
        }
    }

    // Not public API.
    #[doc(hidden)]
    pub fn get_unexpected(&self) -> Rc<Cell<Option<Span>>> {
        self.unexpected.clone()
    }

    // Not public API.
    #[doc(hidden)]
    pub fn check_unexpected(&self) -> Result<()> {
        match self.unexpected.get() {
            Some(span) => Err(Error::new(span, "unexpected token")),
            None => Ok(()),
        }
    }
}

impl Parse for Ident {
    fn parse(input: ParseStream) -> Result<Self> {
        input.step(|cursor| {
            if let Some((ident, rest)) = cursor.ident() {
                match ident.to_string().as_str() {
                    "_"
                    // Based on https://doc.rust-lang.org/grammar.html#keywords
                    // and https://github.com/rust-lang/rfcs/blob/master/text/2421-unreservations-2018.md
                    | "abstract" | "as" | "become" | "box" | "break" | "const"
                    | "continue" | "crate" | "do" | "else" | "enum" | "extern" | "false" | "final"
                    | "fn" | "for" | "if" | "impl" | "in" | "let" | "loop" | "macro" | "match"
                    | "mod" | "move" | "mut" | "override" | "priv" | "proc" | "pub"
                    | "ref" | "return" | "Self" | "self" | "static" | "struct"
                    | "super" | "trait" | "true" | "type" | "typeof" | "unsafe" | "unsized" | "use"
                    | "virtual" | "where" | "while" | "yield" => {}
                    _ => return Ok((ident, rest)),
                }
            }
            Err(cursor.error("expected identifier"))
        })
    }
}

impl<T: Parse> Parse for Box<T> {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse().map(Box::new)
    }
}

impl<T: Parse + Token> Parse for Option<T> {
    fn parse(input: ParseStream) -> Result<Self> {
        if T::peek(&input.lookahead1()) {
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

impl<F, T> Parser for F
where
    F: FnOnce(ParseStream) -> Result<T>,
{
    type Output = T;

    fn parse2(self, tokens: TokenStream) -> Result<T> {
        let buf = TokenBuffer::new2(tokens);
        let unexpected = Rc::new(Cell::new(None));
        let state = ParseBuffer::new(Span::call_site(), buf.begin(), unexpected);
        let node = self(&state)?;
        state.check_unexpected()?;
        if state.is_empty() {
            Ok(node)
        } else {
            Err(state.error("unexpected token"))
        }
    }
}
