// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Parsing interface for parsing a token stream into a syntax tree node.
//!
//! All syntax tree nodes that can be parsed from a token stream need to
//! implement the [`Synom`] trait. This trait is usually implemented using the
//! [`nom`]-style parser combinator macros provided by Syn, but may also be
//! implemented without macros be using the low-level [`Cursor`] API directly.
//!
//! [`Synom`]: trait.Synom.html
//! [`nom`]: https://github.com/Geal/nom
//! [`Cursor`]: ../buffer/index.html
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

use proc_macro2::TokenStream;

pub use error::{PResult, ParseError};

use buffer::Cursor;

/// Parsing interface implemented by all types that can be parsed from a token
/// stream.
///
/// Refer to the [module documentation] for details about parsing in Syn.
///
/// [module documentation]: index.html
///
/// *This trait is available if Syn is built with the `"parsing"` feature.*
pub trait Synom: Sized {
    fn parse(input: Cursor) -> PResult<Self>;

    fn description() -> Option<&'static str> {
        None
    }
}

impl Synom for TokenStream {
    fn parse(input: Cursor) -> PResult<Self> {
        Ok((input.token_stream(), Cursor::empty()))
    }

    fn description() -> Option<&'static str> {
        Some("arbitrary token stream")
    }
}
