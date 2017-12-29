//! Adapted from [`nom`](https://github.com/Geal/nom) by removing the
//! `IPResult::Incomplete` variant which:
//!
//! - we don't need,
//! - is an unintuitive footgun when working with non-streaming use cases, and
//! - more than doubles compilation time.
//!
//! ## Whitespace handling strategy
//!
//! As (sy)nom is a parser combinator library, the parsers provided here and
//! that you implement yourself are all made up of successively more primitive
//! parsers, eventually culminating in a small number of fundamental parsers
//! that are implemented in Rust. Among these are `punct!` and `keyword!`.
//!
//! All synom fundamental parsers (those not combined out of other parsers)
//! should be written to skip over leading whitespace in their input. This way,
//! as long as every parser eventually boils down to some combination of
//! fundamental parsers, we get correct whitespace handling at all levels for
//! free.
//!
//! For our use case, this strategy is a huge improvement in usability,
//! correctness, and compile time over nom's `ws!` strategy.

use proc_macro2::TokenStream;

pub use cursor::*;

pub use error::{PResult, ParseError};

pub trait Synom: Sized {
    fn parse(input: Cursor) -> PResult<Self>;

    fn description() -> Option<&'static str> {
        None
    }
}

impl Synom for TokenStream {
    fn parse(input: Cursor) -> PResult<Self> {
        Ok((Cursor::empty(), input.token_stream()))
    }

    fn description() -> Option<&'static str> {
        Some("arbitrary token stream")
    }
}
