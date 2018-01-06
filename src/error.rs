// Copyright 2018 Syn Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::error::Error;
use buffer::Cursor;
use std::fmt::{self, Display};

/// The result of a parser
pub type PResult<'a, O> = Result<(O, Cursor<'a>), ParseError>;

/// An error with a default error message.
///
/// NOTE: We should provide better error messages in the future.
pub fn parse_error<O>() -> PResult<'static, O> {
    Err(ParseError(None))
}

#[derive(Debug)]
pub struct ParseError(Option<String>);

impl Error for ParseError {
    fn description(&self) -> &str {
        match self.0 {
            Some(ref desc) => desc,
            None => "failed to parse",
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <str as fmt::Display>::fmt(self.description(), f)
    }
}

impl ParseError {
    // For syn use only. Not public API.
    #[doc(hidden)]
    pub fn new<T: Into<String>>(msg: T) -> Self {
        ParseError(Some(msg.into()))
    }
}
