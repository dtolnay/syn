use proc_macro;
use proc_macro2;
use std::error::Error;
use cursor::Cursor;
use std::fmt::{self, Display};

/// The result of a parser
pub type PResult<'a, O> = Result<(Cursor<'a>, O), ParseError>;

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

impl From<proc_macro2::LexError> for ParseError {
    fn from(_: proc_macro2::LexError) -> ParseError {
        ParseError(Some("error while lexing input string".to_owned()))
    }
}

impl From<proc_macro::LexError> for ParseError {
    fn from(_: proc_macro::LexError) -> ParseError {
        ParseError(Some("error while lexing input string".to_owned()))
    }
}

impl ParseError {
    // For syn use only. Not public API.
    #[doc(hidden)]
    pub fn new<T: Into<String>>(msg: T) -> Self {
        ParseError(Some(msg.into()))
    }
}
