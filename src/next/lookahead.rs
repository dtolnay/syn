use std::cell::RefCell;

use buffer::Cursor;
use proc_macro2::Span;

use super::error;
use super::parse::Error;
use super::token::Token;

/// Support for checking the next token in a stream to decide how to parse.
///
/// Use [`ParseStream::lookahead1`] to construct this object.
///
/// [`ParseStream::lookahead1`]: struct.ParseBuffer.html#method.lookahead1
pub struct Lookahead1<'a> {
    scope: Span,
    cursor: Cursor<'a>,
    comparisons: RefCell<Vec<String>>,
}

impl<'a> Lookahead1<'a> {
    // Not public API.
    #[doc(hidden)]
    pub fn new(scope: Span, cursor: Cursor<'a>) -> Self {
        Lookahead1 {
            scope: scope,
            cursor: cursor,
            comparisons: RefCell::new(Vec::new()),
        }
    }

    pub fn peek<T: Peek>(&self, token: T) -> bool {
        let _ = token;
        if T::Token::peek(self) {
            return true;
        }
        self.comparisons.borrow_mut().push(T::Token::display());
        false
    }

    pub fn error(self) -> Error {
        let comparisons = self.comparisons.borrow();
        match comparisons.len() {
            0 => if self.cursor.eof() {
                Error::new(self.scope, "unexpected end of input")
            } else {
                Error::new(self.cursor.span(), "unexpected token")
            },
            1 => {
                let message = format!("expected {}", comparisons[0]);
                error::new_at(self.scope, self.cursor, message)
            }
            _ => {
                let join = comparisons.join(", ");
                let message = format!("expected one of: {}", join);
                error::new_at(self.scope, self.cursor, message)
            }
        }
    }
}

/// Types that can be parsed by looking at just one token.
///
/// This trait is sealed and cannot be implemented for types outside of Syn.
pub trait Peek: private::Sealed {
    // Not public API.
    #[doc(hidden)]
    type Token: Token;
}

impl<F: FnOnce(TokenMarker) -> T, T: Token> Peek for F {
    type Token = T;
}

pub enum TokenMarker {}

// Not public API.
#[doc(hidden)]
pub fn is_token(lookahead: &Lookahead1, repr: &'static str) -> bool {
    if let Some((token, _rest)) = lookahead.cursor.token_tree() {
        token.to_string() == repr
    } else {
        false
    }
}

mod private {
    use super::{Token, TokenMarker};
    pub trait Sealed {}
    impl<F: FnOnce(TokenMarker) -> T, T: Token> Sealed for F {}
}
