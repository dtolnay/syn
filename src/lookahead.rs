use std::cell::RefCell;

use proc_macro2::{Delimiter, Span};

use buffer::Cursor;
use error::{self, Error};
use span::IntoSpans;
use token::Token;

/// Support for checking the next token in a stream to decide how to parse.
///
/// An important advantage over [`ParseStream::peek`] is that here we
/// automatically construct an appropriate error message based on the token
/// alternatives that get peeked. If you are producing your own error message,
/// go ahead and use `ParseStream::peek` instead.
///
/// Use [`ParseStream::lookahead1`] to construct this object.
///
/// [`ParseStream::peek`]: struct.ParseBuffer.html#method.peek
/// [`ParseStream::lookahead1`]: struct.ParseBuffer.html#method.lookahead1
///
/// # Example
///
/// ```
/// # extern crate syn;
/// #
/// use syn::{ConstParam, Ident, Lifetime, LifetimeDef, Token, TypeParam};
/// use syn::parse::{Parse, ParseStream, Result};
///
/// // A generic parameter, a single one of the comma-separated elements inside
/// // angle brackets in:
/// //
/// //     fn f<T: Clone, 'a, 'b: 'a, const N: usize>() { ... }
/// //
/// // On invalid input, lookahead gives us a reasonable error message.
/// //
/// //     error: expected one of: identifier, lifetime, `const`
/// //       |
/// //     5 |     fn f<!Sized>() {}
/// //       |          ^
/// enum GenericParam {
///     Type(TypeParam),
///     Lifetime(LifetimeDef),
///     Const(ConstParam),
/// }
///
/// impl Parse for GenericParam {
///     fn parse(input: ParseStream) -> Result<Self> {
///         let lookahead = input.lookahead1();
///         if lookahead.peek(Ident) {
///             input.parse().map(GenericParam::Type)
///         } else if lookahead.peek(Lifetime) {
///             input.parse().map(GenericParam::Lifetime)
///         } else if lookahead.peek(Token![const]) {
///             input.parse().map(GenericParam::Const)
///         } else {
///             Err(lookahead.error())
///         }
///     }
/// }
/// #
/// # fn main() {}
/// ```
pub struct Lookahead1<'a> {
    scope: Span,
    cursor: Cursor<'a>,
    comparisons: RefCell<Vec<String>>,
}

pub fn new(scope: Span, cursor: Cursor) -> Lookahead1 {
    Lookahead1 {
        scope: scope,
        cursor: cursor,
        comparisons: RefCell::new(Vec::new()),
    }
}

impl<'a> Lookahead1<'a> {
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

    pub fn cursor(&self) -> Cursor<'a> {
        self.cursor
    }
}

/// Types that can be parsed by looking at just one token.
///
/// Use [`ParseStream::peek`] to peek one of these types in a parse stream
/// without consuming it from the stream.
///
/// This trait is sealed and cannot be implemented for types outside of Syn.
///
/// [`ParseStream::peek`]: struct.ParseBuffer.html#method.peek
pub trait Peek: private::Sealed {
    // Not public API.
    #[doc(hidden)]
    type Token: Token;
}

impl<F: FnOnce(TokenMarker) -> T, T: Token> Peek for F {
    type Token = T;
}

pub enum TokenMarker {}

impl<S> IntoSpans<S> for TokenMarker {
    fn into_spans(self) -> S {
        match self {}
    }
}

pub fn is_delimiter(lookahead: &Lookahead1, delimiter: Delimiter) -> bool {
    lookahead.cursor.group(delimiter).is_some()
}

mod private {
    use super::{Token, TokenMarker};
    pub trait Sealed {}
    impl<F: FnOnce(TokenMarker) -> T, T: Token> Sealed for F {}
}
