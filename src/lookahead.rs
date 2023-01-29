use crate::buffer::Cursor;
use crate::error::{self, Error};
use proc_macro2::{Delimiter, Span};
use std::cell::RefCell;
use std::marker::PhantomData;
use std::ops::BitOr;

/// Support for checking the next token in a stream to decide how to parse.
///
/// An important advantage over [`ParseStream::peek`] is that here we
/// automatically construct an appropriate error message based on the token
/// alternatives that get peeked. If you are producing your own error message,
/// go ahead and use `ParseStream::peek` instead.
///
/// Use [`ParseStream::lookahead1`] to construct this object.
///
/// [`ParseStream::peek`]: crate::parse::ParseBuffer::peek
/// [`ParseStream::lookahead1`]: crate::parse::ParseBuffer::lookahead1
///
/// Consuming tokens from the source stream after constructing a lookahead
/// object does not also advance the lookahead object.
///
/// # Example
///
/// ```
/// use syn::{ConstParam, Ident, Lifetime, LifetimeDef, Result, Token, TypeParam};
/// use syn::parse::{Parse, ParseStream};
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
/// ```
pub struct Lookahead1<'a> {
    scope: Span,
    cursor: Cursor<'a>,
    comparisons: RefCell<Vec<&'static str>>,
}

pub fn new(scope: Span, cursor: Cursor) -> Lookahead1 {
    Lookahead1 {
        scope,
        cursor,
        comparisons: RefCell::new(Vec::new()),
    }
}

fn peek_impl(
    lookahead: &Lookahead1,
    peek: fn(Cursor) -> bool,
    display: fn(&mut dyn FnMut(&'static str)),
) -> bool {
    if peek(lookahead.cursor) {
        return true;
    }
    let mut comparisons = lookahead.comparisons.borrow_mut();
    display(&mut |s| comparisons.push(s));
    false
}

impl<'a> Lookahead1<'a> {
    /// Looks at the next token in the parse stream to determine whether it
    /// matches the requested type of token.
    ///
    /// # Syntax
    ///
    /// Note that this method does not use turbofish syntax. Pass the peek type
    /// inside of parentheses.
    ///
    /// - `input.peek(Token![struct])`
    /// - `input.peek(Token![==])`
    /// - `input.peek(Ident)`&emsp;*(does not accept keywords)*
    /// - `input.peek(Ident::peek_any)`
    /// - `input.peek(Lifetime)`
    /// - `input.peek(token::Brace)`
    pub fn peek<T: Peek>(&self, token: T) -> bool {
        let _ = token;
        peek_impl(self, T::peek, T::display)
    }

    /// Triggers an error at the current position of the parse stream.
    ///
    /// The error message will identify all of the expected token types that
    /// have been peeked against this lookahead instance.
    pub fn error(self) -> Error {
        let comparisons = self.comparisons.borrow();
        match comparisons.len() {
            0 => {
                if self.cursor.eof() {
                    Error::new(self.scope, "unexpected end of input")
                } else {
                    Error::new(self.cursor.span(), "unexpected token")
                }
            }
            1 => {
                let message = format!("expected {}", comparisons[0]);
                error::new_at(self.scope, self.cursor, message)
            }
            2 => {
                let message = format!("expected {} or {}", comparisons[0], comparisons[1]);
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
/// Use [`ParseStream::peek`] to peek one of these types in a parse stream
/// without consuming it from the stream.
///
/// This trait is sealed and cannot be implemented for types outside of Syn.
///
/// [`ParseStream::peek`]: crate::parse::ParseBuffer::peek
pub trait Peek: Copy {
    // Not public API.
    #[doc(hidden)]
    type Token;

    // Not public API.
    #[doc(hidden)]
    fn peek(cursor: Cursor) -> bool;

    // Not public API.
    #[doc(hidden)]
    fn display(f: &mut dyn FnMut(&'static str));
}

pub fn is_delimiter(cursor: Cursor, delimiter: Delimiter) -> bool {
    cursor.group(delimiter).is_some()
}

pub struct Either<A, B> {
    _a: PhantomData<A>,
    _b: PhantomData<B>,
}

impl<A, B> Copy for Either<A, B> {}
impl<A, B> Clone for Either<A, B> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<A: Peek, B: Peek> Either<A, B> {
    pub fn new() -> Self {
        Either {
            _a: PhantomData,
            _b: PhantomData,
        }
    }
}

impl<A: Peek, B: Peek> Peek for Either<A, B> {
    type Token = Self;
    fn peek(cursor: Cursor) -> bool {
        A::peek(cursor) || B::peek(cursor)
    }
    fn display(f: &mut dyn FnMut(&'static str)) {
        A::display(f);
        B::display(f);
    }
}

impl<A: Peek, B: Peek, U: Peek> BitOr<U> for Either<A, B> {
    type Output = Either<Self, U>;
    fn bitor(self, _other: U) -> Self::Output {
        Either::new()
    }
}
