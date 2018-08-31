//! Parsing interface for parsing a token stream into a syntax tree node.

use std::cell::Cell;
use std::fmt::Display;
use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;
use std::rc::Rc;

use proc_macro2::{Ident, Span};

use buffer::Cursor;
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
        if self.is_empty() {
            return false;
        }
        let ahead = self.fork();
        ahead
            .step(|cursor| Ok(cursor.token_tree().unwrap()))
            .unwrap();
        ahead.peek(token)
    }

    pub fn peek3<T: Peek>(&self, token: T) -> bool {
        if self.is_empty() {
            return false;
        }
        let ahead = self.fork();
        ahead
            .step(|cursor| Ok(cursor.token_tree().unwrap()))
            .unwrap();
        ahead
            .step(|cursor| Ok(cursor.token_tree().unwrap()))
            .unwrap();
        ahead.peek(token)
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
