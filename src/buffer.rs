//! A stably addressed token buffer supporting efficient traversal based on a
//! cheaply copyable cursor.
//!
//! *This module is available only if Syn is built with the `"parsing"` feature.*

// This module is heavily commented as it contains most of the unsafe code in
// Syn, and caution should be used when editing it. The public-facing interface
// is 100% safe but the implementation is fragile internally.

#[cfg(all(
    not(all(target_arch = "wasm32", any(target_os = "unknown", target_os = "wasi"))),
    feature = "proc-macro"
))]
use crate::proc_macro as pm;
use crate::Lifetime;
use proc_macro2::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};
use std::hint;
use std::marker::PhantomData;
use std::ptr::NonNull;

/// Internal type which is used instead of `TokenTree` to represent a token tree
/// within a `TokenBuffer`.
enum Entry {
    // Mimicking types from proc-macro.
    // Group entries contain the offset to the next entry beyond the end of the group.
    Group(Group, usize),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
}

/// A buffer that can be efficiently traversed multiple times, unlike
/// `TokenStream` which requires a deep copy in order to traverse more than
/// once.
///
/// *This type is available only if Syn is built with the `"parsing"` feature.*
pub struct TokenBuffer {
    // NOTE: Do not implement clone on this - while the current design could be
    // cloned, other designs which could be desirable may not be cloneable.
    entries: Box<[Entry]>,
}

// Keep the explicit impl for backwards compatibility.
impl Drop for TokenBuffer {
    fn drop(&mut self) {}
}

impl TokenBuffer {
    fn new_inner(entries: &mut Vec<Entry>, stream: TokenStream) {
        for tt in stream {
            match tt {
                TokenTree::Ident(ident) => entries.push(Entry::Ident(ident)),
                TokenTree::Punct(punct) => entries.push(Entry::Punct(punct)),
                TokenTree::Literal(literal) => entries.push(Entry::Literal(literal)),
                TokenTree::Group(group) => {
                    let start = entries.len();
                    let stream = group.stream();
                    entries.push(Entry::Group(group, 0)); // we replace the 0 below
                    Self::new_inner(entries, stream);
                    let group_size = entries.len() - start;
                    match unsafe { entries.get_unchecked_mut(start) } {
                        Entry::Group(_, placeholder_size) => *placeholder_size = group_size,
                        _ => unsafe { hint::unreachable_unchecked() },
                    }
                }
            }
        }
    }

    /// Creates a `TokenBuffer` containing all the tokens from the input
    /// `proc_macro::TokenStream`.
    ///
    /// *This method is available only if Syn is built with both the `"parsing"` and
    /// `"proc-macro"` features.*
    #[cfg(all(
        not(all(target_arch = "wasm32", any(target_os = "unknown", target_os = "wasi"))),
        feature = "proc-macro"
    ))]
    pub fn new(stream: pm::TokenStream) -> Self {
        Self::new2(stream.into())
    }

    /// Creates a `TokenBuffer` containing all the tokens from the input
    /// `proc_macro2::TokenStream`.
    pub fn new2(stream: TokenStream) -> Self {
        let mut entries = Vec::new();
        Self::new_inner(&mut entries, stream);
        Self {
            entries: entries.into_boxed_slice(),
        }
    }

    /// Creates a cursor referencing the first token in the buffer and able to
    /// traverse until the end of the buffer.
    pub fn begin(&self) -> Cursor {
        let ptr = self.entries.as_ptr();
        unsafe { Cursor::create(ptr, ptr.add(self.entries.len())) }
    }
}

/// A cheaply copyable cursor into a `TokenBuffer`.
///
/// This cursor holds a shared reference into the immutable data which is used
/// internally to represent a `TokenStream`, and can be efficiently manipulated
/// and copied around.
///
/// An empty `Cursor` can be created directly, or one may create a `TokenBuffer`
/// object and get a cursor to its first token with `begin()`.
///
/// Two cursors are equal if they have the same location in the same input
/// stream, and have the same scope.
///
/// *This type is available only if Syn is built with the `"parsing"` feature.*
pub struct Cursor<'a> {
    // The current entry which the `Cursor` is pointing at.
    ptr: *const Entry,
    // This is the only `Entry::End` object which this cursor is allowed to
    // point at. All other `End` objects are skipped over in `Cursor::create`.
    scope: *const Entry,
    // Cursor is covariant in 'a. This field ensures that our pointers are still
    // valid.
    marker: PhantomData<&'a Entry>,
}

impl<'a> Cursor<'a> {
    /// Creates a cursor referencing a static empty TokenStream.
    pub fn empty() -> Self {
        let ptr = NonNull::dangling().as_ptr() as *const Entry;
        Cursor {
            ptr,
            scope: ptr,
            marker: PhantomData,
        }
    }

    /// This create method intelligently exits non-explicitly-entered
    /// `None`-delimited scopes when the cursor reaches the end of them,
    /// allowing for them to be treated transparently.
    unsafe fn create(ptr: *const Entry, scope: *const Entry) -> Self {
        Cursor {
            ptr,
            scope,
            marker: PhantomData,
        }
    }

    /// Get the current entry.
    fn entry(self) -> Option<&'a Entry> {
        if self.eof() {
            None
        } else {
            Some(unsafe { &*self.ptr })
        }
    }

    /// Bump the cursor to point at the next token after the current one. This
    /// is undefined behavior if the cursor is currently looking at an
    /// `Entry::End`.
    ///
    /// If the cursor is looking at an `Entry::Group`, the bumped cursor will
    /// point at the first token in the group (with the same scope end).
    unsafe fn bump_ignore_group(self) -> Cursor<'a> {
        Cursor::create(self.ptr.offset(1), self.scope)
    }

    /// While the cursor is looking at a `None`-delimited group, move it to look
    /// at the first token inside instead. If the group is empty, this will move
    /// the cursor past the `None`-delimited group.
    ///
    /// WARNING: This mutates its argument.
    fn ignore_none(&mut self) {
        while let Some(Entry::Group(group, _)) = self.entry() {
            if group.delimiter() == Delimiter::None {
                unsafe { *self = self.bump_ignore_group() };
            } else {
                break;
            }
        }
    }

    /// Checks whether the cursor is currently pointing at the end of its valid
    /// scope.
    pub fn eof(self) -> bool {
        // We're at eof if we're at the end of our scope.
        self.ptr == self.scope
    }

    /// If the cursor is pointing at a `Group` with the given delimiter, returns
    /// a cursor into that group and one pointing to the next `TokenTree`.
    pub fn group(mut self, delim: Delimiter) -> Option<(Cursor<'a>, Span, Cursor<'a>)> {
        // If we're not trying to enter a none-delimited group, we want to
        // ignore them. We have to make sure to _not_ ignore them when we want
        // to enter them, of course. For obvious reasons.
        if delim != Delimiter::None {
            self.ignore_none();
        }

        if let Entry::Group(group, end_offset) = self.entry()? {
            if group.delimiter() == delim {
                let end_of_group = unsafe { self.ptr.add(*end_offset) };
                let inside_of_group = unsafe { Cursor::create(self.ptr.add(1), end_of_group) };
                let after_group = unsafe { Cursor::create(end_of_group, self.scope) };
                return Some((inside_of_group, group.span(), after_group));
            }
        }

        None
    }

    /// If the cursor is pointing at a `Ident`, returns it along with a cursor
    /// pointing at the next `TokenTree`.
    pub fn ident(mut self) -> Option<(Ident, Cursor<'a>)> {
        self.ignore_none();
        match self.entry()? {
            Entry::Ident(ident) => Some((ident.clone(), unsafe { self.bump_ignore_group() })),
            _ => None,
        }
    }

    /// If the cursor is pointing at a `Punct`, returns it along with a cursor
    /// pointing at the next `TokenTree`.
    pub fn punct(mut self) -> Option<(Punct, Cursor<'a>)> {
        self.ignore_none();
        match self.entry()? {
            Entry::Punct(punct) if punct.as_char() != '\'' => {
                Some((punct.clone(), unsafe { self.bump_ignore_group() }))
            }
            _ => None,
        }
    }

    /// If the cursor is pointing at a `Literal`, return it along with a cursor
    /// pointing at the next `TokenTree`.
    pub fn literal(mut self) -> Option<(Literal, Cursor<'a>)> {
        self.ignore_none();
        match self.entry()? {
            Entry::Literal(literal) => Some((literal.clone(), unsafe { self.bump_ignore_group() })),
            _ => None,
        }
    }

    /// If the cursor is pointing at a `Lifetime`, returns it along with a
    /// cursor pointing at the next `TokenTree`.
    pub fn lifetime(mut self) -> Option<(Lifetime, Cursor<'a>)> {
        self.ignore_none();
        match self.entry()? {
            Entry::Punct(punct) if punct.as_char() == '\'' && punct.spacing() == Spacing::Joint => {
                let next = unsafe { self.bump_ignore_group() };
                let (ident, rest) = next.ident()?;
                let lifetime = Lifetime {
                    apostrophe: punct.span(),
                    ident,
                };
                Some((lifetime, rest))
            }
            _ => None,
        }
    }

    /// Copies all remaining tokens visible from this cursor into a
    /// `TokenStream`.
    pub fn token_stream(self) -> TokenStream {
        let mut tts = Vec::new();
        let mut cursor = self;
        while let Some((tt, rest)) = cursor.token_tree() {
            tts.push(tt);
            cursor = rest;
        }
        tts.into_iter().collect()
    }

    /// If the cursor is pointing at a `TokenTree`, returns it along with a
    /// cursor pointing at the next `TokenTree`.
    ///
    /// Returns `None` if the cursor has reached the end of its stream.
    ///
    /// This method does not treat `None`-delimited groups as transparent, and
    /// will return a `Group(None, ..)` if the cursor is looking at one.
    pub fn token_tree(self) -> Option<(TokenTree, Cursor<'a>)> {
        let (tree, len) = match self.entry()? {
            Entry::Group(group, end_offset) => (group.clone().into(), *end_offset),
            Entry::Literal(literal) => (literal.clone().into(), 1),
            Entry::Ident(ident) => (ident.clone().into(), 1),
            Entry::Punct(punct) => (punct.clone().into(), 1),
        };

        let rest = unsafe { Cursor::create(self.ptr.add(len), self.scope) };
        Some((tree, rest))
    }

    /// Returns the `Span` of the current token, or `Span::call_site()` if this
    /// cursor points to eof.
    pub fn span(self) -> Span {
        match self.entry() {
            Some(Entry::Group(group, _)) => group.span(),
            Some(Entry::Literal(literal)) => literal.span(),
            Some(Entry::Ident(ident)) => ident.span(),
            Some(Entry::Punct(punct)) => punct.span(),
            None => Span::call_site(),
        }
    }

    /// Skip over the next token without cloning it. Returns `None` if this
    /// cursor points to eof.
    ///
    /// This method treats `'lifetimes` as a single token.
    pub(crate) fn skip(self) -> Option<Cursor<'a>> {
        let len = match self.entry()? {
            // Treat lifetimes as a single tt for the purposes of 'skip'.
            Entry::Punct(punct) if punct.as_char() == '\'' && punct.spacing() == Spacing::Joint => {
                match unsafe { &*self.ptr.add(1) } {
                    Entry::Ident(_) => 2,
                    _ => 1,
                }
            }

            Entry::Group(_, end_offset) => *end_offset,
            _ => 1,
        };

        Some(unsafe { Cursor::create(self.ptr.add(len), self.scope) })
    }
}

impl<'a> Copy for Cursor<'a> {}

impl<'a> Clone for Cursor<'a> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a> Eq for Cursor<'a> {}

impl<'a> PartialEq for Cursor<'a> {
    fn eq(&self, other: &Self) -> bool {
        let Cursor { ptr, scope, marker } = self;
        let _ = marker;
        *ptr == other.ptr && *scope == other.scope
    }
}

pub(crate) fn same_scope(a: Cursor, b: Cursor) -> bool {
    a.scope == b.scope
}

pub(crate) fn open_span_of_group(cursor: Cursor) -> Span {
    match cursor.entry() {
        Some(Entry::Group(group, _)) => group.span_open(),
        _ => cursor.span(),
    }
}

pub(crate) fn close_span_of_group(cursor: Cursor) -> Span {
    match cursor.entry() {
        Some(Entry::Group(group, _)) => group.span_close(),
        _ => cursor.span(),
    }
}
