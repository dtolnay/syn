//! This module defines a cheaply-copyable cursor into a TokenStream's data.
//!
//! It does this by copying the data into a stably-addressed structured buffer,
//! and holding raw pointers into that buffer to allow walking through delimited
//! sequences cheaply.
//!
//! This module is heavily commented as it contains the only unsafe code in
//! `syn`, and caution should be made when editing it. It provides a safe
//! interface, but is fragile internally.

use proc_macro2::*;

use std::ptr;
use std::fmt;
use std::marker::PhantomData;

/// Internal type which is used instead of `TokenTree` to represent a single
/// `TokenTree` within a `SynomBuffer`.
#[derive(Debug)]
enum Entry {
    /// Mimicing types from proc-macro.
    Group(Span, Delimiter, SynomBuffer),
    Term(Span, Term),
    Op(Span, char, Spacing),
    Literal(Span, Literal),
    /// End entries contain a raw pointer to the entry from the containing
    /// TokenTree.
    End(*const Entry),
}

/// A buffer of data which contains a structured representation of the input
/// `TokenStream` object.
#[derive(Debug)]
pub struct SynomBuffer {
    // NOTE: Do not derive clone on this - there are raw pointers inside which
    // will be messed up. Moving the `SynomBuffer` itself is safe as the actual
    // backing slices won't be moved.
    data: Box<[Entry]>,
}

impl SynomBuffer {
    // NOTE: DO NOT MUTATE THE `Vec` RETURNED FROM THIS FUNCTION ONCE IT
    // RETURNS, THE ADDRESS OF ITS BACKING MEMORY MUST REMAIN STABLE.
    fn inner_new(stream: TokenStream, up: *const Entry) -> SynomBuffer {
        // Build up the entries list, recording the locations of any Groups
        // in the list to be processed later.
        let mut entries = Vec::new();
        let mut seqs = Vec::new();
        for tt in stream.into_iter() {
            match tt.kind {
                TokenNode::Term(sym) => {
                    entries.push(Entry::Term(tt.span, sym));
                }
                TokenNode::Op(chr, ok) => {
                    entries.push(Entry::Op(tt.span, chr, ok));
                }
                TokenNode::Literal(lit) => {
                    entries.push(Entry::Literal(tt.span, lit));
                }
                TokenNode::Group(delim, seq_stream) => {
                    // Record the index of the interesting entry, and store an
                    // `End(null)` there temporarially.
                    seqs.push((entries.len(), tt.span, delim, seq_stream));
                    entries.push(Entry::End(ptr::null()));
                }
            }
        }
        // Add an `End` entry to the end with a reference to the enclosing token
        // stream which was passed in.
        entries.push(Entry::End(up));

        // NOTE: This is done to ensure that we don't accidentally modify the
        // length of the backing buffer. The backing buffer must remain at a
        // constant address after this point, as we are going to store a raw
        // pointer into it.
        let mut entries = entries.into_boxed_slice();
        for (idx, span, delim, seq_stream) in seqs {
            // We know that this index refers to one of the temporary
            // `End(null)` entries, and we know that the last entry is
            // `End(up)`, so the next index is also valid.
            let seq_up = &entries[idx + 1] as *const Entry;

            // The end entry stored at the end of this Entry::Group should
            // point to the Entry which follows the Group in the list.
            let inner = Self::inner_new(seq_stream, seq_up);
            entries[idx] = Entry::Group(span, delim, inner);
        }

        SynomBuffer {
            data: entries
        }
    }

    /// Create a new SynomBuffer, which contains the data from the given
    /// TokenStream.
    pub fn new(stream: TokenStream) -> SynomBuffer {
        Self::inner_new(stream, ptr::null())
    }

    /// Create a cursor referencing the first token in the input.
    pub fn begin(&self) -> Cursor {
        unsafe {
            Cursor::create(&self.data[0], &self.data[self.data.len() - 1])
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub struct SeqInfo<'a> {
    pub span: Span,
    pub inside: Cursor<'a>,
    pub outside: Cursor<'a>,
}

/// A cursor into an input `TokenStream`'s data. This cursor holds a reference
/// into the immutable data which is used internally to represent a
/// `TokenStream`, and can be efficiently manipulated and copied around.
///
/// An empty `Cursor` can be created directly, or one may create a `SynomBuffer`
/// object and get a cursor to its first token with `begin()`.
///
/// Two cursors are equal if they have the same location in the same input
/// stream, and have the same scope.
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Cursor<'a> {
    /// The current entry which the `Cursor` is pointing at.
    ptr: *const Entry,
    /// This is the only `Entry::End(..)` object which this cursor is allowed to
    /// point at. All other `End` objects are skipped over in `Cursor::create`.
    scope: *const Entry,
    /// This uses the &'a reference which guarantees that these pointers are
    /// still valid.
    marker: PhantomData<&'a Entry>,
}

impl<'a> Cursor<'a> {
    /// Create a cursor referencing a static empty TokenStream.
    pub fn empty() -> Self {
        // It's safe in this situation for us to put an `Entry` object in global
        // storage, despite it not actually being safe to send across threads
        // (`Term` is a reference into a thread-local table). This is because
        // this entry never includes a `Term` object.
        //
        // This wrapper struct allows us to break the rules and put a `Sync`
        // object in global storage.
        struct UnsafeSyncEntry(Entry);
        unsafe impl Sync for UnsafeSyncEntry {}
        static EMPTY_ENTRY: UnsafeSyncEntry =
            UnsafeSyncEntry(Entry::End(0 as *const Entry));

        Cursor {
            ptr: &EMPTY_ENTRY.0,
            scope: &EMPTY_ENTRY.0,
            marker: PhantomData,
        }
    }

    /// This create method intelligently exits non-explicitly-entered
    /// `None`-delimited scopes when the cursor reaches the end of them,
    /// allowing for them to be treated transparently.
    unsafe fn create(mut ptr: *const Entry, scope: *const Entry) -> Self {
        // NOTE: If we're looking at a `End(..)`, we want to advance the cursor
        // past it, unless `ptr == scope`, which means that we're at the edge of
        // our cursor's scope. We should only have `ptr != scope` at the exit
        // from None-delimited sequences entered with `ignore_none`.
        while let Entry::End(exit) = *ptr {
            if ptr == scope {
                break;
            }
            ptr = exit;
        }

        Cursor {
            ptr: ptr,
            scope: scope,
            marker: PhantomData,
        }
    }

    /// Get the current entry.
    fn entry(self) -> &'a Entry {
        unsafe { &*self.ptr }
    }

    /// Bump the cursor to point at the next token after the current one. This
    /// is undefined behavior if the cursor is currently looking at an
    /// `Entry::End`.
    unsafe fn bump(self) -> Cursor<'a> {
        Cursor::create(self.ptr.offset(1), self.scope)
    }

    /// If the cursor is looking at a `None`-delimited sequence, move it to look
    /// at the first token inside instead. If the sequence is empty, this will
    /// move the cursor past the `None`-delimited sequence.
    ///
    /// WARNING: This mutates its argument.
    fn ignore_none(&mut self) {
        if let Entry::Group(_, Delimiter::None, ref buf) = *self.entry() {
            // NOTE: We call `Cursor::create` here to make sure that situations
            // where we should immediately exit the span after entering it are
            // handled correctly.
            unsafe {
                *self = Cursor::create(&buf.data[0], self.scope);
            }
        }
    }

    /// Check if the cursor is currently pointing at the end of its valid scope.
    #[inline]
    pub fn eof(self) -> bool {
        // We're at eof if we're at the end of our scope.
        self.ptr == self.scope
    }

    /// If the cursor is pointing at a Seq with the given `Delimiter`, return a
    /// cursor into that sequence, and one pointing to the next `TokenTree`.
    pub fn seq(mut self, seq_delim: Delimiter) -> Option<SeqInfo<'a>> {
        // If we're not trying to enter a none-delimited sequence, we want to
        // ignore them. We have to make sure to _not_ ignore them when we want
        // to enter them, of course. For obvious reasons.
        if seq_delim != Delimiter::None {
            self.ignore_none();
        }

        match *self.entry() {
            Entry::Group(span, delim, ref buf) => {
                if delim != seq_delim {
                    return None;
                }

                Some(SeqInfo {
                    span: span,
                    inside: buf.begin(),
                    outside: unsafe { self.bump() },
                })
            }
            _ => None
        }
    }

    /// If the cursor is pointing at a Term, return it and a cursor pointing at
    /// the next `TokenTree`.
    pub fn word(mut self) -> Option<(Cursor<'a>, Span, Term)> {
        self.ignore_none();
        match *self.entry() {
            Entry::Term(span, sym) => {
                Some((
                    unsafe { self.bump() },
                    span,
                    sym,
                ))
            }
            _ => None
        }
    }

    /// If the cursor is pointing at an Op, return it and a cursor pointing
    /// at the next `TokenTree`.
    pub fn op(mut self) -> Option<(Cursor<'a>, Span, char, Spacing)> {
        self.ignore_none();
        match *self.entry() {
            Entry::Op(span, chr, kind) => {
                Some((
                    unsafe { self.bump() },
                    span,
                    chr,
                    kind,
                ))
            }
            _ => None
        }
    }

    /// If the cursor is pointing at a Literal, return it and a cursor pointing
    /// at the next `TokenTree`.
    pub fn literal(mut self) -> Option<(Cursor<'a>, Span, Literal)> {
        self.ignore_none();
        match *self.entry() {
            Entry::Literal(span, ref lit) => {
                Some((
                    unsafe { self.bump() },
                    span,
                    lit.clone(),
                ))
            }
            _ => None
        }
    }

    /// Copy all remaining tokens visible from this cursor into a `TokenStream`.
    pub fn token_stream(self) -> TokenStream {
        let mut tts = Vec::new();
        let mut cursor = self;
        while let Some((next, tt)) = cursor.token_tree() {
            tts.push(tt);
            cursor = next;
        }
        tts.into_iter().collect()
    }

    /// If the cursor is looking at a `TokenTree`, returns it along with a
    /// cursor pointing to the next token in the sequence, otherwise returns
    /// `None`.
    ///
    /// This method does not treat `None`-delimited sequences as invisible, and
    /// will return a `Group(None, ..)` if the cursor is looking at one.
    pub fn token_tree(self) -> Option<(Cursor<'a>, TokenTree)> {
        let tree = match *self.entry() {
            Entry::Group(span, delim, ref buf) => {
                let stream = buf.begin().token_stream();
                TokenTree {
                    span: span,
                    kind: TokenNode::Group(delim, stream),
                }
            }
            Entry::Literal(span, ref lit) => {
                TokenTree {
                    span: span,
                    kind: TokenNode::Literal(lit.clone()),
                }
            }
            Entry::Term(span, sym) => {
                TokenTree {
                    span: span,
                    kind: TokenNode::Term(sym),
                }
            }
            Entry::Op(span, chr, kind) => {
                TokenTree {
                    span: span,
                    kind: TokenNode::Op(chr, kind),
                }
            }
            Entry::End(..) => {
                return None;
            }
        };

        Some((
            unsafe { self.bump() },
            tree
        ))
    }
}

// We do a custom implementation for `Debug` as the default implementation is
// pretty useless.
impl<'a> fmt::Debug for Cursor<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Cursor")
            .field("ptr", &self.ptr)
            .field("scope", &self.scope)
            // Dummy `entry` field to show data behind the `ptr` ptr.
            .field("entry", self.entry())
            .finish()
    }
}
