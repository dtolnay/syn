//! Extensions to the parsing API with niche applicability.

use super::*;

/// Extensions to the `ParseStream` API to support speculative parsing.
pub trait Speculative {
    /// Join this parse stream up with a forked parse stream.
    ///
    /// This is the opposite operation to [`ParseStream::fork`].
    /// You can fork a parse stream, perform some speculative parsing, then join
    /// the original stream to the fork to "commit" the parsing from the fork to
    /// the main stream.
    ///
    /// If you can avoid doing this, you should, as it limits the ability to
    /// generate useful errors. That said, it is often the only way to parse
    /// syntax of the form `A* B*` for arbitrary syntax `A` and `B`. The problem
    /// is that when the fork fails to parse an `A`, it's impossible to tell
    /// whether that was because of a syntax error and the user meant to provide
    /// an `A`, or that the `A`s are finished and its time to start parsing `B`s.
    /// Use with care.
    ///
    /// # Example
    ///
    /// Let's show the example from [`ParseStream::fork`] of `pub(restricted)`
    /// syntax, but using the fork for the speculative parsing instead of just
    /// using it to peek at what's coming.
    ///
    /// The fork will speculatively attempt to parse the `(restricted)` part of
    /// `pub(restricted)`. If successful, the main stream will be joined up;
    /// otherwise, control flow returns to the main parse stream without the
    /// speculative parsing committed to the main stream.
    ///
    /// ```edition2018
    /// use syn::{parenthesized, token, Ident, Path, Result, Token};
    /// use syn::ext::IdentExt;
    /// use syn::parse::{Parse, ParseStream};
    ///
    /// struct PubVisibility {
    ///     pub_token: Token![pub],
    ///     restricted: Option<Restricted>,
    /// }
    ///
    /// struct Restricted {
    ///     paren_token: token::Paren,
    ///     in_token: Option<Token![in]>,
    ///     path: Path,
    /// }
    ///
    /// impl Parse for PubVisibility {
    ///     fn parse(input: ParseStream) -> Result<Self> {
    ///         Ok(PubVisibility {
    ///             pub_token: input.parse()?,
    ///             restricted: {
    ///                 let fork = input.fork();
    ///                 if let Ok(restricted) = fork.parse() {
    ///                     input.join(&fork);
    ///                     Some(restricted)
    ///                 } else {
    ///                     None
    ///                 }
    ///             }
    ///         })
    ///     }
    /// }
    ///
    /// impl Parse for Restricted {
    ///     fn parse(input: ParseStream) -> Result<Self> {
    ///         let content;
    ///         let paren_token = parenthesized!(content in input);
    ///
    ///         let la = content.lookahead1();
    ///         if la.peek(Token![crate])
    ///             || la.peek(Token![self])
    ///             || la.peek(Token![super])
    ///         {
    ///             Ok(Restricted {
    ///                 paren_token: paren_token,
    ///                 in_token: None,
    ///                 path: Path::from(content.call(Ident::parse_any)?),
    ///             })
    ///         } else if la.peek(Token![in]) {
    ///             Ok(Restricted {
    ///                 paren_token: paren_token,
    ///                 in_token: Some(content.parse()?),
    ///                 path: content.call(Path::parse_mod_style)?,
    ///             })
    ///         } else {
    ///             // never seen due to if let in <Parse for PubVisibility>
    ///             Err(la.error())
    ///         }
    ///     }
    /// }
    /// ```
    ///
    /// # Panics
    ///
    /// The forked stream that this joins with must be derived by forking this parse stream.
    ///
    /// [`ParseStream::fork`]: #method.fork
    fn join(&self, fork: &Self);
}

impl Speculative for ParseBuffer {
    fn join(&self, fork: &Self) {
        // See comment on `scope` in the struct definition.
        assert_eq!(
            // Rc::ptr_eq for rustc < 1.17.0
            &*self.scope as *const _, &*fork.scope as *const _,
            "Fork was not derived from the advancing parse stream"
        );
        // See comment on `cell` in the struct definition.
        self.cell.set(unsafe { mem::transmute::<Cursor, Cursor<'static>>(fork.cursor()) })
    }
}
