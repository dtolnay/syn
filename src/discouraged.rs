//! Extensions to the parsing API with niche applicability.

use super::*;

/// Extensions to the `ParseStream` API to support speculative parsing.
pub trait Speculative {
    /// Advance this parse stream to the position of a forked parse stream.
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
    /// Also note that if `A` is a subset of `B`, `A* B*` can be parsed by parsing
    /// `B*` and removing the leading members of `A` from the repetition, bypassing
    /// the need to involve the downsides associated with speculative parsing.
    ///
    /// [`ParseStream::fork`]: ../struct.ParseBuffer.html#method.fork
    ///
    /// # Example
    ///
    /// There has been chatter about the possibility of making the colons in the
    /// turbofish syntax like `path::to::<T>` no longer required by accepting
    /// `path::to<T>` in expression position. Specifically, according to [RFC#2544],
    /// [`PathSegment`] parsing should always try to consume a following `<` token
    /// as the start of generic arguments, and reset to the `<` if that fails
    /// (e.g. the token is acting as a less-than operator).
    ///
    /// This is the exact kind of parsing behavior which requires the "fork, try,
    /// commit" behavior that [`ParseStream::fork`] discourages. With `advance_to`,
    /// we can avoid having to parse the speculatively parsed content a second time.
    ///
    /// This change in behavior can be implemented in syn by replacing just the
    /// `Parse` implementation for `PathSegment`:
    ///
    /// ```edition2018
    /// # use syn::ext::IdentExt;
    /// use syn::parse::discouraged::Speculative;
    /// # use syn::parse::{Parse, ParseStream};
    /// # use syn::{Ident, PathArguments, Result, Token};
    ///
    /// pub struct PathSegment {
    ///     pub ident: Ident,
    ///     pub arguments: PathArguments,
    /// }
    ///
    /// # impl<T> From<T> for PathSegment
    /// # where
    /// #     T: Into<Ident>,
    /// # {
    /// #     fn from(ident: T) -> Self {
    /// #         PathSegment {
    /// #             ident: ident.into(),
    /// #             arguments: PathArguments::None,
    /// #         }
    /// #     }
    /// # }
    ///
    ///
    /// impl Parse for PathSegment {
    ///     fn parse(input: ParseStream) -> Result<Self> {
    ///         if input.peek(Token![super])
    ///             || input.peek(Token![self])
    ///             || input.peek(Token![Self])
    ///             || input.peek(Token![crate])
    ///             || input.peek(Token![extern])
    ///         {
    ///             let ident = input.call(Ident::parse_any)?;
    ///             return Ok(PathSegment::from(ident));
    ///         }
    ///
    ///         let ident = input.parse()?;
    ///         if input.peek(Token![::]) && input.peek3(Token![<]) {
    ///             return Ok(PathSegment {
    ///                 ident: ident,
    ///                 arguments: PathArguments::AngleBracketed(input.parse()?),
    ///             });
    ///         }
    ///         if input.peek(Token![<]) && !input.peek(Token![<=]) {
    ///             let fork = input.fork();
    ///             if let Ok(arguments) = fork.parse() {
    ///                 input.advance_to(&fork);
    ///                 return Ok(PathSegment {
    ///                     ident: ident,
    ///                     arguments: PathArguments::AngleBracketed(arguments),
    ///                 });
    ///             }
    ///         }
    ///         Ok(PathSegment::from(ident))
    ///     }
    /// }
    ///
    /// # syn::parse_str::<PathSegment>("a<b,c>").unwrap();
    /// ```
    ///
    /// [RFC#2544]: https://github.com/rust-lang/rfcs/pull/2544
    /// [`PathSegment`]: ../../struct.PathSegment.html
    ///
    /// # Panics
    ///
    /// The forked stream that this joins with must be derived by forking this parse stream.
    fn advance_to(&self, fork: &Self);
}

impl<'a> Speculative for ParseBuffer<'a> {
    fn advance_to(&self, fork: &Self) {
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
