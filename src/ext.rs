//! Extension traits that are made available within the `call!` parser.
//!
//! *This module is available if Syn is built with the `"parsing"` feature.*

use proc_macro2::Ident;

use parse::{ParseStream, Result};

/// Additional parsing methods for `Ident`.
///
/// This trait is sealed and cannot be implemented for types outside of Syn.
///
/// *This trait is available if Syn is built with the `"parsing"` feature.*
pub trait IdentExt: Sized + private::Sealed {
    /// Parses any identifier including keywords.
    ///
    /// This is useful when parsing a DSL which allows Rust keywords as
    /// identifiers.
    ///
    /// ```rust
    /// #[macro_use]
    /// extern crate syn;
    ///
    /// use syn::Ident;
    ///
    /// // Parses input that looks like `name = NAME` where `NAME` can be
    /// // any identifier.
    /// //
    /// // Examples:
    /// //
    /// //     name = anything
    /// //     name = impl
    /// named!(parse_dsl -> Ident, do_parse!(
    ///     custom_keyword!(name) >>
    ///     punct!(=) >>
    ///     name: call!(Ident::parse_any) >>
    ///     (name)
    /// ));
    /// #
    /// # fn main() {}
    /// ```
    fn parse_any(input: ParseStream) -> Result<Self>;
}

impl IdentExt for Ident {
    fn parse_any(input: ParseStream) -> Result<Self> {
        input.step(|cursor| match cursor.ident() {
            Some((ident, rest)) => Ok((ident, rest)),
            None => Err(cursor.error("expected ident")),
        })
    }
}

mod private {
    use proc_macro2::Ident;

    pub trait Sealed {}

    impl Sealed for Ident {}
}
