#[macro_use]
pub mod parse;

// FIXME
pub mod lookahead;

use std::str::FromStr;

use buffer::TokenBuffer;
#[cfg(feature = "proc-macro")]
use proc_macro;
use proc_macro2::{self, Span};

use error::Result;
use self::parse::{Parse, ParseBuffer};

/// Parse tokens of source code into the chosen syntax tree node.
#[cfg(feature = "proc-macro")]
pub fn parse<T: Parse>(input: proc_macro::TokenStream) -> Result<T> {
    parse2(proc_macro2::TokenStream::from(input))
}

/// Parse a proc-macro2 token stream into the chosen syntax tree node.
pub fn parse2<T: Parse>(input: proc_macro2::TokenStream) -> Result<T> {
    let buf = TokenBuffer::new2(input);
    let state = ParseBuffer::new(Span::call_site(), buf.begin());
    T::parse(&state)
}

/// Parse a string of Rust code into the chosen syntax tree node.
pub fn parse_str<T: Parse>(input: &str) -> Result<T> {
    let tokens = proc_macro2::TokenStream::from_str(input)?;
    parse2(tokens)
}

/// Parse the input TokenStream of a macro, triggering a compile error if the
/// tokens fail to parse.
///
/// # Intended usage
///
/// ```rust
/// # extern crate proc_macro;
/// # extern crate syn;
/// #
/// use proc_macro::TokenStream;
/// use syn::parse_macro_input;
/// use syn::next::parse::{Parse, ParseStream, Result};
///
/// struct MyMacroInput {
///     /* ... */
/// }
///
/// impl Parse for MyMacroInput {
///     fn parse(input: ParseStream) -> Result<Self> {
///         /* ... */
/// #       Ok(MyMacroInput {})
///     }
/// }
///
/// # const IGNORE: &str = stringify! {
/// #[proc_macro]
/// # };
/// pub fn my_macro(tokens: TokenStream) -> TokenStream {
///     let input = parse_macro_input!(tokens as MyMacroInput);
///
///     /* ... */
/// #   "".parse().unwrap()
/// }
/// #
/// # fn main() {}
/// ```
#[cfg(feature = "proc-macro")]
#[macro_export]
macro_rules! parse_macro_input {
    ($tokenstream:ident as $ty:ty) => {
        match $crate::next::parse::<$ty>($tokenstream) {
            $crate::export::Ok(data) => data,
            $crate::export::Err(err) => {
                return $crate::export::TokenStream::from(err.into_compile_error());
            }
        };
    };
}
