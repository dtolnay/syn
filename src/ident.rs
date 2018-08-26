#[cfg(feature = "parsing")]
use lookahead;

pub use proc_macro2::Ident;

#[cfg(feature = "parsing")]
#[doc(hidden)]
#[allow(non_snake_case)]
pub fn Ident(marker: lookahead::TokenMarker) -> Ident {
    match marker {}
}
