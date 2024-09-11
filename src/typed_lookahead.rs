#[doc(hidden)]
#[allow(non_snake_case)]
pub fn Ident(marker: lookahead::TokenMarker) -> Ident {
    match marker {}
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn Lifetime(marker: lookahead::TokenMarker) -> Lifetime {
    match marker {}
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn Lit(marker: lookahead::TokenMarker) -> Lit {
    match marker {}
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn LitBool(marker: lookahead::TokenMarker) -> LitBool {
    match marker {}
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn LitByte(marker: lookahead::TokenMarker) -> LitByte {
    match marker {}
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn LitByteStr(marker: lookahead::TokenMarker) -> LitByteStr {
    match marker {}
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn LitCStr(marker: lookahead::TokenMarker) -> LitCStr {
    match marker {}
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn LitChar(marker: lookahead::TokenMarker) -> LitChar {
    match marker {}
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn LitFloat(marker: lookahead::TokenMarker) -> LitFloat {
    match marker {}
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn LitInt(marker: lookahead::TokenMarker) -> LitInt {
    match marker {}
}

#[doc(hidden)]
#[allow(non_snake_case)]
pub fn LitStr(marker: lookahead::TokenMarker) -> LitStr {
    match marker {}
}

mod typed_lookahead {
    // Value namespace.
    pub(crate) use crate::*;

    // Shadow the type namespace.
    #[allow(unused_imports)]
    use crate::ident::syntax_tree::Ident;
    #[allow(unused_imports)]
    use crate::lifetime::syntax_tree::Lifetime;
    #[allow(unused_imports)]
    use crate::lit::syntax_tree::{
        Lit, LitBool, LitByte, LitByteStr, LitCStr, LitChar, LitFloat, LitInt, LitStr,
    };
}
