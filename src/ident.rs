#[cfg(feature = "parsing")]
use lookahead;
#[cfg(feature = "parsing")]
use parse::{Parse, ParseStream, Result};

pub use proc_macro2::Ident;

#[cfg(feature = "parsing")]
#[doc(hidden)]
#[allow(non_snake_case)]
pub fn Ident(marker: lookahead::TokenMarker) -> Ident {
    match marker {}
}

#[cfg(feature = "parsing")]
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

macro_rules! ident_from_token {
    ($token:ident) => {
        impl From<Token![$token]> for Ident {
            fn from(token: Token![$token]) -> Ident {
                Ident::new(stringify!($token), token.span)
            }
        }
    };
}

ident_from_token!(self);
ident_from_token!(Self);
ident_from_token!(super);
ident_from_token!(crate);
ident_from_token!(extern);

impl From<Token![_]> for Ident {
    fn from(token: Token![_]) -> Ident {
        Ident::new("_", token.spans[0])
    }
}
