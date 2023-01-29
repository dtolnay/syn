pub use proc_macro2::Ident;

#[cfg(not(doc))] // rustdoc bug: https://github.com/rust-lang/rust/issues/105735
#[cfg(feature = "parsing")]
#[doc(hidden)]
#[allow(non_upper_case_globals)]
pub const Ident: parsing::PeekIdent = parsing::PeekIdent {};

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
        Ident::new("_", token.span)
    }
}

pub fn xid_ok(symbol: &str) -> bool {
    let mut chars = symbol.chars();
    let first = chars.next().unwrap();
    if !(first == '_' || unicode_ident::is_xid_start(first)) {
        return false;
    }
    for ch in chars {
        if !unicode_ident::is_xid_continue(ch) {
            return false;
        }
    }
    true
}

#[cfg(feature = "parsing")]
mod parsing {
    use crate::buffer::Cursor;
    use crate::lookahead::{Either, Peek};
    use crate::parse::{Parse, ParseStream, Result};
    use crate::token::Token;
    use proc_macro2::Ident;
    use std::ops::BitOr;

    fn accept_as_ident(ident: &Ident) -> bool {
        match ident.to_string().as_str() {
            "_" |
            // Based on https://doc.rust-lang.org/1.65.0/reference/keywords.html
            "abstract" | "as" | "async" | "await" | "become" | "box" | "break" |
            "const" | "continue" | "crate" | "do" | "dyn" | "else" | "enum" |
            "extern" | "false" | "final" | "fn" | "for" | "if" | "impl" | "in" |
            "let" | "loop" | "macro" | "match" | "mod" | "move" | "mut" |
            "override" | "priv" | "pub" | "ref" | "return" | "Self" | "self" |
            "static" | "struct" | "super" | "trait" | "true" | "try" | "type" |
            "typeof" | "unsafe" | "unsized" | "use" | "virtual" | "where" |
            "while" | "yield" => false,
            _ => true,
        }
    }

    #[cfg_attr(doc_cfg, doc(cfg(feature = "parsing")))]
    impl Parse for Ident {
        fn parse(input: ParseStream) -> Result<Self> {
            input.step(|cursor| {
                if let Some((ident, rest)) = cursor.ident() {
                    if accept_as_ident(&ident) {
                        return Ok((ident, rest));
                    }
                }
                Err(cursor.error("expected identifier"))
            })
        }
    }

    impl Token for Ident {
        fn peek(cursor: Cursor) -> bool {
            if let Some((ident, _rest)) = cursor.ident() {
                accept_as_ident(&ident)
            } else {
                false
            }
        }

        fn display() -> &'static str {
            "identifier"
        }
    }

    #[derive(Copy, Clone)]
    pub struct PeekIdent {}

    impl Peek for PeekIdent {
        type Token = proc_macro2::Ident;
        fn peek(cursor: Cursor) -> bool {
            proc_macro2::Ident::peek(cursor)
        }
        fn display(f: &mut dyn FnMut(&'static str)) {
            f(proc_macro2::Ident::display());
        }
    }

    impl<U: Peek> BitOr<U> for PeekIdent {
        type Output = Either<Self, U>;
        fn bitor(self, _other: U) -> Self::Output {
            Either::new()
        }
    }
}
