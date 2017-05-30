use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt::{self, Display};
use std::hash::{Hash, Hasher};

use proc_macro2::Symbol;

use Span;

#[derive(Clone)]
pub struct Ident {
    pub sym: Symbol,
    pub span: Span,
}

impl Ident {
    pub fn new(sym: Symbol, span: Span) -> Self {
        Ident {
            sym: sym,
            span: span,
        }
    }
}

impl<'a> From<&'a str> for Ident {
    fn from(s: &str) -> Self {
        Ident::new(s.into(), Span::default())
    }
}

impl<'a> From<Cow<'a, str>> for Ident {
    fn from(s: Cow<'a, str>) -> Self {
        Ident::new(s[..].into(), Span::default())
    }
}

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Ident::new(s[..].into(), Span::default())
    }
}

impl From<usize> for Ident {
    fn from(u: usize) -> Self {
        Ident::new(u.to_string()[..].into(), Span::default())
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self.sym.as_str()
    }
}

impl Display for Ident {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        self.sym.as_str().fmt(formatter)
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self.sym.as_str(), formatter)
    }
}

impl<T: ?Sized> PartialEq<T> for Ident
    where T: AsRef<str>
{
    fn eq(&self, other: &T) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl Eq for Ident {}

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Ident) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Ident {
    fn cmp(&self, other: &Ident) -> Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.as_ref().hash(h)
    }
}

#[cfg(feature = "parsing")]
pub mod parsing {
    use super::*;
    use synom::{TokenTree, TokenKind, IResult};
    #[cfg(feature = "full")]
    use lit::parsing::int;

    pub fn ident(input: &[TokenTree]) -> IResult<&[TokenTree], Ident> {
        if let IResult::Done(rest, id) = word(input) {
            match id.as_ref() {
                // From https://doc.rust-lang.org/grammar.html#keywords
                "abstract" | "alignof" | "as" | "become" | "box" | "break" | "const" | "continue" |
                "crate" | "do" | "else" | "enum" | "extern" | "false" | "final" | "fn" | "for" |
                "if" | "impl" | "in" | "let" | "loop" | "macro" | "match" | "mod" | "move" |
                "mut" | "offsetof" | "override" | "priv" | "proc" | "pub" | "pure" | "ref" |
                "return" | "Self" | "self" | "sizeof" | "static" | "struct" | "super" | "trait" |
                "true" | "type" | "typeof" | "unsafe" | "unsized" | "use" | "virtual" | "where" |
                "while" | "yield" => IResult::Error,
                _ => IResult::Done(rest, id),
            }
        } else {
            IResult::Error
        }
    }

    pub fn word(input: &[TokenTree]) -> IResult<&[TokenTree], Ident> {
        if let Some(&TokenTree { kind: TokenKind::Word(ref id), .. }) = input.first() {
            // Check if this word is _actually_ a lifetime, and treat that differently
            if id.chars().next().unwrap() == '\'' {
                IResult::Error
            } else {
                IResult::Done(&input[1..], Ident(id.to_string()))
            }
        } else {
            IResult::Error
        }
    }

    #[cfg(feature = "full")]
    named!(pub wordlike -> Ident, alt!(
        word
        |
        int => { |d| format!("{}", d).into() }
    ));
}

#[cfg(feature = "printing")]
mod printing {
    use super::*;
    use quote::{Tokens, ToTokens};
    use proc_macro2::{TokenTree, TokenKind};

    impl ToTokens for Ident {
        fn to_tokens(&self, tokens: &mut Tokens) {
            tokens.append(TokenTree {
                span: self.span.0,
                kind: TokenKind::Word(self.sym),
            })
        }
    }
}
